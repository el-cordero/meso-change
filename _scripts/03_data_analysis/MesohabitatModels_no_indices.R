# ==============================================
# Mesohabitat classification (ALL labels present in RAT)
# LOO across 7 dates + final model on all dates
# OPTIMIZED + CORRECTED VERSION
#
# Key fixes:
# - RF cap is actually used
# - class weights are computed from rf_train, not full train_df
# - cache filenames include aggregation factor
# - fixed typo in rf_trees argument
# - fixed semi_model feature_names reference
# - no NDVI/NDWI/NBR (4 Planet bands only)
# ==============================================

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tidymodels)
  library(yardstick)
  library(glue)
  library(readr)
  library(parallel)
})

set.seed(42)
VERBOSE <- TRUE

log_step <- function(msg, ..., .envir = parent.frame()) {
  if (VERBOSE) message(glue::glue(msg, ..., .envir = .envir))
}

# ------------------------------
# Global options
# ------------------------------
N_THREADS <- 8
terraOptions(progress = 1)
log_step("Using {N_THREADS} CPU threads for ranger where possible.")

# ------------------------------
# Paths / outputs
# ------------------------------
tables_out_dir   <- "_data/Tables/meso_classification"
pred_out_dir     <- "_data/GIS/Raster/Outputs/meso_predictions"
models_out_dir   <- "_data/Models/meso_models"

cache_dir        <- "_data/Cache/meso_classification"
aligned_dir      <- file.path(cache_dir, "aligned_features")
xy_cache_dir     <- file.path(cache_dir, "xy_tables")

dir.create(tables_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pred_out_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(models_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(aligned_dir,    recursive = TRUE, showWarnings = FALSE)
dir.create(xy_cache_dir,   recursive = TRUE, showWarnings = FALSE)

# ------------------------------
# Load rasters
# ------------------------------
log_step("Loading rasters...")
meso_gt <- rast("_data/GIS/Raster/Clean/mesohabitats.tif", lyrs = 1:7)
planet  <- rast("_data/GIS/Raster/Clean/planet_7dates.tif")

log_step("Loaded meso_gt layers: {paste(names(meso_gt), collapse=', ')}")
log_step("Loaded planet bands: {length(names(planet))} bands; sample: {paste(head(names(planet), 6), collapse=', ')}")

# ------------------------------
# Resolution / aggregation
# ------------------------------
log_step("Current res | meso={paste(terra::res(meso_gt), collapse=' x ')} | planet={paste(terra::res(planet), collapse=' x ')}")

fact <- 1

if (fact > 1) {
  meso_gt <- terra::aggregate(meso_gt, fact = fact, fun = "modal", na.rm = TRUE)
  planet  <- terra::aggregate(planet,  fact = fact, fun = "mean",  na.rm = TRUE)
}

log_step("New res     | meso={paste(terra::res(meso_gt), collapse=' x ')} | planet={paste(terra::res(planet), collapse=' x ')}")

# ------------------------------
# Map meso layer names -> Planet prefixes
# ------------------------------
map_tbl <- tribble(
  ~meso_layer,        ~planet_prefix,
  "01_Apr17_2022",    "03_April17_2022",
  "02_Dec22_2021",    "01_Dec22_2021",
  "03_Feb12_2022",    "02_Feb12_2022",
  "04_July18_2022",   "06_July18_2022",
  "05_Jun20_2022",    "05_June20_2022",
  "06_May31_2022",    "04_May31_2022",
  "07_Sep28_2022",    "07_Sep28_2022"
)

log_step("Date mapping:\n{paste(apply(map_tbl, 1, function(r) glue('  {r[[1]]} -> {r[[2]]}')), collapse='\n')}")

# ------------------------------
# Class labels from raster RAT
# ------------------------------
lvl_df  <- levels(meso_gt)[[1]]
lbl_col <- setdiff(names(lvl_df), c("value"))[1]
lvl_df  <- arrange(lvl_df, value)

codes  <- as.integer(lvl_df$value)
labels <- trimws(as.character(lvl_df[[lbl_col]]))

label_to_code <- setNames(codes, labels)
code_to_label <- setNames(labels, codes)

cat_tbl <- data.frame(value = codes, label = labels, stringsAsFactors = FALSE)

log_step("Detected {length(labels)} classes: {paste(labels, collapse=', ')}")

# ------------------------------
# Helpers
# ------------------------------
planet_for_prefix <- function(pref) {
  nm <- paste0(pref, "_", c("blue", "green", "red", "nir"))
  if (!all(nm %in% names(planet))) {
    stop("Planet bands not found for prefix: ", pref, "\nExpected: ", paste(nm, collapse = ", "))
  }
  out <- planet[[nm]]
  names(out) <- c("blue", "green", "red", "nir")
  out
}

aligned_feat_path <- function(meso_layer) {
  file.path(aligned_dir, glue("features_fact{fact}_{meso_layer}.tif"))
}

xy_cache_path <- function(meso_layer) {
  file.path(xy_cache_dir, glue("xy_fact{fact}_{meso_layer}.rds"))
}

build_aligned_feature_raster <- function(meso_layer, planet_prefix, overwrite = FALSE) {
  out_path <- aligned_feat_path(meso_layer)
  
  if (file.exists(out_path) && !overwrite) {
    log_step("Using cached aligned feature raster for {meso_layer}")
    return(rast(out_path))
  }
  
  log_step("Building aligned feature raster for {meso_layer} ...")
  meso_r <- meso_gt[[meso_layer]]
  r4 <- planet_for_prefix(planet_prefix)
  
  if (!compareGeom(r4, meso_r, stopOnError = FALSE)) {
    log_step("  Reprojecting/resampling Planet -> meso grid for {planet_prefix}")
    r4 <- project(r4, meso_r, method = "bilinear")
    r4 <- resample(r4, meso_r, method = "bilinear")
  }
  
  r4 <- mask(r4, meso_r)
  names(r4) <- c("blue", "green", "red", "nir")
  
  writeRaster(
    r4,
    filename = out_path,
    overwrite = TRUE,
    wopt = list(gdal = "COMPRESS=LZW")
  )
  
  rast(out_path)
}

build_xy_for_date_cached <- function(meso_layer, planet_prefix, overwrite = FALSE) {
  out_path <- xy_cache_path(meso_layer)
  
  if (file.exists(out_path) && !overwrite) {
    log_step("Using cached XY table for {meso_layer}")
    return(readRDS(out_path))
  }
  
  log_step("Building XY table for {meso_layer} ...")
  meso_r <- meso_gt[[meso_layer]]
  feat_r <- build_aligned_feature_raster(meso_layer, planet_prefix, overwrite = overwrite)
  
  X_vals    <- values(feat_r, mat = TRUE)
  meso_vals <- values(meso_r, mat = TRUE)
  
  if (!is.numeric(meso_vals)) {
    meso_vals <- label_to_code[as.character(meso_vals)]
  }
  
  df <- as.data.frame(X_vals)
  names(df) <- c("blue", "green", "red", "nir")
  df$meso_code <- as.integer(meso_vals)
  
  keep <- is.finite(df$blue) &
    is.finite(df$green) &
    is.finite(df$red) &
    is.finite(df$nir) &
    !is.na(df$meso_code) &
    df$meso_code %in% codes
  
  df <- df[keep, , drop = FALSE]
  df$meso <- factor(df$meso_code, levels = codes, labels = labels)
  df$meso_code <- NULL
  
  saveRDS(df, out_path, compress = "gzip")
  log_step("  Saved XY table for {meso_layer}: {nrow(df)} rows")
  
  df
}

build_feat_rast_for_date_cached <- function(meso_layer, planet_prefix) {
  build_aligned_feature_raster(meso_layer, planet_prefix, overwrite = FALSE)
}

per_class_metrics <- function(cm) {
  tbl <- cm$table
  classes <- rownames(tbl)
  total <- sum(tbl)
  
  out <- lapply(classes, function(cl) {
    tp <- tbl[cl, cl]
    fn <- sum(tbl[cl, ]) - tp
    fp <- sum(tbl[, cl]) - tp
    
    prec_den <- tp + fp
    rec_den  <- tp + fn
    
    precision <- if (prec_den > 0) tp / prec_den else NA_real_
    recall    <- if (rec_den  > 0) tp / rec_den  else NA_real_
    f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else {
      NA_real_
    }
    
    data.frame(
      .level = cl,
      precision = precision,
      recall = recall,
      f_meas = f1,
      support = sum(tbl[cl, ]),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out)
}

clean_numeric_rows <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols)) {
    df <- df %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(num_cols), ~ is.finite(.)))
  }
  tidyr::drop_na(df)
}

# ------------------------------
# Precompute/cache all per-date aligned rasters and XY tables once
# ------------------------------
log_step("Precomputing cached aligned rasters and XY tables for all dates...")
purrr::walk2(
  map_tbl$meso_layer,
  map_tbl$planet_prefix,
  ~{
    build_aligned_feature_raster(.x, .y, overwrite = FALSE)
    build_xy_for_date_cached(.x, .y, overwrite = FALSE)
  }
)
log_step("Caching complete.")

# ------------------------------
# Core one-run function (train on N dates, test on 1)
# ------------------------------
run_one <- function(train_ids, test_id,
                    k_clusters = 12,
                    use_class_weights = TRUE,
                    write_fold_rasters = FALSE,
                    km_n_max = 500000,
                    rf_cap_per_class = 25000,
                    rf_trees = 300) {
  
  log_step("====== Fold START | Test={test_id} | Train={paste(train_ids, collapse=', ')} ======")
  
  train_map <- map_tbl %>% dplyr::filter(meso_layer %in% train_ids)
  test_map  <- map_tbl %>% dplyr::filter(meso_layer == test_id)
  
  t0 <- Sys.time()
  
  train_df_list <- purrr::map2(train_map$meso_layer, train_map$planet_prefix, build_xy_for_date_cached)
  train_df <- dplyr::bind_rows(train_df_list, .id = "src") %>%
    dplyr::select(-src) %>%
    clean_numeric_rows()
  
  test_df <- build_xy_for_date_cached(test_map$meso_layer, test_map$planet_prefix) %>%
    clean_numeric_rows()
  
  log_step("Built train_df rows={nrow(train_df)}; test_df rows={nrow(test_df)} in {round(difftime(Sys.time(), t0, units='secs'),1)} s")
  
  train_df$meso <- droplevels(train_df$meso)
  test_df$meso  <- factor(test_df$meso, levels = levels(train_df$meso))
  
  missing_in_train <- setdiff(labels, levels(train_df$meso))
  if (length(missing_in_train)) {
    log_step("WARNING: Classes absent in training this fold: {paste(missing_in_train, collapse=', ')}")
  }
  
  log_step("Training classes present: {paste(levels(train_df$meso), collapse=', ')}")
  log_step("Train class counts:\n{paste(capture.output(print(table(train_df$meso))), collapse='\n')}")
  
  if (nrow(train_df) < 100 || dplyr::n_distinct(train_df$meso) < 2 ||
      nrow(test_df)  < 100 || dplyr::n_distinct(test_df$meso)  < 2) {
    log_step("!! Insufficient data/classes for fold; skipping metrics.")
    return(dplyr::bind_rows(
      tibble::tibble(test_id = test_id, model = "SemiSupervised", .metric = c("accuracy","bal_accuracy","macro_f1"), .estimate = NA_real_),
      tibble::tibble(test_id = test_id, model = "SupervisedRF",   .metric = c("accuracy","bal_accuracy","macro_f1"), .estimate = NA_real_)
    ))
  }
  
  all_lvls <- levels(train_df$meso)
  test_df$meso <- factor(test_df$meso, levels = all_lvls)
  
  # ---------- SEMI-SUPERVISED (K-means + majority) ----------
  log_step("K-means: k={k_clusters} ...")
  X_train_full <- train_df %>% dplyr::select(-meso) %>% as.matrix()
  
  if (!is.null(km_n_max) && nrow(X_train_full) > km_n_max) {
    set.seed(42)
    km_idx <- sample.int(nrow(X_train_full), km_n_max)
    X_train_km <- X_train_full[km_idx, , drop = FALSE]
    y_train_km <- train_df$meso[km_idx]
    log_step("  K-means using sampled rows: {nrow(X_train_km)}/{nrow(X_train_full)}")
  } else {
    X_train_km <- X_train_full
    y_train_km <- train_df$meso
  }
  
  t_km <- Sys.time()
  train_scaled <- scale(X_train_km)
  train_center <- attr(train_scaled, "scaled:center")
  train_scale  <- attr(train_scaled, "scaled:scale")
  train_scale[train_scale == 0 | is.na(train_scale)] <- 1
  
  km_fit <- kmeans(
    train_scaled,
    centers = k_clusters,
    nstart = 50,
    iter.max = 500,
    algorithm = "Hartigan-Wong"
  )
  
  log_step("  K-means done in {round(difftime(Sys.time(), t_km, units='secs'),1)} s; tot.withinss={round(km_fit$tot.withinss,2)}")
  
  clusters <- km_fit$cluster
  cluster_labels <- tibble::tibble(cluster = clusters, meso = y_train_km) %>%
    dplyr::count(cluster, meso, name = "n") %>%
    dplyr::group_by(cluster) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(cluster, meso)
  
  log_step("  Cluster→label map (head):\n{paste(capture.output(print(head(cluster_labels, 10))), collapse='\n')}")
  
  X_test_mat <- test_df %>% dplyr::select(-meso) %>% as.matrix()
  test_scaled <- scale(X_test_mat, center = train_center, scale = train_scale)
  
  rsx <- rowSums(test_scaled^2)
  rsc <- rowSums(km_fit$centers^2)
  dist2 <- outer(rsx, rsc, "+") - 2 * (test_scaled %*% t(km_fit$centers))
  test_cl <- max.col(-dist2, ties.method = "first")
  
  cl_to_lbl <- setNames(as.character(cluster_labels$meso), cluster_labels$cluster)
  semi_pred_lbl <- cl_to_lbl[as.character(test_cl)]
  
  semi_eval_df <- tibble::tibble(
    obs  = test_df$meso,
    pred = factor(semi_pred_lbl, levels = all_lvls)
  ) %>% tidyr::drop_na()
  
  semi_cm  <- yardstick::conf_mat(semi_eval_df, truth = obs, estimate = pred)
  semi_acc <- yardstick::accuracy(semi_eval_df, obs, pred)$.estimate
  
  semi_pc  <- per_class_metrics(semi_cm)
  semi_bal <- mean(semi_pc$recall, na.rm = TRUE)
  semi_f1m <- mean(semi_pc$f_meas, na.rm = TRUE)
  
  log_step("Semi-supervised metrics: Acc={round(semi_acc,4)} | BalAcc={round(semi_bal,4)} | MacroF1={round(semi_f1m,4)}")
  
  # ---------- RF training data cap ----------
  if (!is.null(rf_cap_per_class)) {
    set.seed(42)
    rf_train <- train_df %>%
      dplyr::group_by(meso) %>%
      dplyr::group_modify(~ {
        if (nrow(.x) <= rf_cap_per_class) {
          .x
        } else {
          .x[sample.int(nrow(.x), rf_cap_per_class), , drop = FALSE]
        }
      }) %>%
      dplyr::ungroup()
    log_step("RF training rows after per-class cap: {nrow(rf_train)}")
  } else {
    rf_train <- train_df
  }
  
  # ---------- SUPERVISED RF ----------
  log_step("RF: training (class weights={use_class_weights}) with {N_THREADS} threads...")
  
  cw_vec <- NULL
  if (use_class_weights) {
    cw_tbl <- rf_train %>% count(meso) %>% mutate(w = 1 / n)
    cw_tbl$w <- cw_tbl$w / sum(cw_tbl$w)
    cw_vec <- setNames(cw_tbl$w, as.character(cw_tbl$meso))
    log_step("Class weights (rf_train): {paste(sprintf('%s=%.4f', names(cw_vec), cw_vec), collapse=', ')}")
  }
  
  rec <- recipes::recipe(meso ~ ., data = rf_train) %>%
    recipes::step_zv(recipes::all_predictors())
  
  rf_spec <- parsnip::rand_forest(
    trees = rf_trees,
    mtry  = min(4, ncol(rf_train) - 1),
    min_n = 5
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine(
      "ranger",
      importance = "impurity",
      class.weights = cw_vec,
      num.threads = N_THREADS
    )
  
  wf <- workflows::workflow() %>%
    workflows::add_model(rf_spec) %>%
    workflows::add_recipe(rec)
  
  log_step("RF training rows: {nrow(rf_train)}")
  
  t_rf <- Sys.time()
  rf_fit <- parsnip::fit(wf, rf_train)
  log_step("  RF trained in {round(difftime(Sys.time(), t_rf, units='secs'),1)} s")
  
  prep_rec   <- recipes::prep(rec)
  test_baked <- recipes::bake(prep_rec, new_data = test_df %>% dplyr::select(-meso))
  rf_pred    <- predict(rf_fit, new_data = test_baked, type = "class")$.pred_class
  
  rf_eval_df <- tibble::tibble(
    obs  = test_df$meso,
    pred = factor(rf_pred, levels = all_lvls)
  ) %>% tidyr::drop_na()
  
  rf_cm  <- yardstick::conf_mat(rf_eval_df, truth = obs, estimate = pred)
  rf_acc <- yardstick::accuracy(rf_eval_df, obs, pred)$.estimate
  
  rf_pc  <- per_class_metrics(rf_cm)
  rf_bal <- mean(rf_pc$recall, na.rm = TRUE)
  rf_f1m <- mean(rf_pc$f_meas, na.rm = TRUE)
  
  log_step("RF metrics: Acc={round(rf_acc,4)} | BalAcc={round(rf_bal,4)} | MacroF1={round(rf_f1m,4)}")
  
  # ---------- WRITE PREDICTION RASTERS ----------
  if (isTRUE(write_fold_rasters)) {
    log_step("Writing prediction rasters for {test_id}...")
    
    test_feat_rast <- build_feat_rast_for_date_cached(test_map$meso_layer, test_map$planet_prefix)
    vals_all <- values(test_feat_rast, mat = TRUE)
    keep_all <- apply(is.finite(vals_all), 1, all)
    colnames(vals_all) <- c("blue", "green", "red", "nir")
    
    # Semi
    X_all_scaled <- scale(vals_all, center = train_center, scale = train_scale)
    rsx_all <- rowSums(X_all_scaled^2)
    rsc     <- rowSums(km_fit$centers^2)
    dist2_all <- outer(rsx_all, rsc, "+") - 2 * (X_all_scaled %*% t(km_fit$centers))
    cl_all <- max.col(-dist2_all, ties.method = "first")
    semi_pred_lbl_all <- cl_to_lbl[as.character(cl_all)]
    
    semi_codes <- rep(NA_integer_, nrow(vals_all))
    semi_codes[keep_all] <- label_to_code[as.character(semi_pred_lbl_all[keep_all])]
    
    semi_r <- test_feat_rast[[1]]
    values(semi_r) <- semi_codes
    semi_r <- as.factor(semi_r)
    levels(semi_r) <- cat_tbl
    
    f_semi <- file.path(pred_out_dir, glue("semi_pred_{test_id}.tif"))
    writeRaster(
      semi_r,
      filename = f_semi,
      overwrite = TRUE,
      wopt = list(datatype = "INT2U", gdal = "COMPRESS=LZW")
    )
    
    # RF
    df_all_raw <- as.data.frame(vals_all[keep_all, , drop = FALSE])
    df_all_baked <- recipes::bake(prep_rec, new_data = df_all_raw)
    rf_pred_all <- predict(rf_fit, new_data = df_all_baked, type = "class")$.pred_class
    
    rf_codes <- rep(NA_integer_, nrow(vals_all))
    rf_codes[keep_all] <- unname(label_to_code[as.character(rf_pred_all)])
    
    rf_r <- test_feat_rast[[1]]
    values(rf_r) <- rf_codes
    rf_r <- as.factor(rf_r)
    levels(rf_r) <- cat_tbl
    
    f_rf <- file.path(pred_out_dir, glue("rf_pred_{test_id}.tif"))
    writeRaster(
      rf_r,
      filename = f_rf,
      overwrite = TRUE,
      wopt = list(datatype = "INT2U", gdal = "COMPRESS=LZW")
    )
  }
  
  # ---------- SAVE MODELS ----------
  semi_model <- list(
    kmeans         = km_fit,
    center         = train_center,
    scale          = train_scale,
    cluster_labels = cluster_labels,
    feature_names  = colnames(X_train_km),
    class_levels   = all_lvls,
    train_ids      = train_ids,
    test_id        = test_id,
    k              = k_clusters
  )
  
  f_semi_m <- file.path(models_out_dir, glue("semi_kmeans_{test_id}.rds"))
  saveRDS(semi_model, f_semi_m, compress = "gzip")
  
  f_rf_m <- file.path(models_out_dir, glue("rf_workflow_{test_id}.rds"))
  saveRDS(rf_fit, f_rf_m, compress = "gzip")
  
  log_step("  Saved models:\n    {f_semi_m}\n    {f_rf_m}")
  log_step("====== Fold END | Test={test_id} ======")
  
  dplyr::bind_rows(
    tibble::tibble(test_id = test_id, model = "SemiSupervised", .metric = "accuracy",     .estimate = semi_acc),
    tibble::tibble(test_id = test_id, model = "SemiSupervised", .metric = "bal_accuracy", .estimate = semi_bal),
    tibble::tibble(test_id = test_id, model = "SemiSupervised", .metric = "macro_f1",     .estimate = semi_f1m),
    
    tibble::tibble(test_id = test_id, model = "SupervisedRF",   .metric = "accuracy",     .estimate = rf_acc),
    tibble::tibble(test_id = test_id, model = "SupervisedRF",   .metric = "bal_accuracy", .estimate = rf_bal),
    tibble::tibble(test_id = test_id, model = "SupervisedRF",   .metric = "macro_f1",     .estimate = rf_f1m)
  )
}

# ------------------------------
# LOO across all 7 dates
# ------------------------------
all_dates <- names(meso_gt)
log_step("Starting LOO across {length(all_dates)} dates...")

loo <- purrr::map_dfr(seq_along(all_dates), function(i) {
  td <- all_dates[i]
  log_step("=== LOO fold {i}/{length(all_dates)} | Test={td} ===")
  
  train_set <- setdiff(all_dates, td)[1:6]
  
  run_one(
    train_ids = train_set,
    test_id = td,
    k_clusters = 12,
    use_class_weights = TRUE,
    write_fold_rasters = FALSE,
    km_n_max = 500000,
    rf_cap_per_class = 25000,
    rf_trees = 300
  )
})

loo_summary <- loo %>%
  dplyr::group_by(model, .metric) %>%
  dplyr::summarize(
    mean = mean(.estimate, na.rm = TRUE),
    sd   = stats::sd(.estimate, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(loo,         file.path(tables_out_dir, "loo_fold_metrics.csv"))
readr::write_csv(loo_summary, file.path(tables_out_dir, "loo_summary.csv"))

log_step("LOO summary:\n{paste(capture.output(print(loo_summary)), collapse='\n')}")
log_step("Saved LOO tables to: {normalizePath(tables_out_dir)}")

# ==============================================
# FINAL MODEL on ALL 7 DATES
# ==============================================
log_step("Training FINAL models on all dates...")

full_df_list <- purrr::map2(map_tbl$meso_layer, map_tbl$planet_prefix, build_xy_for_date_cached)
full_df <- dplyr::bind_rows(full_df_list, .id = "src") %>%
  dplyr::select(-src) %>%
  clean_numeric_rows()

log_step("Full training rows={nrow(full_df)}; class counts -> {paste(capture.output(print(table(full_df$meso))), collapse=' ')}")

full_df$meso <- droplevels(full_df$meso)
missing_in_final <- setdiff(labels, levels(full_df$meso))
if (length(missing_in_final)) {
  log_step("WARNING (FINAL): Classes absent in training: {paste(missing_in_final, collapse=', ')}")
}

log_step("FINAL classes present: {paste(levels(full_df$meso), collapse=', ')}")
log_step("FINAL class counts:\n{paste(capture.output(print(table(full_df$meso))), collapse='\n')}")

all_lvls_final <- levels(full_df$meso)

# --- FINAL K-means ---
X_full <- full_df %>% dplyr::select(-meso) %>% as.matrix()

km_n_max_final <- 500000
if (!is.null(km_n_max_final) && nrow(X_full) > km_n_max_final) {
  set.seed(42)
  km_idx_final <- sample.int(nrow(X_full), km_n_max_final)
  X_final_km <- X_full[km_idx_final, , drop = FALSE]
  y_final_km <- full_df$meso[km_idx_final]
  log_step("Final K-means using sampled rows: {nrow(X_final_km)}/{nrow(X_full)}")
} else {
  X_final_km <- X_full
  y_final_km <- full_df$meso
}

t_km_final <- Sys.time()
train_scaled <- scale(X_final_km)
train_center <- attr(train_scaled, "scaled:center")
train_scale  <- attr(train_scaled, "scaled:scale")
train_scale[train_scale == 0 | is.na(train_scale)] <- 1

k_clusters_final <- 12
km_fit_final <- kmeans(
  train_scaled,
  centers = k_clusters_final,
  nstart = 50,
  iter.max = 500,
  algorithm = "Hartigan-Wong"
)

log_step("Final K-means done in {round(difftime(Sys.time(), t_km_final, units='secs'),1)} s; tot.withinss={round(km_fit_final$tot.withinss,2)}")

cluster_labels_final <- tibble::tibble(cluster = km_fit_final$cluster, meso = y_final_km) %>%
  dplyr::count(cluster, meso, name = "n") %>%
  dplyr::group_by(cluster) %>%
  dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::select(cluster, meso)

semi_model_final <- list(
  kmeans         = km_fit_final,
  center         = train_center,
  scale          = train_scale,
  cluster_labels = cluster_labels_final,
  feature_names  = colnames(X_final_km),
  class_levels   = all_lvls_final,
  train_ids      = all_dates,
  test_id        = "FINAL",
  k              = k_clusters_final
)

saveRDS(semi_model_final, file.path(models_out_dir, "semi_kmeans_FINAL.rds"), compress = "gzip")

# --- FINAL RF ---
rf_cap_per_class_final <- 25000
if (!is.null(rf_cap_per_class_final)) {
  set.seed(42)
  rf_train_final <- full_df %>%
    dplyr::group_by(meso) %>%
    dplyr::group_modify(~ {
      if (nrow(.x) <= rf_cap_per_class_final) {
        .x
      } else {
        .x[sample.int(nrow(.x), rf_cap_per_class_final), , drop = FALSE]
      }
    }) %>%
    dplyr::ungroup()
  log_step("Final RF training rows after per-class cap: {nrow(rf_train_final)}")
} else {
  rf_train_final <- full_df
}

cw_tbl <- rf_train_final %>% dplyr::count(meso) %>% dplyr::mutate(w = 1 / n)
cw_tbl$w <- cw_tbl$w / sum(cw_tbl$w)
cw_vec <- setNames(cw_tbl$w, as.character(cw_tbl$meso))
log_step("Final RF class weights: {paste(sprintf('%s=%.4f', names(cw_vec), cw_vec), collapse=', ')}")

rec_final <- recipes::recipe(meso ~ ., data = rf_train_final) %>%
  recipes::step_zv(recipes::all_predictors())

rf_spec_final <- parsnip::rand_forest(
  trees = 500,
  mtry  = min(4, ncol(rf_train_final) - 1),
  min_n = 5
) %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine(
    "ranger",
    importance = "impurity",
    class.weights = cw_vec,
    num.threads = N_THREADS
  )

wf_final <- workflows::workflow() %>%
  workflows::add_model(rf_spec_final) %>%
  workflows::add_recipe(rec_final)

t_rf_final <- Sys.time()
rf_fit_final <- parsnip::fit(wf_final, rf_train_final)
log_step("Final RF trained in {round(difftime(Sys.time(), t_rf_final, units='secs'),1)} s")

rec_final_prep <- recipes::prep(rec_final, training = rf_train_final, retain = TRUE)

saveRDS(rf_fit_final, file.path(models_out_dir, "rf_workflow_FINAL.rds"), compress = "gzip")
log_step("Saved FINAL models to: {normalizePath(models_out_dir)}")

# --- OPTIONAL: Write FINAL predictions for each date ---
write_final_preds <- TRUE

if (isTRUE(write_final_preds)) {
  log_step("Writing FINAL prediction rasters for each date...")
  
  purrr::walk2(map_tbl$meso_layer, map_tbl$planet_prefix, function(ml, pref) {
    feat_r <- build_feat_rast_for_date_cached(ml, pref)
    vals   <- values(feat_r, mat = TRUE)
    keep   <- apply(is.finite(vals), 1, all)
    colnames(vals) <- c("blue", "green", "red", "nir")
    
    # Semi FINAL
    X_all_scaled <- scale(vals, center = semi_model_final$center, scale = semi_model_final$scale)
    rsx_all <- rowSums(X_all_scaled^2)
    rsc     <- rowSums(semi_model_final$kmeans$centers^2)
    dist2   <- outer(rsx_all, rsc, "+") - 2 * (X_all_scaled %*% t(semi_model_final$kmeans$centers))
    cl      <- max.col(-dist2, ties.method = "first")
    
    cl2lbl  <- setNames(as.character(semi_model_final$cluster_labels$meso),
                        semi_model_final$cluster_labels$cluster)
    
    semi_lbl  <- cl2lbl[as.character(cl)]
    semi_code <- rep(NA_integer_, nrow(vals))
    semi_code[keep] <- label_to_code[as.character(semi_lbl[keep])]
    
    r <- feat_r[[1]]
    values(r) <- semi_code
    r <- as.factor(r)
    levels(r) <- cat_tbl
    
    f1 <- file.path(pred_out_dir, glue("semi_pred_FINAL_{ml}.tif"))
    writeRaster(
      r,
      f1,
      overwrite = TRUE,
      wopt = list(datatype = "INT2U", gdal = "COMPRESS=LZW")
    )
    
    # RF FINAL
    df_all_raw   <- as.data.frame(vals[keep, , drop = FALSE])
    df_all_baked <- recipes::bake(rec_final_prep, new_data = df_all_raw)
    
    rf_lbl <- rep(NA_character_, nrow(vals))
    rf_lbl[keep] <- predict(rf_fit_final, new_data = df_all_baked, type = "class")$.pred_class
    
    rf_code <- rep(NA_integer_, nrow(vals))
    rf_code[keep] <- unname(label_to_code[as.character(rf_lbl[keep])])
    
    r2 <- feat_r[[1]]
    values(r2) <- rf_code
    r2 <- as.factor(r2)
    levels(r2) <- cat_tbl
    
    f2 <- file.path(pred_out_dir, glue("rf_pred_FINAL_{ml}.tif"))
    writeRaster(
      r2,
      f2,
      overwrite = TRUE,
      wopt = list(datatype = "INT2U", gdal = "COMPRESS=LZW")
    )
  })
}

writeLines(capture.output(sessionInfo()), file.path(models_out_dir, "R_sessionInfo.txt"))
log_step("All done ✅")