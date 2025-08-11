# ===============================
# Mesohabitat classification (ALL labels 1..8)
# Uses your cleaned stacks:
#   - meso_gt: _data/GIS/Raster/Clean/mesohabitats.tif (7 layers, categorical 1..8)
#   - planet : _data/GIS/Raster/Clean/planet_7dates.tif (28 bands, 4 per date: blue/green/red/nir)
# Train on 2 dates, test on 1
# ===============================

library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(tidymodels)
library(yardstick)
library(glue)

set.seed(42)

# ------------------------------
# Load data
# ------------------------------
meso_gt <- rast("_data/GIS/Raster/Clean/mesohabitats.tif", lyrs = 1:7)
planet  <- rast("_data/GIS/Raster/Clean/planet_7dates.tif")

# ------------------------------
# Map meso layer names -> planet prefixes (matches your names)
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

# ------------------------------
# Build class labels from raster levels (use ALL labels present)
# Assumes all layers share the same codebook (as in your example)
# ------------------------------
lvl_df <- levels(meso_gt)[[1]] %>% arrange(value)
class_lbls <- setNames(lvl_df[[2]], lvl_df$value)  # value -> label (character)

# ------------------------------
# Helpers
# ------------------------------
planet_for_prefix <- function(pref) {
  nm <- paste0(pref, "_", c("blue","green","red","nir"))
  stopifnot(all(nm %in% names(planet)))
  planet[[nm]]
}

add_indices <- function(r4) {
  # order: blue, green, red, nir
  B <- r4[[1]]; G <- r4[[2]]; R <- r4[[3]]; NIR <- r4[[4]]
  safe_ratio <- function(num, den) {
    out <- (num - den) / (num + den)
    out[!is.finite(out)] <- NA
    out
  }
  ndvi <- safe_ratio(NIR, R)
  ndwi <- safe_ratio(G, NIR)
  nbr  <- safe_ratio(NIR, B)
  out <- c(r4, ndvi, ndwi, nbr)
  names(out) <- c("blue","green","red","nir","NDVI","NDWI","NBR")
  out
}


# --- replacement: build_xy_for_date that uses numeric codes safely ---
build_xy_for_date <- function(meso_r, planet_pref) {
  # 1) Get the Planet 4-band raster for the date and align to meso grid
  r4 <- planet_for_prefix(planet_pref)
  if (!compareGeom(r4, meso_r, stopOnError = FALSE)) {
    r4 <- project(r4, meso_r, method = "bilinear")
    r4 <- resample(r4, meso_r, method = "bilinear")
  }
  
  # 2) Add indices (safe ratios) and mask to meso extent so we don't drag in huge NA regions
  X <- add_indices(r4)
  X <- mask(X, meso_r)
  
  # 3) Pull raw cell values (predictors as matrix, meso as numeric codes)
  X_vals    <- values(X, mat = TRUE)             # numeric predictors + indices
  meso_vals <- values(meso_r, mat = TRUE)        # SHOULD be numeric codes (1..8)
  
  # If meso_vals came through as factors/characters for some reason, coerce to numeric codes
  if (!is.numeric(meso_vals)) {
    # Try to map labels back to codes using class_lbls (names = codes, values = labels)
    lbl_to_code <- setNames(as.integer(names(class_lbls)), unname(class_lbls))
    meso_vals <- lbl_to_code[as.character(meso_vals)]
  }
  
  # 4) Assemble data.frame
  df <- as.data.frame(X_vals)
  names(df) <- c("blue","green","red","nir","NDVI","NDWI","NBR")
  df$meso_code <- as.integer(meso_vals)
  
  # 5) Filter to valid rows: keep rows where meso_code is 1..8 and predictors are all finite
  keep <- is.finite(df$blue) & is.finite(df$green) & is.finite(df$red) & is.finite(df$nir) &
    is.finite(df$NDVI) & is.finite(df$NDWI) & is.finite(df$NBR) &
    !is.na(df$meso_code) & df$meso_code %in% as.integer(names(class_lbls))
  
  df <- df[keep, , drop = FALSE]
  
  # 6) Map numeric codes → labels (factor)
  df$meso <- factor(
    df$meso_code,
    levels = as.integer(names(class_lbls)),
    labels = unname(class_lbls)
  )
  
  # 7) Drop helper column
  df$meso_code <- NULL
  
  # Sanity print
  message("Rows after build_xy_for_date(", planet_pref, "): ", nrow(df))
  if (nrow(df) > 0) {
    message("Class counts:\n", paste(capture.output(print(table(df$meso))), collapse = "\n"))
  }
  
  df
}


# ------------------------------
# Choose train/test dates (edit here if desired)
# ------------------------------
train_ids <- c("01_Apr17_2022",  "02_Dec22_2021",  "03_Feb12_2022",  "04_July18_2022", 
               "05_Jun20_2022",  "06_May31_2022")
test_id   <- "07_Sep28_2022"


stopifnot(all(c(train_ids, test_id) %in% names(meso_gt)))

train_map <- map_tbl %>% filter(meso_layer %in% train_ids)
test_map  <- map_tbl %>% filter(meso_layer == test_id)

meso_train_list <- lapply(train_map$meso_layer, function(nm) meso_gt[[nm]])
meso_test       <- meso_gt[[test_map$meso_layer]]

# ------------------------------
# Build training & test data frames
# ------------------------------
train_df_list <- purrr::map2(meso_train_list, train_map$planet_prefix, ~build_xy_for_date(.x, .y))
train_df <- dplyr::bind_rows(train_df_list, .id = "src") %>% dplyr::select(-src)

test_df <- build_xy_for_date(meso_test, test_map$planet_prefix)

message("Train rows: ", nrow(train_df), " | Test rows: ", nrow(test_df))
message("Train class counts:\n", paste(capture.output(print(table(train_df$meso))), collapse="\n"))


# Ensure factor levels are consistent across splits
all_lvls <- levels(train_df$meso)
test_df$meso <- factor(test_df$meso, levels = all_lvls)

clean_numeric_rows <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  # remove non-finite values in predictors
  df <- df %>%
    filter(if_all(all_of(num_cols), ~ is.finite(.)))
  # drop any remaining NA rows
  df <- tidyr::drop_na(df)
  df
}

train_df <- clean_numeric_rows(train_df)
test_df  <- clean_numeric_rows(test_df)

# sanity checks
message("Train rows: ", nrow(train_df), " | Test rows: ", nrow(test_df))
message("Train class counts:\n", paste(capture.output(print(table(train_df$meso))), collapse="\n"))

# ------------------------------
# SEMI-SUPERVISED: K-means + majority label
# ------------------------------
X_train <- train_df %>% select(-meso) %>% drop_na()
train_mat <- as.matrix(X_train)

# scale training features and SAVE center/scale
train_scaled <- scale(train_mat)
train_center <- attr(train_scaled, "scaled:center")
train_scale  <- attr(train_scaled, "scaled:scale")
# guard against zero-variance columns
train_scale[train_scale == 0 | is.na(train_scale)] <- 1

# k_clusters <- length(all_lvls)  # 8 labels; tweak if you want
k_clusters <- 12  # gives clusters room for sub-modes across more dates
km_fit <- kmeans(train_scaled, centers = k_clusters, nstart = 10, iter.max = 300)

# attach clusters back to full training rows
clusters <- rep(NA_integer_, nrow(train_df))
ok <- complete.cases(train_df %>% select(-meso))
clusters[ok] <- km_fit$cluster

# majority label for each cluster
cluster_labels <- bind_cols(cluster = clusters, meso = train_df$meso) %>%
  drop_na() %>%
  count(cluster, meso, name = "n") %>%
  group_by(cluster) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(cluster, meso)

# ---- predict clusters for TEST (no predict.kmeans in base R) ----
X_test <- test_df %>% select(-meso)
keep_test <- complete.cases(X_test)
test_mat <- as.matrix(X_test)

# scale test with training parameters
test_scaled <- matrix(NA_real_, nrow = nrow(test_mat), ncol = ncol(test_mat))
colnames(test_scaled) <- colnames(test_mat)
if (any(keep_test)) {
  test_scaled[keep_test, ] <- scale(
    test_mat[keep_test, , drop = FALSE],
    center = train_center,
    scale  = train_scale
  )
}

# assign each test row to the nearest cluster center (Euclidean, in scaled space)
# distance^2 = ||x||^2 + ||c||^2 - 2 x·c
test_cl <- rep(NA_integer_, nrow(test_mat))
if (any(keep_test)) {
  rsx <- rowSums(test_scaled[keep_test, , drop = FALSE]^2)
  rsc <- rowSums(km_fit$centers^2)
  dist2 <- outer(rsx, rsc, "+") - 2 * (test_scaled[keep_test, , drop = FALSE] %*% t(km_fit$centers))
  test_cl[keep_test] <- max.col(-dist2, ties.method = "first")  # index of nearest center
}

# map clusters -> mesohabitat labels
cl_to_lbl <- setNames(as.character(cluster_labels$meso), cluster_labels$cluster)
semi_pred_lbl <- rep(NA_character_, length(test_cl))
semi_pred_lbl[!is.na(test_cl)] <- cl_to_lbl[as.character(test_cl[!is.na(test_cl)])]

semi_eval_df <- tibble(
  obs  = test_df$meso,
  pred = factor(semi_pred_lbl, levels = all_lvls)
) %>% drop_na()

semi_cm <- yardstick::conf_mat(semi_eval_df, truth = obs, estimate = pred)
semi_acc <- yardstick::accuracy(semi_eval_df, obs, pred)
semi_kap <- yardstick::kap(semi_eval_df, obs, pred)


# ---- By-class metrics (per class) ----
# Robust per-class metrics from a yardstick conf_mat object (multi-class)
per_class_metrics <- function(cm) {
  tbl <- cm$table  # matrix: rows = truth, cols = prediction
  classes <- rownames(tbl)
  total <- sum(tbl)
  out <- lapply(classes, function(cl) {
    tp <- tbl[cl, cl]
    fn <- sum(tbl[cl, ]) - tp
    fp <- sum(tbl[, cl]) - tp
    tn <- total - tp - fn - fp
    
    prec_den <- tp + fp
    rec_den  <- tp + fn
    
    precision <- if (prec_den > 0) tp / prec_den else NA_real_
    recall    <- if (rec_den  > 0) tp / rec_den  else NA_real_
    f1 <- if (isTRUE(precision > 0) && isTRUE(recall > 0) && (precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else NA_real_
    
    data.frame(.level = cl, precision = precision, recall = recall, f_meas = f1, 
               support = sum(tbl[cl, ]), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Use it:
semi_byclass <- per_class_metrics(semi_cm)

# ------------------------------
# SUPERVISED: Random Forest (tidymodels / ranger)
# ------------------------------
rf_train <- train_df %>% drop_na()

rec <- recipe(meso ~ ., data = rf_train) %>%
  step_normalize(all_predictors())

# compute inverse-frequency class weights
cw_tbl <- train_df %>% count(meso) %>%
  mutate(w = 1 / n, w = w / sum(w))
cw_vec <- cw_tbl$w; names(cw_vec) <- as.character(cw_tbl$meso)

rf_spec <- rand_forest(trees = 800,
                       mtry  = min(6, ncol(rf_train)-1),
                       min_n = 5) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "impurity",
             class.weights = cw_vec)

wf <- workflow() %>% add_model(rf_spec) %>% add_recipe(rec)
rf_fit <- fit(wf, rf_train)

prep_rec <- prep(rec)
test_baked <- bake(prep_rec, new_data = test_df %>% select(-meso))
rf_pred <- predict(rf_fit, new_data = test_baked, type = "class")$.pred_class

rf_eval_df <- tibble(
  obs  = test_df$meso,
  pred = factor(rf_pred, levels = all_lvls)
) %>% drop_na()

rf_cm <- conf_mat(rf_eval_df, truth = obs, estimate = pred)
rf_acc <- accuracy(rf_eval_df, obs, pred)
rf_kap <- kap(rf_eval_df, obs, pred)
rf_byclass <- metrics(rf_eval_df, obs, pred, options = estimator_multinomial()) %>%
  filter(.metric %in% c("precision","recall","f_meas"))

# ------------------------------
# Outputs
# ------------------------------
out_dir <- "_data/Tables/meso_classification"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Overall
write.csv(bind_rows(
  tibble(model="SemiSupervised", .metric="accuracy", .estimate=semi_acc$.estimate),
  tibble(model="SemiSupervised", .metric="kappa",    .estimate=semi_kap$.estimate),
  tibble(model="SupervisedRF",   .metric="accuracy", .estimate=rf_acc$.estimate),
  tibble(model="SupervisedRF",   .metric="kappa",    .estimate=rf_kap$.estimate)
), file.path(out_dir, glue("overall_{test_map$meso_layer}.csv")), row.names = FALSE)

# Confusion matrices
write.csv(as.data.frame(semi_cm$table), file.path(out_dir, glue("semi_confusion_{test_map$meso_layer}.csv")), row.names = FALSE)
write.csv(as.data.frame(rf_cm$table),   file.path(out_dir, glue("rf_confusion_{test_map$meso_layer}.csv")),   row.names = FALSE)

# Per-class metrics
write.csv(semi_byclass, file.path(out_dir, glue("semi_byclass_{test_map$meso_layer}.csv")), row.names = FALSE)
write.csv(rf_byclass,   file.path(out_dir, glue("rf_byclass_{test_map$meso_layer}.csv")),   row.names = FALSE)

cat(glue("\nDone. Train: {paste(train_ids, collapse=', ')} | Test: {test_id}\nOutputs: {normalizePath(out_dir)}\n"))
