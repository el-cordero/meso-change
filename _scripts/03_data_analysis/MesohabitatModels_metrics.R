suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(glue)
  library(readr)
})

VERBOSE <- TRUE
vmsg <- function(...) if (VERBOSE) message(glue(...))

# ------------------------------
# Paths (match your project)
# ------------------------------
gt_path        <- "_data/GIS/Raster/Clean/mesohabitats.tif"
pred_out_dir   <- "_data/GIS/Raster/Outputs/meso_predictions"
tables_out_dir <- "_data/Tables/meso_classification"
dir.create(tables_out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------
# Settings: MUST match modeling
# ------------------------------
FACT_AGG <- 5  # you used fact <- 5 in the modeling script

ALLOW_RESAMPLE_IF_MISMATCH <- TRUE

# ------------------------------
# Load GT
# ------------------------------
vmsg("Loading GT: {gt_path}")
meso_gt_raw <- rast(gt_path)  # contains 7 folds + median layers in your file

# Keep only fold layers (01_...07_...)
all_dates <- names(meso_gt_raw)
all_dates <- all_dates[grepl("^\\d{2}_[A-Za-z]+\\d{1,2}_\\d{4}$", all_dates)]

vmsg("Found {length(all_dates)} fold GT layers: {paste(all_dates, collapse=', ')}")

# Aggregate GT to match prediction support (15 m)
vmsg("Aggregating GT to match prediction support (fact={FACT_AGG}, modal)...")
meso_gt <- terra::aggregate(meso_gt_raw[[all_dates]], fact = FACT_AGG, fun = "modal", na.rm = TRUE)

# ------------------------------
# Robust helpers
# ------------------------------
# Multi-class MCC (Gorodkin / generalized MCC) computed in double precision.
mcc_multiclass <- function(conf_mat) {
  # conf_mat: square matrix [K x K], rows=truth, cols=pred
  C <- as.matrix(conf_mat)
  C <- matrix(as.double(C), nrow = nrow(C), ncol = ncol(C))

  s <- sum(C)  # total
  if (s <= 0) return(NA_real_)

  # row/col sums
  t_k <- rowSums(C)  # truth totals
  p_k <- colSums(C)  # pred totals

  c <- sum(diag(C))  # correct

  # denominator pieces
  sum_pk_tk <- sum(p_k * t_k)
  denom_left  <- s^2 - sum(p_k^2)
  denom_right <- s^2 - sum(t_k^2)
  denom <- sqrt(denom_left * denom_right)

  if (!is.finite(denom) || denom <= 0) return(NA_real_)

  (c * s - sum_pk_tk) / denom
}

macro_f1_from_conf <- function(conf_mat) {
  C <- as.matrix(conf_mat)
  K <- nrow(C)

  f1s <- numeric(K)
  for (k in seq_len(K)) {
    tp <- C[k, k]
    fp <- sum(C[, k]) - tp
    fn <- sum(C[k, ]) - tp
    prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    f1s[k] <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA_real_
  }
  mean(f1s, na.rm = TRUE)
}

bal_acc_from_conf <- function(conf_mat) {
  C <- as.matrix(conf_mat)
  K <- nrow(C)
  recs <- numeric(K)
  for (k in seq_len(K)) {
    tp <- C[k, k]
    fn <- sum(C[k, ]) - tp
    recs[k] <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  }
  mean(recs, na.rm = TRUE)
}

conf_from_vectors <- function(truth, pred, levels) {
  truth <- factor(truth, levels = levels)
  pred  <- factor(pred,  levels = levels)
  tbl <- table(truth, pred, useNA = "no")
  as.matrix(tbl)
}

align_pred_to_gt <- function(pr_r, gt_r, gt_layer_name, model_key) {
  # Crop to intersection first (prevents extent edge artifacts)
  inter <- terra::intersect(ext(pr_r), ext(gt_r))
  if (is.null(inter)) stop("No overlap between GT and pred for ", gt_layer_name, " (", model_key, ")")

  pr2 <- crop(pr_r, inter)
  gt2 <- crop(gt_r, inter)

  if (!compareGeom(pr2, gt2, stopOnError = FALSE)) {
    if (!ALLOW_RESAMPLE_IF_MISMATCH) {
      stop("Geometry mismatch for ", gt_layer_name, " (", model_key, ").")
    }
    vmsg("Geometry mismatch for {gt_layer_name} ({model_key}). Resampling pred -> GT grid (near)...")
    pr2 <- resample(pr2, gt2, method = "near")
  }
  list(pred = pr2, gt = gt2)
}

metrics_for_date <- function(gt_layer_name, model_key = c("semi","rf")) {
  model_key <- match.arg(model_key)

  gt_r <- meso_gt[[gt_layer_name]]

  pred_path <- file.path(pred_out_dir, paste0(model_key, "_pred_", gt_layer_name, ".tif"))
  if (!file.exists(pred_path)) {
    vmsg("Missing pred raster: {pred_path}")
    return(tibble(
      test_id = gt_layer_name,
      model   = ifelse(model_key == "semi", "SemiSupervised", "SupervisedRF"),
      accuracy = NA_real_,
      bal_accuracy = NA_real_,
      macro_f1 = NA_real_,
      mcc = NA_real_
    ))
  }

  pr_r <- rast(pred_path)

  aligned <- align_pred_to_gt(pr_r, gt_r, gt_layer_name, model_key)
  pr_a <- aligned$pred
  gt_a <- aligned$gt

  # Extract values, drop NA
  gt_v <- values(gt_a, mat = FALSE)
  pr_v <- values(pr_a, mat = FALSE)

  keep <- !is.na(gt_v) & !is.na(pr_v)
  gt_v <- gt_v[keep]
  pr_v <- pr_v[keep]

  if (length(gt_v) < 10) {
    return(tibble(
      test_id = gt_layer_name,
      model   = ifelse(model_key == "semi", "SemiSupervised", "SupervisedRF"),
      accuracy = NA_real_,
      bal_accuracy = NA_real_,
      macro_f1 = NA_real_,
      mcc = NA_real_
    ))
  }

  # Use common levels from GT RAT if present; otherwise union of codes
  lev <- NULL
  lvl <- levels(gt_a)[[1]]
  if (!is.null(lvl) && "value" %in% names(lvl)) {
    lev <- as.integer(lvl$value)
  } else {
    lev <- sort(unique(c(gt_v, pr_v)))
  }

  C <- conf_from_vectors(gt_v, pr_v, levels = lev)

  acc <- sum(diag(C)) / sum(C)
  bal <- bal_acc_from_conf(C)
  f1  <- macro_f1_from_conf(C)
  mcc <- mcc_multiclass(C)

  tibble(
    test_id = gt_layer_name,
    model   = ifelse(model_key == "semi", "SemiSupervised", "SupervisedRF"),
    accuracy = acc,
    bal_accuracy = bal,
    macro_f1 = f1,
    mcc = mcc
  )
}

# ------------------------------
# Compute metrics for all folds
# ------------------------------
vmsg("Computing metrics from saved rasters for {length(all_dates)} dates...")

fold_metrics <- bind_rows(
  purrr::map_dfr(all_dates, \(d) metrics_for_date(d, "semi")),
  purrr::map_dfr(all_dates, \(d) metrics_for_date(d, "rf"))
) %>%
  tidyr::pivot_longer(
    cols = c(accuracy, bal_accuracy, macro_f1, mcc),
    names_to = ".metric",
    values_to = ".estimate"
  ) %>%
  dplyr::arrange(test_id, model, .metric)

# Summary like your original
fold_summary <- fold_metrics %>%
  group_by(model, .metric) %>%
  summarise(
    mean = mean(.estimate, na.rm = TRUE),
    sd   = sd(.estimate, na.rm = TRUE),
    min  = min(.estimate, na.rm = TRUE),
    max  = max(.estimate, na.rm = TRUE),
    .groups = "drop"
  )

# Save
write_csv(fold_metrics, file.path(tables_out_dir, "loo_fold_metrics_FROM_SAVED.csv"))
write_csv(fold_summary, file.path(tables_out_dir, "loo_summary_FROM_SAVED.csv"))

vmsg("Saved:\n  {file.path(tables_out_dir, 'loo_fold_metrics_FROM_SAVED.csv')}\n  {file.path(tables_out_dir, 'loo_summary_FROM_SAVED.csv')}")
print(fold_summary)
