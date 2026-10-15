undersample_to_minimum <- function(data, outcome, seed) {
  outcome <- rlang::ensym(outcome)
  counts <- data |>
    count(!!outcome, name = "n_available")
  if (nrow(counts) < 2L || any(counts$n_available < 1L)) {
    stop("At least two nonempty classes are required for undersampling.")
  }
  target_n <- min(counts$n_available)
  set.seed(seed)
  balanced <- data |>
    group_by(!!outcome) |>
    group_modify(~ .x[sample.int(nrow(.x), target_n), , drop = FALSE]) |>
    ungroup()
  list(
    data = balanced,
    counts = counts |> mutate(n_balanced = target_n),
    target_n = target_n
  )
}

classification_metrics <- function(truth, estimate) {
  truth <- factor(truth, levels = levels(truth))
  estimate <- factor(estimate, levels = levels(truth))
  confusion <- table(truth = truth, estimate = estimate)
  multiclass_metrics_from_matrix(confusion)
}

multiclass_metrics_from_matrix <- function(confusion) {
  confusion <- as.matrix(confusion)
  total <- sum(confusion)
  if (!total) stop("Cannot calculate metrics from an empty confusion matrix.")
  true_totals <- rowSums(confusion)
  predicted_totals <- colSums(confusion)
  true_positive <- diag(confusion)
  recall <- true_positive / true_totals
  precision <- true_positive / predicted_totals
  # A reference class with no correct predictions contributes zero F1. Using
  # precision and recall here would produce NaN and omit failed classes.
  f1_denominator <- true_totals + predicted_totals
  class_f1 <- ifelse(f1_denominator > 0, 2 * true_positive / f1_denominator, NA_real_)
  observed_agreement <- sum(true_positive) / total
  expected_agreement <- sum(true_totals * predicted_totals) / total^2
  tibble(
    .metric = c("accuracy", "kappa", "bal_accuracy", "macro_f1"),
    .estimate = c(
      observed_agreement,
      (observed_agreement - expected_agreement) / (1 - expected_agreement),
      mean(recall, na.rm = TRUE),
      mean(class_f1, na.rm = TRUE)
    )
  )
}

metrics_from_wide_confusion <- function(data, group_columns) {
  class_columns <- setdiff(names(data), c(group_columns, "truth"))
  data |>
    group_by(across(all_of(group_columns))) |>
    group_modify(~ {
      matrix_data <- as.matrix(select(.x, all_of(class_columns)))
      rownames(matrix_data) <- .x$truth
      matrix_data <- matrix_data[class_columns, class_columns, drop = FALSE]
      multiclass_metrics_from_matrix(matrix_data)
    }) |>
    ungroup()
}

mean_ci <- function(x, conf = 0.95) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (!n) {
    return(tibble(n = 0L, mean = NA_real_, sd = NA_real_, ci_low = NA_real_, ci_high = NA_real_, minimum = NA_real_, maximum = NA_real_))
  }
  estimate <- mean(x)
  spread <- stats::sd(x)
  half_width <- if (n > 1L) stats::qt((1 + conf) / 2, df = n - 1) * spread / sqrt(n) else NA_real_
  tibble(
    n = n,
    mean = estimate,
    sd = spread,
    ci_low = estimate - half_width,
    ci_high = estimate + half_width,
    minimum = min(x),
    maximum = max(x)
  )
}

paired_wilcox <- function(data, value_col, group_col, id_col, group_1, group_2) {
  wide <- data |>
    select(all_of(c(id_col, group_col, value_col))) |>
    pivot_wider(names_from = all_of(group_col), values_from = all_of(value_col)) |>
    filter(is.finite(.data[[group_1]]), is.finite(.data[[group_2]]))
  if (nrow(wide) < 2L) {
    return(tibble(n_pairs = nrow(wide), statistic = NA_real_, p_value = NA_real_))
  }
  result <- suppressWarnings(stats::wilcox.test(wide[[group_1]], wide[[group_2]], paired = TRUE, exact = FALSE))
  tibble(n_pairs = nrow(wide), statistic = unname(result$statistic), p_value = result$p.value)
}

multiclass_confusion <- function(truth, estimate) {
  lvls <- levels(truth)
  as.data.frame.matrix(table(
    truth = factor(truth, levels = lvls),
    estimate = factor(estimate, levels = lvls)
  )) |>
    rownames_to_column("truth")
}

predict_ranger_codes <- function(model, data, class_levels) {
  predictions <- predict(model, data = as.data.frame(data), num.threads = N_THREADS)$predictions
  match(as.character(predictions), class_levels)
}

make_categorical_stack <- function(paths, layer_names, categories, output_path) {
  output <- rast(paths)
  output <- as.factor(output)
  for (i in seq_len(nlyr(output))) {
    levels(output[[i]]) <- categories
  }
  names(output) <- layer_names
  writeRaster(
    output,
    output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
  )
  invisible(rast(output_path))
}

safe_scale <- function(x, center, scale) {
  scale[!is.finite(scale) | scale == 0] <- 1
  sweep(sweep(x, 2, center, "-"), 2, scale, "/")
}

kmeans_predict_codes <- function(model, data) {
  x <- as.matrix(data)
  scaled <- safe_scale(x, model$center, model$scale)
  distance_sq <- outer(rowSums(scaled^2), rowSums(model$centers^2), "+") - 2 * (scaled %*% t(model$centers))
  clusters <- max.col(-distance_sq, ties.method = "first")
  unname(model$cluster_to_code[as.character(clusters)])
}

binary_metrics <- function(tp, tn, fp, fn) {
  n <- tp + tn + fp + fn
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  precision <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  tibble(
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    accuracy = (tp + tn) / n,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    npv = npv,
    f1 = 2 * precision * sensitivity / (precision + sensitivity),
    mcc = ifelse(denominator > 0, (tp * tn - fp * fn) / denominator, NA_real_),
    informedness_j = sensitivity + specificity - 1,
    balanced_accuracy = (sensitivity + specificity) / 2,
    prevalence = (tp + fn) / n,
    detection_prevalence = (tp + fp) / n
  )
}

confusion_from_binary_rasters <- function(pred_water, obs_water) {
  combined <- pred_water * 2 + obs_water
  frequencies <- freq(combined)
  counts <- setNames(rep(0, 4), as.character(0:3))
  if (!is.null(frequencies) && nrow(frequencies)) {
    counts[as.character(frequencies$value)] <- frequencies$count
  }
  binary_metrics(
    tp = unname(counts["3"]),
    tn = unname(counts["0"]),
    fp = unname(counts["2"]),
    fn = unname(counts["1"])
  )
}
