source("_scripts/03_data_analysis/08_binary_config.R")

undersample_binary <- function(data, outcome, seed) {
  outcome <- rlang::ensym(outcome)
  counts <- data |> count(!!outcome, name = "n_available")
  if (nrow(counts) != 2L || any(counts$n_available < 1L)) {
    stop("Binary fitting requires nonempty land and surface-water classes.")
  }
  target_n <- min(counts$n_available)
  set.seed(seed)
  balanced <- data |>
    group_by(!!outcome) |>
    group_modify(~ .x[sample.int(nrow(.x), target_n), , drop = FALSE]) |>
    ungroup()
  list(data = balanced, counts = counts, target_n = target_n)
}

binary_metrics_from_factors <- function(truth, estimate) {
  truth <- factor(truth, levels = BINARY_LEVELS)
  estimate <- factor(estimate, levels = BINARY_LEVELS)
  cm <- table(truth = truth, estimate = estimate)
  tn <- unname(cm["land", "land"])
  fp <- unname(cm["land", "water"])
  fn <- unname(cm["water", "land"])
  tp <- unname(cm["water", "water"])
  binary_metrics_from_counts(tp, tn, fp, fn)
}

binary_metrics_from_counts <- function(tp, tn, fp, fn) {
  safe_ratio <- function(numerator, denominator) {
    ifelse(denominator > 0, numerator / denominator, NA_real_)
  }
  n <- tp + tn + fp + fn
  sensitivity <- safe_ratio(tp, tp + fn)
  specificity <- safe_ratio(tn, tn + fp)
  precision <- safe_ratio(tp, tp + fp)
  npv <- safe_ratio(tn, tn + fn)
  f1 <- safe_ratio(2 * precision * sensitivity, precision + sensitivity)
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  tibble(
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    accuracy = safe_ratio(tp + tn, n),
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    npv = npv,
    f1 = f1,
    mcc = ifelse(denominator > 0, (tp * tn - fp * fn) / denominator, NA_real_),
    informedness_j = sensitivity + specificity - 1,
    balanced_accuracy = (sensitivity + specificity) / 2,
    prevalence = safe_ratio(tp + fn, n),
    detection_prevalence = safe_ratio(tp + fp, n)
  )
}

binary_confusion_wide <- function(truth, estimate) {
  cm <- table(
    truth = factor(truth, levels = BINARY_LEVELS),
    estimate = factor(estimate, levels = BINARY_LEVELS)
  )
  as.data.frame.matrix(cm) |> rownames_to_column("truth")
}

predict_binary_codes <- function(model, data) {
  prediction <- predict(
    model,
    data = as.data.frame(data),
    num.threads = BINARY_THREADS
  )$predictions
  match(as.character(prediction), BINARY_LEVELS)
}

write_binary_stack <- function(paths, layer_names, output_path) {
  output <- rast(paths)
  output <- as.factor(output)
  categories <- data.frame(value = 1:2, class = unname(BINARY_LABELS[BINARY_LEVELS]))
  for (i in seq_len(nlyr(output))) levels(output[[i]]) <- categories
  names(output) <- layer_names
  writeRaster(
    output,
    output_path,
    overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW", "PREDICTOR=2"))
  )
  invisible(rast(output_path))
}

mean_ci_binary <- function(x, conf = 0.95) {
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

paired_wilcox_binary <- function(data, value_col, group_col, id_col, group_1, group_2) {
  wide <- data |>
    select(all_of(c(id_col, group_col, value_col))) |>
    pivot_wider(names_from = all_of(group_col), values_from = all_of(value_col)) |>
    filter(is.finite(.data[[group_1]]), is.finite(.data[[group_2]]))
  if (nrow(wide) < 2L) {
    return(tibble(n_pairs = nrow(wide), statistic = NA_real_, p_value = NA_real_))
  }
  result <- suppressWarnings(stats::wilcox.test(
    wide[[group_1]], wide[[group_2]], paired = TRUE, exact = FALSE
  ))
  tibble(n_pairs = nrow(wide), statistic = unname(result$statistic), p_value = result$p.value)
}

confusion_from_binary_rasters <- function(predicted_water, observed_water) {
  combined <- predicted_water * 2 + observed_water
  frequencies <- freq(combined)
  counts <- setNames(rep(0, 4), as.character(0:3))
  if (!is.null(frequencies) && nrow(frequencies)) {
    counts[as.character(frequencies$value)] <- frequencies$count
  }
  binary_metrics_from_counts(
    tp = unname(counts["3"]),
    tn = unname(counts["0"]),
    fp = unname(counts["2"]),
    fn = unname(counts["1"])
  )
}

count_binary_codes <- function(layer) {
  numeric_layer <- if (terra::is.factor(layer)) as.numeric(layer) else layer
  frequencies <- freq(numeric_layer)
  observed <- if (is.null(frequencies) || !nrow(frequencies)) {
    tibble(code = integer(), n = numeric())
  } else {
    tibble(code = as.integer(frequencies$value), n = as.numeric(frequencies$count))
  }
  tibble(code = 1:2) |>
    left_join(observed, by = "code") |>
    mutate(n = replace_na(n, 0), class = BINARY_LEVELS[code])
}
