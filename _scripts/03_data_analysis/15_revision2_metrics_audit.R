#!/usr/bin/env Rscript
# Reproduce R2 reporting from saved confusion counts. No model fitting,
# raster prediction, or changes to historical results are performed.
# Run from the project root:
# Rscript _scripts/03_data_analysis/15_revision2_metrics_audit.R

input_dir <- "_data/Tables/balanced_analysis"
binary_dir <- "_data/Tables/binary_surface_water"
output_dir <- "_data/Tables/revision2"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

classes <- c("Shallow Pool", "Medium Pool", "Deep Pool", "Slow Riffle",
             "Fast Riffle", "Raceway", "Faster than Raceway", "Faster than Deep Pool")
models <- c("Cluster-assisted", "Random forest")
read_counts <- function(name) read.csv(file.path(input_dir, name), check.names = FALSE)
write_result <- function(data, name) {
  write.csv(data, file.path(output_dir, name), row.names = FALSE, na = "")
}

class_metrics <- function(cm) {
  observed <- rowSums(cm)
  predicted <- colSums(cm)
  tp <- diag(cm)
  data.frame(
    class = classes, support = observed, predicted = predicted, tp = tp,
    precision = ifelse(predicted > 0, tp / predicted, NA_real_),
    recall = ifelse(observed > 0, tp / observed, NA_real_),
    f1 = ifelse(observed + predicted > 0, 2 * tp / (observed + predicted), NA_real_),
    row.names = NULL
  )
}

overall_metrics <- function(cm) {
  cm_class <- class_metrics(cm)
  accuracy <- sum(diag(cm)) / sum(cm)
  expected <- sum(rowSums(cm) * colSums(cm)) / sum(cm)^2
  c(accuracy = accuracy, kappa = (accuracy - expected) / (1 - expected),
    bal_accuracy = mean(cm_class$recall, na.rm = TRUE),
    macro_f1 = mean(cm_class$f1, na.rm = TRUE))
}

mean_ci <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (!n) return(c(n = 0, mean = NA, sd = NA, ci_low = NA, ci_high = NA,
                   ci_half = NA, minimum = NA, maximum = NA))
  half <- if (n > 1) qt(.975, n - 1) * sd(x) / sqrt(n) else NA_real_
  c(n = n, mean = mean(x), sd = sd(x), ci_low = mean(x) - half,
    ci_high = mean(x) + half, ci_half = half, minimum = min(x), maximum = max(x))
}

saved <- read_counts("mesohabitat_loo_confusion_by_fold.csv")
dates <- unique(saved$test_id)
stopifnot(length(dates) == 7L)
folds <- list()
by_class <- list()
for (date in dates) for (model in models) {
  rows <- saved[saved$test_id == date & saved$model == model, ]
  stopifnot(nrow(rows) == 8L, setequal(rows$truth, classes))
  cm <- as.matrix(rows[match(classes, rows$truth), classes])
  stopifnot(all(is.finite(cm)), all(cm >= 0), all(rowSums(cm) > 0))
  values <- overall_metrics(cm)
  key <- paste(date, model)
  folds[[key]] <- data.frame(test_id = date, model = model,
                            .metric = names(values), .estimate = unname(values))
  by_class[[key]] <- cbind(test_id = date, model = model, class_metrics(cm))
}
folds <- do.call(rbind, folds)
by_class <- do.call(rbind, by_class)
rownames(folds) <- rownames(by_class) <- NULL

summaries <- list()
tests <- list()
for (metric in unique(folds$.metric)) {
  for (model in models) {
    rows <- folds[folds$.metric == metric & folds$model == model, ]
    summaries[[paste(model, metric)]] <- cbind(
      data.frame(model = model, .metric = metric),
      as.data.frame(as.list(mean_ci(rows$.estimate)))
    )
  }
  ca <- folds[folds$.metric == metric & folds$model == models[1], ]
  rf <- folds[folds$.metric == metric & folds$model == models[2], ]
  rf <- rf[match(ca$test_id, rf$test_id), ]
  test <- suppressWarnings(wilcox.test(ca$.estimate, rf$.estimate,
                                       paired = TRUE, exact = FALSE))
  tests[[metric]] <- data.frame(.metric = metric, n_pairs = nrow(ca),
                                statistic = unname(test$statistic), p_value = test$p.value)
}
summaries <- do.call(rbind, summaries)
tests <- do.call(rbind, tests)
rownames(summaries) <- rownames(tests) <- NULL

pooled_rows <- read_counts("mesohabitat_loo_confusion_pooled.csv")
pooled <- list()
for (model in models) {
  cm <- matrix(0, 8, 8, dimnames = list(classes, classes))
  rows <- pooled_rows[pooled_rows$model == model, ]
  for (i in seq_len(nrow(rows))) cm[rows$truth[i], rows$prediction[i]] <- rows$count[i]
  from_folds <- matrix(0, 8, 8)
  for (date in dates) {
    date_rows <- saved[saved$test_id == date & saved$model == model, ]
    from_folds <- from_folds + as.matrix(date_rows[match(classes, date_rows$truth), classes])
  }
  stopifnot(identical(unname(cm), unname(from_folds)))
  pooled[[model]] <- cbind(model = model, class_metrics(cm))
}
pooled <- do.call(rbind, pooled)
rownames(pooled) <- NULL
stopifnot(all(tapply(pooled$support, pooled$model, sum) == 911543))

historical <- read_counts("mesohabitat_loo_fold_metrics.csv")
compared <- merge(folds, historical,
                  by = c("test_id", "model", ".metric"), suffixes = c("_R2", "_previous"))
compared$difference <- compared$.estimate_R2 - compared$.estimate_previous
stopifnot(nrow(compared) == 56L,
          all(abs(compared$difference[compared$.metric != "macro_f1"]) < 1e-12),
          all(abs(compared$difference[compared$.metric == "macro_f1" &
                                      compared$model == "Random forest"]) < 1e-12))

binary <- read.csv(file.path(binary_dir, "hydraulic_date_specific_validation.csv"))
binary_audit <- do.call(rbind, lapply(seq_len(nrow(binary)), function(i) {
  x <- binary[i, ]
  sensitivity <- x$tp / (x$tp + x$fn)
  specificity <- x$tn / (x$tn + x$fp)
  data.frame(test_id = x$test_id, workflow = x$workflow, tp = x$tp, tn = x$tn,
             fp = x$fp, fn = x$fn,
             sensitivity = sensitivity, specificity = specificity,
             balanced_accuracy = (sensitivity + specificity) / 2,
             saved_balanced_accuracy = x$balanced_accuracy)
}))
stopifnot(nrow(binary_audit) == 14L,
          all(abs(binary_audit$balanced_accuracy - binary_audit$saved_balanced_accuracy) < 1e-12))

write_result(folds, "mesohabitat_loo_fold_metrics.csv")
write_result(summaries, "mesohabitat_loo_summary_95ci.csv")
write_result(tests, "mesohabitat_loo_paired_tests.csv")
write_result(by_class, "mesohabitat_classwise_by_fold.csv")
write_result(pooled, "mesohabitat_classwise_pooled.csv")
write_result(compared, "mesohabitat_metric_change_audit.csv")
write_result(binary_audit, "hydraulic_date_specific_metric_audit.csv")

writeLines(c(
  "R2 results derived from saved confusion counts without model fitting or prediction.",
  "Class F1 = 2 TP / (observed support + predicted support).",
  "Classes with observed support but no correct predictions contribute F1 = 0.",
  "Precision is undefined when the model never predicts a class (blank in CSV).",
  "Macro F1 averages all eight classes in each held-out date.",
  "Table 7 summarizes seven dates with equal date weight and t-based 95% intervals.",
  "Paired tests retain the prior two-sided continuity-corrected Wilcoxon procedure.",
  "P-values are unadjusted exploratory comparisons.",
  "Pooled class metrics weight pixel-date observations and are descriptive.",
  "All previous accuracy, kappa and balanced-accuracy values reproduced.",
  "Random-forest macro F1 reproduced. All 14 binary balanced accuracies reproduced."
), file.path(output_dir, "reporting_notes.txt"))
writeLines(capture.output(sessionInfo()), file.path(output_dir, "R_sessionInfo.txt"))
message("R2 summary audit complete: ", output_dir)
print(summaries[summaries$.metric == "macro_f1", ], row.names = FALSE)
print(tests[tests$.metric == "macro_f1", ], row.names = FALSE)
