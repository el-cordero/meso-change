library(terra)
library(cluster)

kmeans_diagnostics <- function(
    r,
    k_max = 40,
    km_n = 50000,
    sil_n = 5000,
    seed = 99,
    nstart = 50,
    iter.max = 500,
    algorithm = "Hartigan-Wong",
    scale_values = TRUE,
    plot_results = TRUE,
    verbose = TRUE
) {
  # Drop cells with any NA across layers
  valid_mask <- app(!is.na(r), fun = all)
  r_valid <- mask(r, valid_mask)
  
  # Optionally scale values
  if (isTRUE(scale_values)) {
    r_valid <- scale(r_valid)
  }
  
  # Convert raster to data.frame
  df <- as.data.frame(r_valid, cell = FALSE, na.rm = TRUE)
  
  if (nrow(df) < 2) {
    stop("Not enough complete rows after NA removal.")
  }
  
  # Sample for K-means
  set.seed(seed)
  km_n_use <- min(km_n, nrow(df))
  km_idx <- sample.int(nrow(df), km_n_use)
  df_km <- df[km_idx, , drop = FALSE]
  
  # Sample for silhouette
  sil_n_use <- min(sil_n, nrow(df_km))
  sil_idx <- sample.int(nrow(df_km), sil_n_use)
  
  # Storage
  wcss <- numeric(k_max)
  sil_scores <- rep(NA_real_, k_max)
  
  # Run K-means across k
  for (k in 1:k_max) {
    model <- kmeans(
      df_km,
      centers = k,
      nstart = nstart,
      iter.max = iter.max,
      algorithm = algorithm
    )
    
    wcss[k] <- model$tot.withinss
    
    if (k > 1) {
      sil_widths <- silhouette(
        model$cluster[sil_idx],
        dist(df_km[sil_idx, , drop = FALSE])
      )
      sil_scores[k] <- mean(sil_widths[, "sil_width"])
    }
    
    if (isTRUE(verbose)) {
      message("k = ", k, " done")
    }
  }
  
  # Assemble outputs
  results_df <- data.frame(
    k = 1:k_max,
    wcss = wcss,
    sil_width = sil_scores
  )
  
  # Optional plotting
  if (isTRUE(plot_results)) {
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(mfrow = c(1, 2))
    
    plot(
      results_df$k,
      results_df$wcss,
      type = "b",
      pch = 16,
      xlab = "k",
      ylab = "Total within-cluster SS",
      main = "Elbow Method"
    )
    
    plot(
      results_df$k[2:k_max],
      results_df$sil_width[2:k_max],
      type = "b",
      pch = 16,
      xlab = "k",
      ylab = "Mean silhouette width",
      main = "Silhouette Analysis"
    )
  }
  
  invisible(list(
    results = results_df,
    df_km = df_km,
    km_idx = km_idx,
    sil_idx = sil_idx,
    settings = list(
      k_max = k_max,
      km_n = km_n_use,
      sil_n = sil_n_use,
      seed = seed,
      nstart = nstart,
      iter.max = iter.max,
      algorithm = algorithm,
      scale_values = scale_values
    )
  ))
}