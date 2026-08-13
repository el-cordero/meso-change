library(tidyverse)

# ---------------------------------------------------------
# Read datasets
# ---------------------------------------------------------
ls_df <- read.csv('_data/Tables/resultsTimeSeries_landsat_core4.csv')
sn_df <- read.csv('_data/Tables/resultsTimeSeries_sentinel_core4.csv')
pl_df <- read.csv('_data/Tables/resultsTimeSeries_planet.csv')

# ---------------------------------------------------------
# Helper: merge bare land + cropland + urban
# ---------------------------------------------------------
prep_corr_df <- function(df) {
  df %>%
    mutate(
      class = case_when(
        class == "water" ~ "water",
        class %in% c("bareland", "cropland", "urban") ~ "bareland",
        TRUE ~ class
      )
    ) %>%
    filter(class %in% c("water", "bareland")) %>%
    group_by(date, method, class) %>%
    summarise(
      area = sum(area, na.rm = TRUE),
      area_diff = sum(area_diff, na.rm = TRUE),
      area_diff_baseline = sum(area_diff_baseline, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------------------------------------
# Helper function
# ---------------------------------------------------------
run_kendall_corrs <- function(df, sensor_name) {
  
  df2 <- prep_corr_df(df)
  
  km_water    <- df2 %>% filter(class == 'water',    method == 'kmeans') %>% arrange(date)
  rf_water    <- df2 %>% filter(class == 'water',    method == 'randomforest') %>% arrange(date)
  km_bareland <- df2 %>% filter(class == 'bareland', method == 'kmeans') %>% arrange(date)
  rf_bareland <- df2 %>% filter(class == 'bareland', method == 'randomforest') %>% arrange(date)
  
  cat('\n====================================\n')
  cat(sensor_name, '\n')
  cat('====================================\n')
  
  cat('\nRandom forest: area_diff\n')
  print(cor(
    rf_water$area_diff,
    rf_bareland$area_diff,
    method = "kendall",
    use = 'complete.obs'
  ))
  
  cat('\nRandom forest: area_diff_baseline\n')
  print(cor(
    rf_water$area_diff_baseline,
    rf_bareland$area_diff_baseline,
    method = "kendall",
    use = 'complete.obs'
  ))
  
  cat('\nK-means: area_diff\n')
  print(cor(
    km_water$area_diff,
    km_bareland$area_diff,
    method = "kendall",
    use = 'complete.obs'
  ))
  
  cat('\nK-means: area_diff_baseline\n')
  print(cor(
    km_water$area_diff_baseline,
    km_bareland$area_diff_baseline,
    method = "kendall",
    use = 'complete.obs'
  ))
}

# ---------------------------------------------------------
# Run correlations
# ---------------------------------------------------------
run_kendall_corrs(ls_df, 'Landsat-9')
run_kendall_corrs(sn_df, 'Sentinel-2')
run_kendall_corrs(pl_df, 'PlanetScope')