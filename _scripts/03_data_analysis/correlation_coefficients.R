library(tidyverse)

# save dataset
ls_df <- read.csv('_data/Tables/resultsTimeSeries_landsat.csv')
pl_df <- read.csv('_data/Tables/resultsTimeSeries_planet.csv')

ls_df_km_water <- ls_df %>% filter(class == 'water', method == 'kmeans') 
ls_df_rf_water <- ls_df %>% filter(class == 'water', method == 'randomforest')
ls_df_km_bareland <- ls_df %>% filter(class == 'bareland', method == 'kmeans') 
ls_df_rf_bareland <- ls_df %>% filter(class == 'bareland', method == 'randomforest')

# cor(ls_df_rf_water$area_diff,ls_df_rf_bareland$area_diff, 
#     method = "pearson", 
#     use ='complete.obs')
# cor(ls_df_km_water$area_diff_baseline,ls_df_km_bareland$area_diff_baseline, 
#     method = "pearson")

cor(ls_df_rf_water$area_diff,ls_df_rf_bareland$area_diff, 
    method = "kendall", 
    use ='complete.obs')
cor(ls_df_rf_water$area_diff_baseline,ls_df_rf_bareland$area_diff_baseline, 
    method = "kendall", 
    use ='complete.obs')
cor(ls_df_km_water$area_diff,ls_df_km_bareland$area_diff, 
    method = "kendall", 
    use ='complete.obs')
cor(ls_df_km_water$area_diff_baseline,ls_df_km_bareland$area_diff_baseline, 
    method = "kendall", 
    use ='complete.obs')

# cor(ls_df_rf_water$area_diff,ls_df_rf_bareland$area_diff, 
#     method = "spearman", 
#     use ='complete.obs')
# cor(ls_df_km_water$area_diff_baseline,ls_df_km_bareland$area_diff_baseline, 
#     method = "spearman")


pl_df_km_water <- pl_df %>% filter(class == 'water', method == 'kmeans') 
pl_df_rf_water <- pl_df %>% filter(class == 'water', method == 'randomforest')
pl_df_km_bareland <- pl_df %>% filter(class == 'bareland', method == 'kmeans') 
pl_df_rf_bareland <- pl_df %>% filter(class == 'bareland', method == 'randomforest')

# cor(pl_df_rf_water$area_diff,pl_df_rf_bareland$area_diff, 
#     method = "pearson", 
#     use ='complete.obs')
# cor(pl_df_km_water$area_diff_baseline,pl_df_km_bareland$area_diff_baseline, 
#     method = "pearson")

cor(pl_df_rf_water$area_diff,pl_df_rf_bareland$area_diff, 
    method = "kendall", 
    use ='complete.obs')
cor(pl_df_rf_water$area_diff_baseline,pl_df_rf_bareland$area_diff_baseline, 
    method = "kendall", 
    use ='complete.obs')
cor(pl_df_km_water$area_diff,pl_df_km_bareland$area_diff, 
    method = "kendall", 
    use ='complete.obs')
cor(pl_df_km_water$area_diff_baseline,pl_df_km_bareland$area_diff_baseline, 
    method = "kendall", 
    use ='complete.obs')

# cor(pl_df_rf_water$area_diff,pl_df_rf_bareland$area_diff, 
#     method = "spearman", 
#     use ='complete.obs')
# cor(pl_df_km_water$area_diff_baseline,pl_df_km_bareland$area_diff_baseline, 
#     method = "spearman")

