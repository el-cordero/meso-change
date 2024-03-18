random_forest_raster <- function(r, df, na_rows, model_date){
    df_clean <- na.omit(df[-na_rows,])

    # Partition the data for training and testing
    # 80% of the data will be used for the training
    data_split <- initial_split(df_clean, prop = 0.80, strata=class)  # Adjust the proportion as needed
    train_data <- training(data_split)
    test_data <- testing(data_split)

    rf_model_spec <- rand_forest(
        trees = 1000, min_n = 6, mtry = 78) %>%
        set_engine("ranger") %>%
        set_mode("classification")  

    rf_recipe <- recipe(class ~ ., data = train_data) #%>%
        # step_normalize(all_predictors())  # Example step to normalize predictors

    rf_workflow <- workflow() %>%
    add_model(rf_model_spec) %>%
    add_recipe(rf_recipe) %>%
    fit(data = train_data)

    rf_results <- predict(rf_workflow, new_data = test_data) %>%
    bind_cols(test_data) %>%  
    metrics(truth = class, estimate = .pred_class)
    rf_results$model <- model_date

    predictions <- predict(rf_workflow, new_data = df[-na_rows,-1])  # Combine predictions with true outcomes for evaluation
    df$predictions <- NULL
    df[-na_rows,'predictions'] <- predictions$.pred_class

    rf_raster <- init(r[[1]],NA)
    values(rf_raster) <- df$predictions
    names(rf_raster) <- model_date

    return(list(raster = rf_raster, results = rf_results))
}
