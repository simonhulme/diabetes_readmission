## XG Boost tidymodels ----

library(tidyverse)
library(tidymodels)
library(vip)
library(parallel)
library(doParallel)
library(themis)

train <- read_rds("00_Data/processed/for_analysis/diabetes_for_trees_train.rds")
test  <- read_rds("00_Data/processed/for_analysis/diabetes_for_trees_test.rds")

# Build XGBoost model ----
set.seed(1234)

xgb_rec <- 
    recipe(readmitted ~ ., data = train) %>% 
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_upsample(readmitted)

xgb_spec <-
    boost_tree(
        trees = 1000,
        tree_depth = tune(),
        min_n = tune(),
        loss_reduction = tune(),
        mtry = tune(),
        learn_rate = tune(), 
        sample_size = tune(), 
        stop_iter = tune()
    ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")

## grid search: space filling design ----

set.seed(1234)
xgb_grid <-
    grid_max_entropy(
        tree_depth(),
        min_n(),
        loss_reduction(),
        sample_size = sample_prop(),
        finalize(mtry(), train),
        learn_rate(),
        stop_iter(),
        size = 20
    )

## workflow

xgb_workflow <- 
    workflow() %>% 
    add_recipe(xgb_rec) %>% 
    add_model(xgb_spec)

## set up cross validation ----

set.seed(1234)
diabetes_folds <- vfold_cv(train, strata = readmitted, v = 5)

## tune models ----
cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makePSOCKcluster(cores)
doParallel::registerDoParallel(cl)

set.seed(1234)
start <- Sys.time()
xgb_res <-
    tune_grid(
        object = xgb_workflow,
        resamples = diabetes_folds,
        grid = xgb_grid,
        control = control_grid(
            allow_par = TRUE,
            parallel_over = "everything"
        )
    )
end <- Sys.time()
end - start

xgb_metrics <- 
    xgb_res %>% 
    collect_metrics()

xgb_metrics %>% 
    filter(.metric == "roc_auc") %>% 
    select(mean, mtry:sample_size) %>% 
    pivot_longer(mtry:sample_size, 
                 names_to = "parameter", 
                 values_to = "value") %>% 
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~ parameter, scales = "free_x") + 
    theme_bw()

# write_rds(xgb_res, "04_Modelling/tidymodels/xgb_res.rds")

top_5 <- show_best(xgb_res, "roc_auc")
top_5[1,]$.config %>% pluck(1)

best_auc <- select_best(xgb_res, "roc_auc")
     

# ----

final_xgb <- finalize_workflow(xgb_workflow, best_auc)

final_model_fit <- fit(final_xgb, train)

final_model_fit %>% 
    extract_fit_parsnip() %>% 
    vip(geom = "point", num_features = 10) +
    theme_bw()

# train data
predictions_train <- predict(final_model_fit, train, type = "prob") 
train_auc <-  pROC::auc(train$readmitted, predictions_train$.pred_Yes)
train_auc 

predictions <- predict(final_model_fit, test, type = "prob") 

roc_auc_result <- pROC::auc(test$readmitted, predictions$.pred_Yes)
roc_auc_result

write_rds(final_model_fit, "04_Modelling/tidymodels/XGB_model_1.rds")

parallel::stopCluster(cl)
