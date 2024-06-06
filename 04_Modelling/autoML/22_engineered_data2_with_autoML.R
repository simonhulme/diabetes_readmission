# Load libraries
library(tidyverse)
library(h2o)

# Initialise cluster
h2o.init()
h2o.removeAll()

source("00_Scripts/save_h2o_model.R")

# Load data
diabetes_train <- read_rds("00_Data/processed/for_analysis/diabetes_engineered_train.rds")

# Run autoML algorithm  ----

## Train models 

diabetes_train_h2o <- as.h2o(diabetes_train, destination_frame = "train")
response           <- "readmitted"
predictors         <- setdiff(names(diabetes_train_h2o), response)

# run for longer than previously
diabetes_automl <-
    h2o.automl(
        x = predictors,
        y = response,
        training_frame = diabetes_train_h2o,
        nfolds = 5,
        max_runtime_secs = 1200,
        balance_classes = TRUE,
        seed = 1234
    )

best_fit_by_model_type <- 
    diabetes_automl@leaderboard %>%
    as_tibble() %>% 
    mutate(model_type = str_split_i(model_id , "_", 1)) %>% 
    group_by(model_type) %>% 
    slice_max(auc, n = 1) %>% 
    ungroup() %>% 
    mutate(filename = paste0("engineered_autoML_", model_type)) %>% 
    arrange(desc(auc)) %>% 
    select(model_type, filename, model_id, auc, logloss) %>% 
    mutate(model = map(model_id, ~ h2o.getModel(.x)))

# no real improvement over previous models - GLM and XGBoost are best non stacked models

# focus further modelling on these 2 models

# questions: how does feature selection impact glm - intrinsic and extrinsic 
#            how does altering constraints on number of trees impact XG Boost.

## start with autoML based hyperparameter tuning before considering manual approach:

## tune XGBoost using autoML ----

h2o.removeAll(retained_elements = "train")

autoML_XGB <-
    h2o.automl(
        x = predictors,
        y = response,
        training_frame = diabetes_train_h2o,
        nfolds = 10,
        balance_classes = TRUE,
        max_runtime_secs = 300, 
        max_after_balance_size = 10,
        seed = 1234,
        include_algos = "XGBoost"
    )



