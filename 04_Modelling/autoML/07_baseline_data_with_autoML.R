# BASELINE MODELS USING AUTO ML ----

# Set Up ----
# Load libraries
library(tidyverse)
library(h2o)

source("00_Scripts/save_h2o_model.R")

# Initialise cluster
h2o.init()
h2o.removeAll()

# Load data
diabetes_train <- read_rds("00_Data/processed/for_analysis/diabetes_train.rds")

# Run autoML algorithm  ----

diabetes_train_h2o <- as.h2o(diabetes_train)
response           <- "readmitted"
predictors         <- setdiff(names(diabetes_train_h2o), response)

diabetes_automl <-
    h2o.automl(
        x = predictors,
        y = response,
        training_frame = diabetes_train_h2o,
        nfolds = 10,
        max_runtime_secs = 600,
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
    mutate(filename = paste0("autoML_", model_type)) %>% 
    arrange(desc(auc)) %>% 
    select(model_type, filename, model_id, auc, logloss) %>% 
    mutate(model = map(model_id, ~ h2o.getModel(.x)))

best_fit_by_model_type

# Save best performing models (by type)  ----

map2(.x = best_fit_by_model_type$model,
     .y = best_fit_by_model_type$filename,
     save_h2o_model,
     path = "04_Modelling/models/baseline_autoML_models/")