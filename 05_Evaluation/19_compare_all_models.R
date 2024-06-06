library(tidyverse)
library(h2o)

h2o.init()
h2o.removeAll()

model_paths <- 
    fs::dir_ls(c("04_Modelling/models/baseline_autoML_models/",
                 "04_Modelling/models/baseline_tuned_models/",
                 "04_Modelling/models/filtered_autoML_models/",
                 "04_Modelling/models/filtered_tuned_models/",
                 "04_Modelling/models/engineered_autoML_models/",
                 "04_Modelling/models/engineered_tuned_models/")) %>% 
    as.character()

model_names <- 
    model_paths %>% 
    str_split_i("/", i = 4) 

all_models <- 
    tibble(model_names, model_paths) %>% 
    mutate(models = map(model_paths, ~ h2o.loadModel(.x)))

model_comparison <- 
    all_models %>% 
    mutate(auc     = map(models, ~ h2o.auc(.x, train = TRUE, xval = TRUE)),
           logloss = map(models, ~ h2o.logloss(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(c(auc, logloss), names_sep = "_") %>% 
    arrange(-auc_xval) %>% 
    select(model_names, contains("xval")) %>% 
    separate(model_names, into = c("data", "tuning", "model_type"), sep = "_", remove = FALSE)

model_comparison

model_comparison %>% 
    ggplot(aes(auc_xval, model_type, colour = data, shape = tuning)) +
    geom_point(size = 3) +
    # facet_wrap(~ tuning, ncol = 1) +
    theme_bw() +
    tidyquant::scale_color_tq()

## Choose selection of models for further exploration

selected_models_for_evaluation <- 
    all_models %>% 
    filter(
        model_names %in% c(
            "baseline_autoML_StackedEnsemble",
            "baseline_autoML_GLM",
            "engineered_autoML_GLM",
            "engineered_autoML_StackedEnsemble",
            "filtered_autoML_StackedEnsemble",
            "engineered_tuned_GBM",
            "engineered_tuned_XGB",
            "engineered_tuned_DRF"
        )) %>%  
    separate(model_names, into = c("data", "tuning", "model_type"), sep = "_", remove = FALSE) %>% 
    select((-c(tuning, model_type))) %>% 
    mutate(data = fct(data))

# load testing data

baseline_test   <- read_rds("00_Data/processed/for_analysis/diabetes_test.rds")
filtered_test   <- read_rds("00_Data/processed/for_analysis/diabetes_filtered_test.rds")
engineered_test <- read_rds("00_Data/processed/for_analysis/diabetes_engineered_filtered_test.rds")

test_data <- list(baseline = baseline_test, filtered = filtered_test, engineered = engineered_test)

models <- 
    selected_models_for_evaluation %>% 
    group_split(data)

baseline_models <- 
    models[[1]] %>% 
    mutate(test_data = list(baseline_test)) %>% 
    mutate(performance = map2(models, test_data, ~h2o.performance(model = .x, newdata = as.h2o(.y)))) %>% 
    mutate(auc = map(models, ~ h2o.auc(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(auc, names_sep = "_") %>% 
    mutate(auc_test = map_dbl(performance, h2o.auc))

baseline_models

filtered_models <- 
    models[[2]] %>% 
    mutate(test_data = list(filtered_test)) %>% 
    mutate(performance = map2(models, test_data, ~h2o.performance(model = .x, newdata = as.h2o(.y)))) %>% 
    mutate(auc = map(models, ~ h2o.auc(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(auc, names_sep = "_") %>% 
    mutate(auc_test = map_dbl(performance, h2o.auc))

filtered_models

engineered_models <- 
    models[[3]] %>% 
    mutate(test_data = list(engineered_test)) %>% 
    mutate(performance = map2(models, test_data, ~h2o.performance(model = .x, newdata = as.h2o(.y)))) %>% 
    mutate(auc = map(models, ~ h2o.auc(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(auc, names_sep = "_") %>% 
    mutate(auc_test = map_dbl(performance, h2o.auc))

engineered_models

full_evaluation <- 
    reduce(list(baseline_models, filtered_models, engineered_models), rbind) %>% 
    arrange(desc(auc_test))

full_evaluation

## full evaluation on engineered_models

stacked_ensemble <- 
    all_models %>% 
    filter(model_names == "engineered_autoML_StackedEnsemble") %>% 
    pull(models) %>% 
    pluck(1)

h2o.saveModel(stacked_ensemble,
              path = "04_Modelling/models/final_candidates",
              filename = "stacked_ensemble")

glm <- 
    all_models %>% 
    filter(model_names == "engineered_autoML_GLM") %>% 
    pull(models) %>% 
    pluck(1)

h2o.saveModel(glm,
              path = "04_Modelling/models/final_candidates",
              filename = "glm")

xg_boost <- 
    all_models %>% 
    filter(model_names == "engineered_tuned_XGB") %>% 
    pull(models) %>% 
    pluck(1)

h2o.saveModel(xg_boost,
              path = "04_Modelling/models/final_candidates",
              filename = "xg_boost")

gbm <- 
    all_models %>% 
    filter(model_names == "engineered_tuned_GBM") %>% 
    pull(models) %>% 
    pluck(1)

h2o.saveModel(gbm,
              path = "04_Modelling/models/final_candidates",
              filename = "gbm")

random_forest <- 
    all_models %>% 
    filter(model_names == "engineered_tuned_DRF") %>% 
    pull(models) %>% 
    pluck(1)

h2o.saveModel(random_forest,
              path = "04_Modelling/models/final_candidates",
              filename = "random_forest")