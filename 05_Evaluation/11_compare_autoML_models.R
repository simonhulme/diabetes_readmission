library(tidyverse)
library(h2o)

h2o.init()

model_paths <- 
    fs::dir_ls(c("04_Modelling/baseline_autoML_models/", 
                 "04_Modelling/basic_processed_data/",
                 "04_Modelling/filtered_data/")) %>% 
    as.character()

model_names <- 
    model_paths %>% 
    str_split_i("/", i = 3) 

all_models <- 
    tibble(model_names, model_paths) %>% 
    mutate(models = map(model_paths, ~ h2o.loadModel(.x)))

all_models %>% 
    mutate(auc     = map(models, ~ h2o.auc(.x, train = TRUE, xval = TRUE)),
           logloss = map(models, ~ h2o.logloss(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(c(auc, logloss), names_sep = "_") %>% 
    arrange(-auc_xval) %>% 
    select(model_names, contains("xval"))
