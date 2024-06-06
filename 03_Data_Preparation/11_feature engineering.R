# FEATURE ENGINEERING ----

library(tidyverse)
library(recipes)

diabetes_train <- read_rds("00_Data/processed/for_analysis/diabetes_train.rds")
diabetes_test  <- read_rds("00_Data/processed/for_analysis/diabetes_test.rds")

source("00_Scripts/feature_engineer_diabetes_meds.R")
source("00_Scripts/adjust_counts_by_time.R")
source("../../scripts/replace_na_factor.R")

engineered_train <- 
    diabetes_train %>% 
    feature_engineer_diabetes_meds() %>% 
    adjust_counts_by_time()

engineered_test <- 
    diabetes_test %>% 
    feature_engineer_diabetes_meds() %>% 
    adjust_counts_by_time()

# Filter engineered data ----

# remove zero-variance features
# remove redundant features
# handle missing categorical data by creating new factor level
# recode categorical vars with high cardinality to capture maximum information with fewest levels

filter_recipe <-
    engineered_train %>%
    recipe(readmitted ~ .) %>% 
    step_mutate_at(all_nominal_predictors(), fn = replace_na_factor) %>%
    step_zv(all_predictors()) %>% 
    step_rm(contains("chapter")) %>% 
    step_other(medical_specialty, threshold = 0.0005) %>%
    step_other(admission_source, threshold = 0.015) %>%
    step_other(admission_type, threshold = 0.06) %>%
    step_other(diag_1_major, threshold = 0.0025) %>%
    step_other(diag_2_major, threshold = 0.0025) %>%
    step_other(diag_3_major, threshold = 0.002) %>%
    prep()

diabetes_engineered_filtered_train <- bake(filter_recipe, new_data = engineered_train)
diabetes_engineered_filtered_test  <- bake(filter_recipe, new_data = engineered_test)

write_rds(
    diabetes_engineered_filtered_train,
    "00_Data/processed/for_analysis/diabetes_engineered_filtered_train.rds"
)

write_rds(
    diabetes_engineered_filtered_test,
    "00_Data/processed/for_analysis/diabetes_engineered_filtered_test.rds"
)
