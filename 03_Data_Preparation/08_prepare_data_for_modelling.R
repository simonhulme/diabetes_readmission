# DATA PREPARATION ----

library(tidyverse)
library(recipes)
library(here)

diabetes_train <- read_rds(here("00_Data/processed/for_analysis/diabetes_train.rds"))
diabetes_test <- read_rds(here("00_Data/processed/for_analysis/diabetes_test.rds"))

source("../../scripts/replace_na_factor.R")

# Filtered dataset ----

# remove near zero-variance features
# handle missing categorical data by creating new factor level
# remove uninformative features identified during exploratory data analysis
# recode categorical vars with high cardinality to capture maximum information with fewest levels

low_info_vars <-
    diabetes_train %>%
    select(
        c(
            race,
            gender,
            num_procedures,
            a1cresult,
            nateglinide:acetohexamide,
            glyburide:citoglipton,
            glyburide_metformin:change,
            diag_1_chapter,
            diag_2_chapter,
            diag_3_chapter
        )
    ) %>%
    names()

filter_recipe <-
    diabetes_train %>%
    recipe(readmitted ~ .) %>%
    step_rm(all_of(low_info_vars)) %>% 
    step_mutate_at(all_nominal_predictors(), fn = replace_na_factor) %>%
    step_zv(all_predictors()) %>%
    step_other(medical_specialty, threshold = 0.0005) %>%
    step_other(admission_source, threshold = 0.015) %>%
    step_other(admission_type, threshold = 0.06) %>%
    step_other(diag_1_major, threshold = 0.0025) %>%
    step_other(diag_2_major, threshold = 0.0025) %>%
    step_other(diag_3_major, threshold = 0.002) %>%
    prep()

diabetes_filtered_train <- bake(filter_recipe, new_data = diabetes_train)
diabetes_filtered_test  <- bake(filter_recipe, new_data = diabetes_test)

write_rds(diabetes_filtered_train, "00_Data/processed/for_analysis/diabetes_filtered_train.rds")
write_rds(diabetes_filtered_test, "00_Data/processed/for_analysis/diabetes_filtered_test.rds")