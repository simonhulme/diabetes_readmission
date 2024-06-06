## model performance so far is poor so need to revisit data and review feature engineering

## maybe add this to initial feature engineering 

library(tidyverse)
library(tidymodels)

diabetes_train <- read_rds("00_Data/processed/for_analysis/diabetes_train.rds")
diabetes_test  <- read_rds("00_Data/processed/for_analysis/diabetes_test.rds")

source("../../scripts/replace_na_factor.R")

## Feature Engineering ----

### Medication features ----

change_tbl <- 
    diabetes_train %>% 
    select(metformin:metformin_pioglitazone) %>% 
    mutate(across(metformin:metformin_pioglitazone, 
                  ~ case_when(
                      .x == "Steady" ~ 0,
                      .x == "Up" ~ 1,
                      .x == "Down" ~ -1,
                      .x == "No" ~ 0
                  )))

aggregated_change_tbl <- 
    change_tbl %>% 
    rowwise() %>%
    mutate(change_in_treatment_intensity = sum(c_across(metformin:metformin_pioglitazone))) %>%
    ungroup()

count_tbl <-
    diabetes_train %>%
    select(metformin:metformin_pioglitazone) %>% 
    mutate(across(metformin:metformin_pioglitazone, ~ if_else(.x == "No", 0, 1))) %>%
    select(where( ~ !all(. == 0)))

recode_combination_meds <- function(data, med_name) {
    # Split combination_med string into individual medications
    meds <- str_split(med_name, "_", simplify = TRUE)
    med_1 <- meds[1]
    med_2 <- meds[2]
    
    # Convert med_1 and med_2 into symbols for tidy evaluation
    med_1_sym <- sym(med_1)
    med_2_sym <- sym(med_2)
    med_name_sym <- sym(med_name)
    
    # Use mutate to create new columns based on the combination_med column and remove the combination column
    data <- data %>%
        mutate(
            !!med_1_sym := case_when(
                !!med_1_sym == 0 & !!med_name_sym == 1 ~ 1,
                !!med_1_sym == 0 & !!med_name_sym == 0 ~ 0,
                !!med_1_sym == 1 ~ 1
            ),
            !!med_2_sym := case_when(
                !!med_2_sym == 0 & !!med_name_sym == 1 ~ 1,
                !!med_2_sym == 0 & !!med_name_sym == 0 ~ 0,
                !!med_2_sym == 1 ~ 1
            ))
    
    data <-
        data %>%
        select(-!!med_name_sym)
    
    return(data)
}

combination_meds <- 
    count_tbl %>% 
    select(contains("metformin") & contains ("_")) %>% 
    names()

count_tbl <-
    reduce(combination_meds, recode_combination_meds, .init = count_tbl)

med_columns <-
    count_tbl %>% 
    select(metformin:insulin) %>% 
    names()

aggregated_count_tbl <- 
    count_tbl %>%
    rowwise() %>%
    mutate(total_meds = sum(c_across(all_of(med_columns)))) %>%
    ungroup()

## combine with main data ----

diabetes_train_proc <- 
    diabetes_train %>% 
    select(-c(repaglinide:citoglipton, glyburide_metformin:metformin_pioglitazone, diabetes_med)) %>% 
    mutate(change_in_treatment_intensity = aggregated_change_tbl$change_in_treatment_intensity) %>% 
    mutate(treatment_intensity = aggregated_count_tbl$total_meds) %>% 
    mutate(across(contains("num_"), ~ .x / time_in_hospital)) %>% 
    select(readmitted , everything())

## further pre-processing ----

filter_recipe <-
    diabetes_train_proc %>%
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

train_tbl <- bake(filter_recipe, diabetes_train_proc)

## test data
change_tbl <- 
    diabetes_test %>% 
    select(metformin:metformin_pioglitazone) %>% 
    mutate(across(metformin:metformin_pioglitazone, 
                  ~ case_when(
                      .x == "Steady" ~ 0,
                      .x == "Up" ~ 1,
                      .x == "Down" ~ -1,
                      .x == "No" ~ 0
                  )))

aggregated_change_tbl <- 
    change_tbl %>% 
    rowwise() %>%
    mutate(change_in_treatment_intensity = sum(c_across(metformin:metformin_pioglitazone))) %>%
    ungroup()

count_tbl <-
    diabetes_test %>%
    select(metformin:metformin_pioglitazone) %>% 
    mutate(across(metformin:metformin_pioglitazone, ~ if_else(.x == "No", 0, 1))) %>%
    select(where( ~ !all(. == 0)))

combination_meds <- 
    count_tbl %>% 
    select(contains("metformin") & contains ("_")) %>% 
    names()

count_tbl <-
    reduce(combination_meds, recode_combination_meds, .init = count_tbl)

med_columns <-
    count_tbl %>% 
    select(metformin:insulin) %>% 
    names()

aggregated_count_tbl <- 
    count_tbl %>%
    rowwise() %>%
    mutate(total_meds = sum(c_across(all_of(med_columns)))) %>%
    ungroup()

## combine with main data ----

diabetes_test_proc <- 
    diabetes_test %>% 
    select(-c(repaglinide:citoglipton, glyburide_metformin:metformin_pioglitazone, diabetes_med)) %>% 
    mutate(change_in_treatment_intensity = aggregated_change_tbl$change_in_treatment_intensity) %>% 
    mutate(treatment_intensity = aggregated_count_tbl$total_meds) %>% 
    mutate(across(contains("num_"), ~ .x / time_in_hospital)) %>% 
    select(readmitted , everything())

## further pre-processing ----
test_tbl <- bake(filter_recipe, diabetes_test_proc)

## save data

write_rds(train_tbl, "00_Data/processed/for_analysis/diabetes_for_trees_train.rds")
write_rds(test_tbl, "00_Data/processed/for_analysis/diabetes_for_trees_test.rds")