# Preparation for analysis ----

# Set-Up ----

# Load libraries
library(tidyverse)
library(rsample)

# Import data
diabetes_data <- read_rds("00_Data/processed/diabetes_processed.rds")

# Process data for analysis ----

# Most of weight data missing so drop feature along with id variables
# also drop payer code due to missing data ++ and not comparable to UK data

diabetes_final <- 
    diabetes_data %>% 
    select(-c(weight, encounter_id, patient_nbr, payer_code))

write_rds(diabetes_final, "00_Data/processed/diabetes_final.rds")

# Split into train and test sets
set.seed(42)

train_test_split   <- initial_split(diabetes_final, prop = 0.75, strata = readmitted)
diabetes_train     <- training(train_test_split)
diabetes_test      <- testing(train_test_split)

# save data ----
write_rds(x = diabetes_train , file = "00_Data/processed/for_analysis/diabetes_train.rds")
write_rds(x = diabetes_test,   file = "00_Data/processed/for_analysis/diabetes_test.rds")