# DATA CLEANING ----

# Set-Up ----

# Load libraries
library(tidyverse)

# Import data 
diabetes_data     <- read_csv("00_Data/raw/diabetic_data.csv")
definitions       <- read_csv("00_Data/raw/IDS_mapping.csv")
icd_9_definitions <- read_csv("00_Data/raw/icd_9_definitions.csv")

# Source functions
source("00_Scripts/clean_diabetes_data.R")

# Process data prior to initial exploration ----

## merge IDS mapping and icd9 data with main dataset 
## make NAs explicit
## move NAs for diagnoses features to lowest level - 1->2->3
## filter for first admission for adult patient
## filter for discharge to community (excluding end-of-life care) 
## exclude psychiatric admissions
## manage categorical data: convert to binary response, create factors and drop unused levels
## clean non syntactic variable names

processed_data <-
    clean_diabetes_data(
        diabetes_data = diabetes_data,
        definitions = definitions,
        icd_9_definitions = icd_9_definitions
    )

write_rds(processed_data, "00_Data/processed/diabetes_processed.rds")