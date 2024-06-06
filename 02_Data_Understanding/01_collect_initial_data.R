# COLLECT INITIAL RAW DATA ----

# Set-Up ----

# Load libraries
library(tidyverse)
library(icd.data)

# Source functions
source("../../scripts/extract_zipped_files_via_url.R")

# Diabetes and definitions ----

url <- 
    "https://archive.ics.uci.edu/static/public/296/diabetes+130-us+hospitals+for+years+1999-2008.zip"

extract_zipped_files_via_url(url) %>% 
    import_extracted_csv_files()

write_csv(x = diabetic_data, file = "00_Data/raw/diabetic_data.csv")
write_csv(x = IDS_mapping, file = "00_Data/raw/IDS_mapping.csv")

# ICD 9 codes and definitions ----

icd_9_definitions <- 
    icd9cm_hierarchy

write_csv(x = icd_9_definitions, file = "00_Data/raw/icd_9_definitions.csv")