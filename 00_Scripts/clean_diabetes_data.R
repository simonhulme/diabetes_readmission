# Data processing prior to splitting into training and test sets

# Binary response, replace numeric keys, create factors, make missing values explicit, 
# ensure single observations per patient 

# Libraries
library(tidyverse)

clean_diabetes_data <- function(diabetes_data, definitions, icd_9_definitions) {
    
    # HELPER FUNCTIONS ----
    
    create_definitions_list <- function(definitions) {
        # get data into format suitable for joining with diabetes data
        definitions %>%
            names() %>%
            as_tibble() %>%
            pivot_wider(names_from = value) %>%
            rbind(definitions) %>%
            setNames(c("X1", "X2")) %>% 
            
            # create column where each row contains feature name
            mutate(feature = if_else(str_detect(X1, "[:digit:]"), NA, X1)) %>%
            fill(feature) %>%
            
            # remove rows containing headers and NAs
            filter(feature != X1) %>%
            
            # create named data frames stored in a list and format to aid merge with diabetes data
            split(.$feature) %>%
            map( ~ select(., -feature)) %>%
            map( ~ mutate(., X1 = as.numeric(X1))) %>%
            map( ~ mutate(., X2 = as_factor(X2))) %>%
            map2(.x = ., .y = names(.), .f = ~ set_names(.x, .y, paste0(.y, "_value")))
    }
    
    merge_definitions_with_diabetes_data <- function(definitions_list, diabetes_data) {
        # merge definitions with diabetes data using reduce to iteratively left join list 
        diabetes_data %>%
            list() %>%
            append(definitions_list, after = 1) %>%
            reduce(left_join) %>%
            select(-!!names(definitions_list)) %>%
            set_names(str_replace_all(names(.), "_id_value", ""))
    }
    
    merge_icd_codes <- function(diabetic_data, icd_9_definitions) {
        
        # extract icd9 codes and definitions
        icd_9_definitions <- 
            icd_9_definitions %>% 
            select(three_digit, major, chapter) %>% 
            distinct()
        
        # ensure formatting of values matches format in ICD-9 definitions table
        data <- 
            diabetic_data %>%
            # ensure values match format in ICD-9 definitions table
            mutate(across(contains("diag_"), ~ na_if(.x, "?"))) %>% 
            mutate(across(contains("diag_"), ~ str_sub(.x, 1, 3) %>% 
                              str_pad(width = 3, side = "left", pad = "0"))) 
        
        # handle missing data by moving NA down from diag 1->2->3
        data <- 
            data %>% 
            mutate(diag_2 = if_else(is.na(diag_2), diag_3, diag_2)) %>%
            mutate(diag_1 = if_else(is.na(diag_1), diag_2, diag_1)) %>%
            mutate(diag_2 = if_else(diag_1 == diag_2, diag_3, diag_2)) %>% 
            mutate(diag_3 = ifelse(diag_2 == diag_3, NA, diag_3))
        
        data %>% 
            # merge data and rename cols
            left_join(icd_9_definitions, by = c("diag_1" = "three_digit")) %>% 
            rename("diag_1_major" = "major", "diag_1_chapter" = "chapter") %>% 
            left_join(icd_9_definitions, by = c("diag_2" = "three_digit")) %>% 
            rename("diag_2_major" = "major", "diag_2_chapter" = "chapter") %>% 
            left_join(icd_9_definitions, by = c("diag_3" = "three_digit")) %>% 
            rename("diag_3_major" = "major", "diag_3_chapter" = "chapter") %>% 
            
            # tidy up
            select(-c(diag_1, diag_2, diag_3))
    }
    
    make_NAs_explicit <- function(data) {
        data %>%
            mutate(across(where(is.character), ~ na_if(.x, "?"))) %>%
            mutate(across(where(is.character), ~ na_if(.x, "Unknown/Invalid")))
    }
    
    remove_duplicate_pts <- function(data) {
        # multiple observations for some patients so only keep first one for each
        data %>%
            group_by(patient_nbr) %>%
            slice_min(order_by = encounter_id, n = 1) %>%
            ungroup()
    }
    
    select_population <- function(data) {
        # adults / community discharge / alive / non-psychiatric
        data %>% 
            filter(!(age %in% c("[0-10)", "[10-20)"))) %>%
            filter(!(str_detect(discharge_disposition, "Expired") |
                         str_detect(discharge_disposition, "Hospice"))) %>%
            filter(!(str_detect(medical_specialty, "Psychiatry") |
                         str_detect(medical_specialty, "Pediatrics"))) %>%
            filter(
                str_detect(discharge_disposition, "home") |
                    str_detect(discharge_disposition, "SNF") |
                    str_detect(discharge_disposition, "ICF") |
                    str_detect(discharge_disposition, "long term care")
            ) 
    }
    
    create_binary_response <- function(data) {
        data %>% 
            mutate(readmitted = if_else(readmitted == "<30", "Yes", "No"))
    }
    
    convert_to_factors <- function (data) {
        data %>% 
            mutate(across(where(is.character), as.factor))
    }
    
    drop_unused_levels <- function (data) {
        data %>% 
            mutate(discharge_disposition = fct_drop(discharge_disposition),
                   age = fct_drop(age),
                   medical_specialty = fct_drop(medical_specialty))
    }
    
    # FUNCTION BODY ----
    output <-
        create_definitions_list(definitions) %>% 
        merge_definitions_with_diabetes_data(diabetes_data) %>%
        merge_icd_codes(icd_9_definitions) %>%
        remove_duplicate_pts() %>% 
        select_population() %>% 
        make_NAs_explicit() %>%
        create_binary_response() %>% 
        convert_to_factors() %>%
        drop_unused_levels() %>%
        janitor::clean_names()
    
    return(output)
}