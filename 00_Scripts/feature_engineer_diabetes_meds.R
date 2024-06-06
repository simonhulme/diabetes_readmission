# Combine individual medications into single indicator ----

library(tidyverse)

calculate_total_meds <- function(data) {
    
    # HELPER FUNCTIONS:
    
    # recode each med as binary variable: 0 - not taking, 1- taking
    binary_recode_meds <- function(data) {
        output <-
            data %>%
            mutate(across(
                metformin:metformin_pioglitazone,
                ~ if_else(.x == "No", 0, 1)
            )) 
        return(output)
    }
    
    # remove combination meds
    remove_combination_meds <- function(data) {
        
        combination_meds <- 
            data %>%
            select(contains("metformin") & contains ("_")) %>%
            names()
        
        extract_data_from_combination_meds <- function(data, combination_med) {
            
            meds <- str_split(combination_med, "_", simplify = TRUE)
            med_1 <- meds[1]
            med_2 <- meds[2]
            
            # Convert med_1 and med_2 into symbols for tidy evaluation
            med_1_sym <- sym(med_1)
            med_2_sym <- sym(med_2)
            combination_med_sym <- sym(combination_med)
            
            # extract information from combination med column and remove it
            output <- data %>%
                mutate(
                    !!med_1_sym := case_when(
                        !!med_1_sym == 0 & !!combination_med_sym == 1 ~ 1,
                        !!med_1_sym == 0 &
                            !!combination_med_sym == 0 ~ 0,
                        !!med_1_sym == 1 ~ 1
                    ),
                    !!med_2_sym := case_when(
                        !!med_2_sym == 0 & !!combination_med_sym == 1 ~ 1,
                        !!med_2_sym == 0 &
                            !!combination_med_sym == 0 ~ 0,
                        !!med_2_sym == 1 ~ 1
                    )
                )
            
            output <-
                output %>%
                select(-!!combination_med_sym)
            
            return(output)
        }
        
        reduce(combination_meds, extract_data_from_combination_meds, .init = data)
        
    }
    
    convert_totals_into_therapy_description <- function(data) {
        
        output <- 
            data %>% 
            mutate(
                diabetes_therapy =
                    case_when(
                        total_meds == 0 ~ "none",
                        total_meds == 1 ~ "mono",
                        total_meds == 2 ~ "dual",
                        total_meds == 3 ~ "triple",
                        total_meds >= 4 ~ "quad_plus"
                    ),
                diabetes_therapy =
                    fct(
                        diabetes_therapy,
                        levels = c("none", "mono", "dual", "triple", "quad_plus")
                    ))
        
        return(output)
    }
    
    add_totals <- function(data) {
        
        med_columns <-
            data %>%
            select(metformin:insulin) %>%
            names()
        
        output <- 
            data %>% 
            rowwise() %>%
            mutate(total_meds = sum(c_across(all_of(med_columns)))) %>%
            ungroup()
        
        return(output)
    }
    
    # FUNCTION BODY
    
    output <-
        data %>%
        binary_recode_meds() %>% 
        remove_combination_meds() %>% 
        add_totals() %>% 
        convert_totals_into_therapy_description() %>% 
        select(-c(total_meds, diabetes_med, nateglinide:acetohexamide, glyburide:citoglipton, glimepiride_pioglitazone))
    
    return(output)
} 

evaluat

