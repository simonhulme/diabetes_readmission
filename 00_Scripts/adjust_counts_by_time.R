# adjust count features by time in hospital ----

library(tidyverse)

adjust_counts_by_time <- function(data) {
    
    count_features <-
        data %>% 
        select(starts_with("num_")) %>% 
        names()
    
    count_features
    
    data %>% 
        mutate(across(all_of(count_features), ~ .x / time_in_hospital))
}