# Exploratory analysis prior to feature engineering ----

# Set Up ----
library(tidyverse)
library(recipes)

source("../../scripts/plot_probabilities_by_numeric_vals.R")
source("../../scripts/plot_proportions_by_level.R")

# load processed but unfiltered data
diabetes_train <- read_rds("00_Data/processed/for_analysis/diabetes_train.rds")

## Issue 1: Change in treatment intensity ----

## I would like to keep information about the number of diabetes drugs each patient is prescribed
## whilst also capturing more detail about changes to treatment

## My hope is to replace diabetes_meds and change with more informative predictors

## I can then drop these 2 features as well as details about individual drugs especially if these
## are not very informative

### Recode drugs numerically according to type of change ----
change_tbl <- 
    diabetes_train %>% 
    select(readmitted, change, a1cresult, metformin:metformin_pioglitazone) %>% 
    mutate(across(metformin:metformin_pioglitazone, 
                  ~ case_when(
                      .x == "Steady" ~ 0,
                      .x == "Up" ~ 1,
                      .x == "Down" ~ -1,
                      .x == "No" ~ 0
                  )))

### Aggregate scores ----
aggregated_change_tbl <- 
    change_tbl %>% 
    rowwise() %>%
    mutate(change_in_treatment_intensity = sum(c_across(metformin:metformin_pioglitazone))) %>%
    ungroup()

### Explore distribution of new feature ----
aggregated_change_tbl %>% 
    select(readmitted, change, change_in_treatment_intensity) %>% 
    ggplot(aes(change_in_treatment_intensity)) +
    geom_bar(fill = "steelblue", color = "grey30") +
    theme_bw() +
    ggtitle("Majority of patients have no change in treatment intensity")
    

### Compare aggregate values to 'change' feature ----
aggregated_change_tbl %>% 
    mutate(change_in_treatment_intensity = if_else(change_in_treatment_intensity != 0, "Ch", "No")) %>% 
    xtabs(data = ., ~ change + change_in_treatment_intensity)

## 9756 patients are classified as having a change in meds without this being apparent from the 
## individual drug data which raises questions around dropping 'change' as a feature 
## is this accurate or does it capture other information that may be useful

result <- aggregated_change_tbl %>% 
    filter(change_in_treatment_intensity == 0 & change == "Ch") %>% 
    select(-c(readmitted, change, a1cresult, change_in_treatment_intensity)) %>% 
    rowwise() %>% 
    mutate(any_non_zero = any(c_across(everything()) != 0)) %>% 
    ungroup()
  
paste0(round(100 - sum(result$any_non_zero == TRUE) / nrow(result) * 100, 2), 
       "% of these 9756 change instances have no evidence of any change in diabetes treatment")
    
### Explore relationship with response ----
aggregated_change_tbl %>%
    plot_probabilities_by_numeric_vals(change_in_treatment_intensity, readmitted, k = 3) +
    ggtitle("Non-linear, non-monotonic relationship")

### Explore interactions ----

#### a1cresult  ----
change_summary_tbl <- 
    aggregated_change_tbl %>% 
    select(change_in_treatment_intensity, change, a1cresult, readmitted) %>% 
    group_by(readmitted, a1cresult, change) %>% 
    summarise(
        n = n(),
        mean = mean(change_in_treatment_intensity))

change_summary_tbl %>% 
    ggplot(aes(a1cresult, mean)) +
    geom_col(aes(fill = readmitted), position = "dodge") +
    theme_bw() +
    labs(y = "Mean change in treatment",
         title = "Patients with HbA1c > 7 who are readmitted more likely to have had treatment reduced",
         subtitle = "This appears to demonstrate an important interaction") +
    tidyquant::scale_fill_tq()

### potential issue with reducing treatment in patients with sl elevated a1c levels
### patients with  sl elevated a1c level appear more likely to be readmitted early if
### their diabetes treatment is reduced.
### those with a1c > 8 more likely to have treatment increased, 

## plot proportion readmitted vs change in meds at different a1c levels 
aggregated_change_tbl %>% 
    filter(a1cresult == ">7") %>% 
    plot_probabilities_by_numeric_vals(change_in_treatment_intensity, readmitted, k = 3) +
    ggtitle("Significant negative linear relationship in patients with a1c >7")

aggregated_change_tbl %>% 
    filter(a1cresult == ">8") %>% 
    plot_probabilities_by_numeric_vals(change_in_treatment_intensity, readmitted, k = 3) +
    ggtitle("No significant relationship in patients with a1c >8")

aggregated_change_tbl %>% 
    filter(a1cresult == "None") %>% 
    plot_probabilities_by_numeric_vals(change_in_treatment_intensity, readmitted, k = 3) +
    ggtitle("Non linear significant relationship in patients with no a1c level")

aggregated_change_tbl %>% 
    filter(a1cresult == "Norm") %>% 
    plot_probabilities_by_numeric_vals(change_in_treatment_intensity, readmitted, k = 3) +
    ggtitle("Non-significant negative linear relationship in patients with normal a1c")

## changing diabetes treatment in those without a1c level appears to be associated with higher
## proportion of patients being readmitted early 

#### age ----

aggregated_change_tbl %>% 
    select(readmitted, change_in_treatment_intensity) %>% 
    mutate(age = diabetes_train$age) %>% 
    group_by(readmitted, age) %>% 
    summarise(mean_change = mean(change_in_treatment_intensity)) %>% 
    ungroup() %>% 
    ggplot(aes(age, mean_change, colour = readmitted, group = readmitted)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(title = "Patients under 50 readmitted early patients seem to have had treatment reduced",
         subtitle = "No difference in patients over 50")

## Issue 2: Measurement of treatment intensity ----

### Recode as binary features - taking / not-taking drug  ----

count_tbl <-
    diabetes_train %>%
    select(readmitted, diabetes_med, metformin:metformin_pioglitazone) %>% 
    mutate(across(metformin:metformin_pioglitazone, ~ if_else(.x == "No", 0, 1))) %>%
    select(where( ~ !all(. == 0)))

## several medications are combinations of those represented elsewhere 

### Recode combination meds ----
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

### Aggregate counts ----

med_columns <-
    count_tbl %>% 
    select(metformin:insulin) %>% 
    names()

aggregated_count_tbl <- 
    count_tbl %>%
    rowwise() %>%
    mutate(total_meds = sum(c_across(all_of(med_columns)))) %>%
    ungroup() %>% 
    select(readmitted, total_meds, everything())

aggregated_count_tbl

### Explore distribution of new feature ----

aggregated_count_tbl %>% 
    ggplot(aes(total_meds)) +
    geom_bar(fill = "skyblue", color = "grey30") +
    theme_bw()

### Compare aggregate values to 'diabetes_med' feature ----
aggregated_count_tbl %>%
    select(total_meds, diabetes_med) %>% 
    group_by(diabetes_med) %>% 
    count(total_meds)

## measure appears to accurately reflect reality and therefore ok to drop diabetes_med feature

### Explore relationship with response ----

aggregated_count_tbl %>%
    plot_probabilities_by_numeric_vals(
        predictor = total_meds,
        response = readmitted,
        success = "Yes",
        k = 3
    ) +
    ggtitle("Non-linear relationship between total_meds and probability of readmission")

# non linear relationship between total_meds and probability of readmission
# taking no meds - lower prob readmission (milder disease? less adverse effects?)
# taking 1 meds - higher prob readmission (under-treated?)
# taking 2 to 3 meds - average prob readmission (good glycaemic control?)
# 4 or more meds - probability falls although wider CI due to low frequency

### Engineer as categorical variable ----

categorised_count_tbl <- 
    aggregated_count_tbl %>% 
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
                levels = c("none", "mono", "dual", "triple", "quad_plus"))) %>% 
    select(-c(total_meds, diabetes_med, repaglinide:tolazamide))

categorised_count_tbl %>% 
    ggplot(aes(diabetes_therapy)) +
    geom_bar(fill = "steelblue", color = "grey30") +
    theme_bw() 

### Explore potential interactions ----

#### a1cresult -----

# statistical tests
anova(lm(aggregated_count_tbl$total_meds ~ diabetes_train$a1cresult)) 
chisq.test(table(categorised_count_tbl$diabetes_therapy, diabetes_train$a1cresult))

# visualisation: continuous vs categorical

categorised_count_tbl %>%
    mutate(a1cresult = diabetes_train$a1cresult) %>% 
    # filter(readmitted == "Yes") %>% 
    mutate(a1cresult = fct_relevel(a1cresult, c("None", "Norm", ">7", ">8")),
           diabetes_therapy = fct_rev(diabetes_therapy)) %>% 
    ggplot(aes(a1cresult)) +
    geom_bar(aes(fill = diabetes_therapy), position = "fill") +
    # geom_bar(color = "grey30", fill = "skyblue") +
    theme_bw() +
    facet_wrap(~ readmitted, scales = "free_y", ncol = 1) +
    labs(title = "Glycaemic control vs treatment intensity in patients readmitted early",
         subtitle = "No obvious differences") 

categorised_count_tbl %>% 
    mutate(a1cresult = diabetes_train$a1cresult) %>% 
    mutate(a1cresult = fct_relevel(a1cresult, c("None", "Norm", ">7", ">8")),
           diabetes_therapy = diabetes_therapy) %>% 
    select(diabetes_therapy, a1cresult, readmitted) %>% 
    group_by(readmitted, diabetes_therapy) %>% 
    count(a1cresult) %>% 
    mutate(prop = proportions(n)) %>% 
    ggplot() +
    geom_col(aes(diabetes_therapy, prop, fill = a1cresult), position = "dodge") +
    facet_wrap(~ readmitted, scales = "free_y") +
    theme_bw() +
    labs(title = "Glycaemic control vs treatment intensity by early readmission",
         subtitle = "No obvious differences")

# Patients on less medication more likely not to have a1c measured
# Patients on more meds appear to have higher values for A1c but may not be real due the above issue
# pattern same for readmitted and admitted so interaction not likely to be predictive

#### age ----

aggregated_count_tbl %>% 
    select(readmitted, total_meds) %>% 
    mutate(age = diabetes_train$age) %>% 
    group_by(readmitted, age) %>% 
    summarise(mean_meds = mean(total_meds)) %>% 
    ungroup() %>% 
    ggplot(aes(age, mean_meds, group = 1)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ readmitted) +
    theme_bw() +
    labs(title = "Number of diabetes meds associated with age - non-linear and non-monotonic",
         subtitle = "Similar pattern in admitted and non-admitted")

#### primary diabetes admission ----

# result %>% 
#     plot_proportions_by_level(predictor = diabetes_therapy, response = readmitted, success = "Yes")

# those on more increasing number diabetes meds more likely to have primary admission rel to diabets
# same pattern for those patients with and without early admission

# Issue 3: Variables correlated with time in hospital ----

potential_correlation_tbl <- 
    diabetes_train %>% 
    select(time_in_hospital, starts_with("num")) %>% 
    mutate(total_meds = aggregated_count_tbl$total_meds,
           change_in_treatment_intensity = aggregated_change_tbl$change_in_treatment_intensity)

potential_correlation_tbl %>% 
    cor() %>% 
    corrplot::corrplot(order = "FPC")





## time_in_hospital: distribution ----
diabetes_train %>% 
    ggplot(aes(time_in_hospital)) +
    geom_bar() +
    theme_bw() +
    labs(title = "Positively skewed")

## num_lab_procedures ----

### Distribution ----
diabetes_train %>% 
    ggplot(aes(num_lab_procedures)) +
    geom_bar() +
    theme_bw() +
    labs(title = "Normal but with significant spike at 1")

table(diabetes_train$num_lab_procedures)

### Relationship ----
cor.test(diabetes_train$num_lab_procedures, diabetes_train$time_in_hospital)
# moderate statistically significant positive correlation

diabetes_train %>% 
    ggplot(aes(time_in_hospital, num_lab_procedures)) +
    geom_point(alpha = 0.1, position = "jitter") +
    geom_smooth() +
    theme_bw() +
    ggtitle("Monotonic positive non linear relationship")

# with log transformation
diabetes_train %>% 
    ggplot(aes(log(time_in_hospital), num_lab_procedures)) +
    geom_point(alpha = 0.1, position = "jitter") +
    geom_smooth() +
    ggtitle("More linear with log transform of independent variable") +
    theme_bw()

### Combine ----
lab_procedure_rate <- 
    diabetes_train %>% 
    select(readmitted, time_in_hospital, num_lab_procedures) %>% 
    mutate(lab_procedure_rate = num_lab_procedures / time_in_hospital)

lab_procedure_rate %>% 
    ggplot(aes(lab_procedure_rate)) +
    geom_histogram(bins = 30) +
    theme_bw()

### lab_procedure_rate vs. early readmission
lab_procedure_rate %>%
    plot_probabilities_by_numeric_vals(predictor = lab_procedure_rate,
                                       response = readmitted,
                                       points = FALSE) +
    ggtitle("Non linear, non-monotonic relationship")

## num_procedures ----

### Distribution ----
diabetes_train %>% 
    ggplot(aes(num_procedures)) +
    geom_bar() +
    theme_bw() +
    labs(title = "Severely positively skewed")

### Relationship ----
cor.test(diabetes_train$num_procedures, diabetes_train$time_in_hospital)
# weak statistically significant positive correlation

diabetes_train %>% 
    ggplot(aes(time_in_hospital, num_procedures)) +
    geom_point(alpha = 0.1, position = "jitter") +
    geom_smooth() +
    theme_bw() +
    ggtitle("Non linear, non monotonic relationship")

### Combine ----
procedure_rate <- 
    diabetes_train %>% 
    select(readmitted, time_in_hospital, num_procedures) %>% 
    mutate(procedure_rate = num_procedures / time_in_hospital)

procedure_rate %>% 
    ggplot(aes(procedure_rate)) +
    geom_histogram(bins = 30) +
    theme_bw() +
    ggtitle("Severely right skewed")

### procedure_rate vs. early readmission
procedure_rate %>%
    plot_probabilities_by_numeric_vals(predictor = procedure_rate,
                                       response = readmitted,
                                       points = FALSE) +
    ggtitle("Non linear, non-monotonic relationship")

## num_medications ----

### Distribution ----
diabetes_train %>% 
    ggplot(aes(num_medications)) +
    geom_bar() +
    theme_bw() +
    labs(title = "Moderately positively skewed")

### Relationship ----
cor.test(diabetes_train$num_medications, diabetes_train$time_in_hospital)
# moderate to strong statistically significant positive correlation

diabetes_train %>% 
    ggplot(aes(time_in_hospital, num_medications)) +
    geom_point(alpha = 0.1, position = "jitter") +
    geom_smooth() +
    theme_bw() +
    ggtitle("Approximately linear relationship")

### Combine ----
medication_rate <- 
    diabetes_train %>% 
    select(readmitted, time_in_hospital, num_medications) %>% 
    mutate(medication_rate = num_medications / time_in_hospital)

medication_rate %>% 
    ggplot(aes(medication_rate)) +
    geom_histogram(bins = 30) +
    theme_bw() +
    ggtitle("Severely right skewed")

### medication_rate vs. early readmission
medication_rate %>% 
    ggplot(aes(time_in_hospital, medication_rate)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

cor(medication_rate$medication_rate, medication_rate$time_in_hospital)

medication_rate %>%
    plot_probabilities_by_numeric_vals(predictor = medication_rate,
                                       response = readmitted,
                                       points = FALSE) +
    ggtitle("Almost linear negative relationship")

### number meds vs early readmission
diabetes_train %>%
    plot_probabilities_by_numeric_vals(predictor = num_medications,
                                       response = readmitted,
                                       points = FALSE) +
    ggtitle("Non Linear relationship")


### explore medication changes vs hba1c and readmission


#### Issue 4: other interactions: diabetes admissions ----

# primary diabetes admissions: ie. diag_1_major == Diabetes

aggregated_count_tbl %>%
    select(total_meds, readmitted) %>%
    mutate(primary_diagnosis = diabetes_train$diag_1_major,
           readmitted = fct_rev(readmitted)) %>%
    mutate(diabetes_admission = if_else(str_detect(primary_diagnosis, "[D|d]iabetes"), "Yes", "No")) %>% 
    select(- primary_diagnosis) %>% 
    xtabs(~ diabetes_admission + readmitted, data = .) %>% 
    prop.test()

## statistically significant difference in readmissions for admissions primarily due to DM
## non diabetes ~ 8.1% vs. diabetes ~ 9.2% (chi_sq test p <0.05)


