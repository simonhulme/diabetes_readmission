# Candidate model evaluation ----

library(tidyverse)
library(h2o)

h2o.init()
h2o.removeAll()

# Load Models ----

glm  <- h2o.loadModel("04_Modelling/models/final_candidates/glm")
random_forest <- h2o.loadModel("04_Modelling/models/final_candidates/random_forest")
gbm <- h2o.loadModel("04_Modelling/models/final_candidates/gbm")
xg_boost <- h2o.loadModel("04_Modelling/models/final_candidates/xg_boost")

# Load Test Data ----

test_data <- read_rds("00_Data/processed/for_analysis/diabetes_engineered_filtered_test.rds")
test_h2o <- as.h2o(test_data)

## 
test_data %>% 
    count(readmitted) %>% 
    mutate(prop = proportions(n))

# First Evaluation ----

performance_h2o <- h2o.performance(xg_boost, newdata = test_h2o)

h2o.metric(performance_h2o) %>% 
    as_tibble() %>% 
    select(threshold, precision, recall) %>% 
    pivot_longer(cols = - threshold) %>% 
    ggplot(aes(threshold, value, color = name)) +
    geom_line() +
    geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1"), linetype = 2) +
    theme_bw()

# Performance of multiple models ----

multiple_models <-
    enframe(
        list(
            gbm = gbm,
            glm = glm,
            random_forest = random_forest,
            xg_boost = xg_boost
        ),
        value = "model"
    )

performance_tbl <- 
    multiple_models %>% 
    mutate(
        performance = map(model, ~ h2o.performance(.x, newdata = test_h2o)),
        auc         = map(model, ~ h2o.auc(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(auc, names_sep = "_") %>% 
    mutate(
        auc_test    = map_dbl(performance, h2o.auc),
        metrics     = map(performance, ~ h2o.metric(.x) %>% as_tibble),
        metrics     = map(metrics, ~ select(.x, tpr, fpr, precision, recall))
    ) %>% 
    unnest_longer(metrics) %>% 
    mutate(name = fct(name))

summary_auc <- 
    multiple_models %>% 
    mutate(
        performance = map(model, ~ h2o.performance(.x, newdata = test_h2o)),
        auc         = map(model, ~ h2o.auc(.x, train = TRUE, xval = TRUE))) %>% 
    unnest_wider(auc, names_sep = "_") %>% 
    mutate(auc_test    = map_dbl(performance, h2o.auc))

summary_auc_long <- 
    summary_auc %>%
    select(name, starts_with("auc_")) %>% 
    mutate(name = fct_reorder(name, auc_test)) %>% 
    pivot_longer(
        cols = starts_with("auc_"),
        names_to = "data_type",
        values_to = "auc_value"
    ) 

summary_auc_long %>% 
    ggplot(aes(auc_value, name, color = data_type)) +
    geom_point(size = 2) + 
    theme_bw()

## ROC curves ----

performance_tbl %>% 
    mutate(auc_test = fct(as.character(round(auc_test, 4)))) %>% 
    ggplot(aes(metrics$fpr, metrics$tpr, color = name, linetype = auc_test)) +
    geom_line(linewidth = 1) +
    geom_abline() +
    labs(x = "False Positive Rate - FPR", 
         y = "True Positive Rate - TPR") +
    theme_bw()

## Precision:Recall curves ----

performance_tbl %>% 
    ggplot(aes(metrics$recall, metrics$precision, color = name)) +
    geom_smooth(se = FALSE) +
    labs(x = "Recall (TP / (TP + FN))", 
         y = "Precision (TP / (TP + FP)") +
    theme_bw()

# Deep dive into high performing model ----

## Gain and Lift Charts ----

## Start with XG Boost 

## Gain

gain_lift_tbl <- 
    performance_h2o %>% 
    h2o.gains_lift() %>% 
    as_tibble() %>% 
    select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)

gain_transformed_tbl <- 
    gain_lift_tbl %>% 
    select(- cumulative_lift) %>% 
    mutate(baseline = cumulative_data_fraction) %>% 
    rename("gain" = cumulative_capture_rate) %>% 
    pivot_longer(cols = c(gain, baseline), names_to = "key")

gain_lift_tbl %>% 
    mutate(diff = cumulative_capture_rate - cumulative_data_fraction) %>% 
    ggplot(aes(cumulative_data_fraction, diff)) +
    geom_line() +
    theme_bw()

gain_lift_tbl %>% 
    mutate(diff = cumulative_capture_rate - cumulative_data_fraction) %>% 
    slice_max(order_by = diff, n = 1) %>% 
    select(cumulative_data_fraction, cumulative_capture_rate)

gain_transformed_tbl %>% 
    ggplot(aes(x = cumulative_data_fraction, y = value, colour = key)) +
    geom_line(linewidth = 1.5) +
    geom_vline(xintercept = 0.501, linetype = 2) +
    tidyquant::theme_tq() +
    tidyquant::scale_color_tq() +
    labs(title = "Gain Chart", x = "Cumulative Data Fraction", y = "Gain")

# Lift
lift_transformed_tbl <- 
    gain_lift_tbl %>% 
    select(- cumulative_capture_rate) %>% 
    mutate(baseline = 1) %>% 
    rename("lift" = cumulative_lift) %>% 
    pivot_longer(cols = c(lift, baseline), names_to = "key")

lift_transformed_tbl %>% 
    ggplot(aes(x = cumulative_data_fraction, y = value, colour = key)) +
    geom_line(linewidth = 1.5) +
    geom_vline(xintercept = 0.501, linetype = 2) +
    tidyquant::theme_tq() +
    tidyquant::scale_color_tq() +
    labs(title = "Lift Chart", x = "Cumulative Data Fraction", y = "Lift")

## Explore calibration of probabilities ----

predictions <- 
    h2o.predict(object = xg_boost, newdata = test_h2o) %>% 
    as_tibble() %>% 
    select(predicted = predict , prob = Yes)
    
predicted_tbl <- 
    test_data %>% 
    select(observed = readmitted) %>% 
    bind_cols(predictions)

predicted_tbl %>% 
    count(predicted, observed)

calibration_tbl <- 
    predicted_tbl %>% 
    select(observed, prob) %>% 
    mutate(group = cut(prob, breaks = seq(0, 1, 0.1))) %>% 
    group_by(group) %>% 
    count(observed) %>% 
    filter(observed == "Yes") %>% 
    ungroup() %>% 
    mutate(observed_event_total = cumsum(n)) %>% 
    mutate(observed_event_pct = observed_event_total / max(observed_event_total)) %>% 
    mutate(bin_low = str_split_i(group, ",", i = 1) %>% str_remove("\\(") %>% as.numeric()) %>% 
    mutate(bin_high = str_split_i(group, ",", i = 2) %>% str_remove("\\]") %>% as.numeric()) %>% 
    mutate(bin_midpoint = (bin_high + bin_low) / 2) %>% 
    select(bin_midpoint, observed_event_pct)

calibration_tbl %>% 
    bind_rows(
        tibble(bin_midpoint = c(0.05, 0.15, 0.95),
               observed_event_pct = c(0, 0, 1))
    ) %>% 
    ggplot(aes(bin_midpoint, observed_event_pct)) +
    geom_point() +
    geom_line() +
    geom_abline(linetype = 3) +
    expand_limits(x = c(0, 1)) +
    theme_bw() +
    labs(title = "Not well calibrated probabilities results in sigmoid curve")

predicted_tbl %>% 
    ggplot(aes(prob)) +
    geom_histogram(bins = 20, fill = "steelblue", colour = "grey30") +
    facet_wrap(~ observed, ncol = 1) +
    theme_bw()

predicted_tbl %>% 
    group_by(observed) %>% 
    summarise(mean = mean(prob),
              sd = sd(prob))

t.test(prob ~ observed, data = predicted_tbl)

## explore different threshold and impact on metrics ----

f0.5_predictions <- 
    predicted_tbl %>% 
    mutate(predicted = 
               if_else(prob > 0.594462, "Yes", "No"))

f0.5_predictions %>% 
    count(predicted, observed)

