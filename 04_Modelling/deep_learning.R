# Deep Learning Using Full Data ----

## Set Up ----
# Load Libraries
library(tidyverse)
library(h2o)

# Load Data 
diabetes_train <- read_rds("00_Data/processed/for_analysis/diabetes_train.rds")
diabetes_test <- read_rds("00_Data/processed/for_analysis/diabetes_test.rds")

# H2O
h2o.init()
h2o.removeAll()
training_h2o <- as.h2o(diabetes_train)
testing_h2o <- as.h2o(diabetes_test)

# Define Model Parameters
response   <- "readmitted"
predictors <- setdiff(names(diabetes_train), response)

## Single layer - no regularisation ----

### Tuning models ----
hidden_opt <- c(64, 128, 256, 512)
epochs_opt <- c(10, 25, 50)
hyper_params <- list(hidden = hidden_opt, epochs = epochs_opt)

### Train models ----

simple_dl_grid <- 
    h2o.grid(
    "deeplearning",
    grid_id = "simple_dl_grid",
    hyper_params = hyper_params,
    x = predictors,
    y = response,
    training_frame = training_h2o,
    nfolds = 5,
    seed = 42
)

### Assess models ----
simple_dl_grid_perf <- 
    h2o.getGrid("simple_dl_grid", sort_by = "auc", decreasing = TRUE)

simple_dl_grid_perf

## appears that more epochs improves performance across models
## explore each individually using regularisation techniques - dropout and l2

## Single layer - regularisation ----

### 256 neurons / 25 epochs ----
dl_model_256_25 <- h2o.getModel("simple_dl_grid_model_8")
dl_model_256_25 %>%  h2o.auc(train = TRUE, xval = TRUE)

### overfitted +++ 

### Tuning models ----

activation_opt <- c("Rectifier", "RectifierWithDropout") 
l2_opt <- c(0, 0.0001, 0.001, 0.01)

hyper_params <- list(activation = activation_opt, l2 = l2_opt)

### Train models ----

regularisation_256_25_grid <- 
    h2o.grid(
        "deeplearning",
        grid_id = "regularisation_256_25_grid",
        hyper_params = hyper_params,
        x = predictors,
        y = response,
        training_frame = training_h2o,
        epochs = 25,
        hidden = 256,
        nfolds = 5,
        seed = 42
    )

