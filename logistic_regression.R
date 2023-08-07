# Load libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(26-07-2023)

# Get tidy manual data
source("//wsl.localhost/Debian/home/ethanthoma/projects/gmee-data-analysis/imputed_mturk_data.R")
manual_data_file_path = "//wsl.localhost/Debian/home/ethanthoma/projects/generative-model-extrinsic-eval/commongen_validation_test_set_generation/mturk_batches/sandbox_env_internal_first_100_questions/Batch_387776_batch_results.csv"
manual_data = get_manual_data(manual_data_file_path)

# Get tidy automatic data
get_automatic_data <- function (file_path) {
  # Read in the automatic evaluation data
  raw_auto_data <- read.csv(file_path)

  # Tidy the data
  raw_auto_data %>%
    column_to_rownames("Model.Name") %>%
    mutate(
      No_Choice_Percentage = as.numeric(sub("%", "", No.Choice..)) / 100,
      Inclusion_Percentage = as.numeric(sub("%", "", Inclusion..)) / 100,
      Sequence_Percentage = as.numeric(sub("%", "", Sequence..)) / 100,
      .keep = "unused"
    ) %>%
    return
}
automatic_data_file_path = "//wsl.localhost/Debian/home/ethanthoma/projects/gmee-data-analysis/automatic_evaluation.csv"
automatic_data = get_automatic_data(automatic_data_file_path)

# Merge both metric types
model_data <- automatic_data %>%
  rownames_to_column("Model") %>%
  merge(
    manual_data %>% rownames_to_column("Model"),
    by = "Model"
  ) %>%
  column_to_rownames("Model")

# Get the variable names of the automatic data
automatic_metrics <- automatic_data %>%
  select(-Coverage) %>%
  colnames

# Perform MLR for each automatic metric to get learned weights via RMSE
# optimization
perform_MLR <- function (model_data, automatic_metrics) {
  library(caret)

  for (response_var in automatic_metrics) {
    # List to drop other responses
    other_metrics <- setdiff(automatic_metrics, response_var)

    # Drop missing value
    # i.e. chatgpt sequence
    curr_data <- model_data %>%
      select(-one_of(other_metrics)) %>%
      drop_na() %>%
      mutate(response = get({{response_var}})) %>%
      select(-{{response_var}}) %>%
      select(-c(fluency))

    # Define the LOOCV train control
    loocv_control <- trainControl(method = "LOOCV")

    # Fit the linear model using LOOCV
    lm_model <- train(
      response ~ .,
      data = curr_data,
      method = "lm",
      trControl = loocv_control
    )

    # Print the results for the current response variable
    cat("Response Variable:", response_var, "\n")
    print(lm_model$results)
    cat("\n")
    print(lm_model$finalModel)
    cat("\n")
  }
}
perform_MLR(model_data, automatic_metrics)

# Perform manual weighted linear regression for each automatic metric
perform_MWLR <- function (model_data, automatic_metrics) {
  res <- model_data %>%
    mutate(
      complexity = complexity * 0.15,
      fluency = fluency * 0,
      sensibility = sensibility * 0.70,
      Coverage = Coverage * 0.15
    ) %>%
    mutate(
      Res = complexity + fluency + sensibility,
      .keep = "unused"
    ) %>%
    pull(Res)

  for (response_var in automatic_metrics) {
    cor_df <- model_data %>%
      pull(response_var) %>%
      cbind(res)

    cor_df <- cor_df[complete.cases(cor_df), ]

    # Print the results for the current response variable
    cat("Variable:", response_var, "\n")
    print(cor(cor_df[,1], cor_df[,2] * 5))
    cat("\n")
  }
}
perform_MWLR(model_data, automatic_metrics)
