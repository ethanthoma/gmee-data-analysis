# Load libraries
library(tidyverse)
library(boot)
library(caret)

# Set seed for reproducibility
set.seed(26-07-2023)

# Read in raw mturk data
raw_df <- read.csv("//wsl.localhost/Debian/home/ethanthoma/projects/generative-model-extrinsic-eval/commongen_validation_test_set_generation/mturk_batches/sandbox_env_internal_first_100_questions/Batch_387776_batch_results.csv")

# Drop useless columns
dropped_df <- raw_df %>%
  select(starts_with("Answer"), starts_with("Input"), AssignmentStatus, HITId, WorkerId) %>%
  select(-c(Input.set_name, Input.concept_set_id, Input.concepts, Answer.feedback.on))

# Rejoin Input columns
merge_columns <- dropped_df %>%
  select(HITId, WorkerId, starts_with("Input"))

# Tidy the data
df <- dropped_df %>%
  filter(AssignmentStatus != "Rejected") %>%
  group_by(HITId, WorkerId) %>%
  pivot_longer(
    cols = starts_with("Answer."),
    names_pattern = "(.+)(\\d)",
    names_to = c("name", "val")
  ) %>%
  mutate(
    name = name %>% str_sub(1, -2)
  ) %>%
  mutate(
    score = ifelse(value, val, 0) %>% as.numeric,
    .keep = "unused"
  ) %>%
  group_by(
    HITId, WorkerId, name
  ) %>%
  summarize(
    score = sum(score),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = score
  ) %>%
  left_join(merge_columns, by = c("HITId", "WorkerId"))

# Perform imputation
answer_cols <- df %>%
  select(starts_with("Answer")) %>%
  colnames
input_cols <- df %>%
  select(starts_with("Input")) %>%
  colnames

# TODO: vectorize
for (i in 1:nrow(df)) {
  row <- df[i, ]
  for (answer_col in answer_cols) {
    cell <- row %>%
      select(answer_col)

    if (cell %>% is.na) {
      # Get Input that matches
      input_col <- answer_col %>%
        sub(pattern = "Answer", replacement = "Input") %>%
        sub(pattern = "\\.[A-Za-z]+$",  replacement = "")

      input_val <- row[input_col]

      metric <- cell %>%
        colnames %>%
        sub(pattern = ".+\\.", replacement = "")

      for (input in input_cols) {
        input_cell <- row[input]

        if (input_cell == input_val) {
          model_name <- input_cell %>%
            colnames %>%
            sub(pattern = "Input\\.", replacement = "")

          value <- row[paste0("Answer.", model_name, ".", metric)]

          if (!(value %>% is.na)) {
            df[i, answer_col] = value
            break
          }
        }
      }
    }
  }
}

# Reorganize data for the models
manual_data <- df %>%
  select(starts_with("Answer.")) %>%
  rename_with(~ gsub("^Answer\\.", "", .), starts_with("Answer.")) %>%
  pivot_longer(
    everything(),
    names_to = "Model",
    values_to = "Score"
  ) %>%
  group_by(Model) %>%
  summarize(
    Score = mean(Score)
  ) %>%
  separate(
    Model,
    into = c("row_name", "column_name"),
    sep = "\\.") %>%
  pivot_wider(
    names_from = column_name,
    values_from = Score
  ) %>%
  column_to_rownames("row_name")

# Read in the automatic evaluation data
raw_auto_data <- read.csv("//wsl.localhost/Debian/home/ethanthoma/projects/gmee-data-analysis/automatic_evaluation.csv")

automatic_data <- raw_auto_data %>%
  column_to_rownames("Model.Name") %>%
  mutate(
    No_Choice_Percentage = as.numeric(sub("%", "", No.Choice..)) / 100,
    Inclusion_Percentage = as.numeric(sub("%", "", Inclusion..)) / 100,
    Sequence_Percentage = as.numeric(sub("%", "", Sequence..)) / 100,
    .keep = "unused"
  ) %>%
  select(-Coverage)

model_data <- automatic_data %>%
  rownames_to_column("Model") %>%
  merge(
    manual_data %>% rownames_to_column("Model"),
    by = "Model"
  ) %>%
  column_to_rownames("Model")

automatic_metrics <- automatic_data %>% colnames

# Loop through each response variable
for (response_var in automatic_metrics) {
  # List to drop other responses
  other_metrics <- setdiff(automatic_metrics, response_var)

  # Drop missing value
  # i.e. chatgpt sequence
  curr_data <- model_data %>%
    select(-one_of(other_metrics)) %>%
    drop_na()

  X <- curr_data %>%
    select(-{{response_var}})
  Y <- curr_data %>%
    pull({{response_var}})

  # Define the LOOCV train control
  loocv_control <- trainControl(method = "LOOCV")

  # Fit the linear model using LOOCV
  lm_model <- train(
    Y ~ .,
    data = cbind(X, Y),
    method = "lm",
    trControl = loocv_control
  )

  # Print the results for the current response variable
  cat("Response Variable:", response_var, "\n")
  print(lm_model$results)
  cat("\n")
}



