# Load libraries
library(tidyverse)

# Set seed for reproducibility
set.seed(26-07-2023)

# Get tidy manual data
get_manual_data <- function (file_path) {
  # Read in raw mturk data
  raw_df <- read.csv(file_path)

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
        select(all_of(answer_col))

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
  df %>%
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
    column_to_rownames("row_name") %>%
    return
}
