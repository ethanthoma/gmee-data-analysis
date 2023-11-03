library(jsonlite)
library(irr)
library(tidyverse)

first_batch_file_path <- "//wsl.localhost/Debian/home/ethanthoma/projects/gmee-data-analysis/data/first_batch.json"
first_batch <- first_batch_file_path %>%
  fromJSON %>%
  mutate(Batch = 1)

second_batch_file_path <- "//wsl.localhost/Debian/home/ethanthoma/projects/gmee-data-analysis/data/second_batch.json"
second_batch <- second_batch_file_path %>%
  fromJSON %>%
  mutate(Batch = 2)

stacked_df <- bind_rows(first_batch, second_batch)

tidy_df <- stacked_df %>%
  select(-c(HITId, AssignmentId, AcceptTime, SubmitTime, SubsetName)) %>%
  rename_with(~ gsub("_", "-", .)) %>%
  mutate(
    across(!one_of("ConceptSetID", "WorkerId", "Batch"),
      list(
        complexity = ~ .[["complexity"]],
        fluency = ~ .[["fluency"]],
        sensibility = ~ .[["sensibility"]]
      )
    ),
    .keep = "unused"
  ) %>%
  pivot_longer(
    cols = !one_of("ConceptSetID", "WorkerId", "Batch"),
    names_to = c(".value", "Category"),
    names_pattern = "(.*?)_(.*)"
  ) %>%
  pivot_longer(
    cols = !one_of("ConceptSetID", "Category", "WorkerId", "Batch"),
    names_to = "Model",
    values_to = "Score"
  ) %>%
  pivot_wider(
    names_from = "WorkerId",
    values_from = "Score"
  ) %>%
  select(-c(ConceptSetID, Category, Model))

batch_one_score <- tidy_df %>%
  filter(Batch == 1) %>%
  select(-Batch) %>%
  select(where(~ !all(is.na(.)))) %>%
  kappam.fleiss %>%
  `$`(value)

batch_two_score <- tidy_df %>%
  filter(Batch == 2) %>%
  select(-Batch) %>%
  select(where(~ !all(is.na(.)))) %>%
  kappam.fleiss %>%
  `$`(value)

cat(sprintf("First batch score is: %f\n", batch_one_score))
cat(sprintf("Second batch score is: %f\n", batch_two_score))
cat(sprintf("Average score is: %f\n", mean(batch_one_score + batch_two_score)))
