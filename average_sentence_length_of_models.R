# Import libraries
library(rjson)
library(tidyverse)

root <- "//wsl.localhost/Debian/home/ethanthoma/projects/generative-model-extrinsic-eval/experiments/generated_sentences/"
type <- "-train-WITH-choicewords-noquestionwordlimit.json"

# Ordered by sensibility
models <- c(
  "mt0_small",
  "mt0_base",
  "mt0_large",
  "bloomz_560m",
  "mt0_xl",
  "bloomz_1b1",
  "bloomz_1b7",
  "bloomz_3b",
  "flan_t5_large",
  "t0_3b",
  "flan_t5_xl",
  "tk_instruct_3b_def",
  "chatgpt"
)

# Calculates the average word count of a sentence
get_word_avg_count_of_sentence <- function(sentence) {
  strsplit(sentence, split = ' ') %>%
    lengths %>%
    mean %>%
    return()
}

# Calculates the running mean of a list
# Takes in a function to transform the list
running_mean <- function(xs, f = identity) {
  xs %>%
    reduce(
      .f = function(index_and_mean, x_i) {
        f_of_x_i = x_i %>% f
        index = index_and_mean[1] + 1
        curr_mean = (index_and_mean[2] * (index-1) + f_of_x_i) / index

        return(c(index, curr_mean))
      },
      .init = c(0, 0)
    ) %>%
    `[`(2)
}

# Calculate the average sentence lengths per model
average_length <- models %>%
  map(
    ~ list(
      model = .,
      mean = fromJSON(file = paste0(root, ., type)) %>%
        do.call(what = rbind) %>%
        as.data.frame() %>%
        mutate(
          mean_sentence_length = sentences %>%
            lapply(
              function (list_of_sentences)
                running_mean(
                  list_of_sentences,
                  f = get_word_avg_count_of_sentence
                )
            ) %>% unlist
        ) %>%
        summarize(
          mean = mean_sentence_length %>% mean
        ) %>%
        pull
      )
  ) %>%
  do.call(what = rbind)

average_length %>% print
average_length %>% view
