#### Preamble ####
# Purpose: Tests... [...UPDATE THIS...]
# Author: Harsh M Pareek, Arshh Relan, Benji Feurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, relanarshh@gmail.com
# License: MIT
# Pre-requisites: [...UPDATE THIS...]

#### Workspace setup ####
library(testthat)
library(dplyr)
library(readr)

# Read the cleaned data from CSV
cleaned_data <- read_csv("data/02-analysis_data/cleaned_polling_data.csv", show_col_types = FALSE)

#### Tests ####

test_that("No missing values in key columns", {
  key_cols <- c("poll_id", "pollster_id", "pollster", "start_date", "end_date",
                "sample_size", "methodology", "population", "state",
                "candidate_name", "candidate_id", "pct")
  for (col in key_cols) {
    expect_false(any(is.na(cleaned_data[[col]])), info = paste("Missing values in", col))
  }
})

test_that("Percentages between 0 and 100", {
  expect_true(all(cleaned_data$pct >= 0 & cleaned_data$pct <= 100), info = "Percentages not between 0 and 100")
})

test_that("Dates are in correct format", {
  expect_true(all(class(cleaned_data$start_date) == "Date"), info = "start_date is not Date type")
  expect_true(all(class(cleaned_data$end_date) == "Date"), info = "end_date is not Date type")
  expect_true(all(class(cleaned_data$election_date) == "Date"), info = "election_date is not Date type")
})

test_that("Percentage sums to approximately 100 per poll", {
  pct_sums <- cleaned_data %>%
    group_by(poll_id) %>%
    summarise(total_pct = sum(pct))
  # Allowing for small rounding errors
  expect_true(all(abs(pct_sums$total_pct - 100) < 2), info = "Percentages per poll do not sum to approximately 100")
})

cat("All tests passed for cleaned data.\n")