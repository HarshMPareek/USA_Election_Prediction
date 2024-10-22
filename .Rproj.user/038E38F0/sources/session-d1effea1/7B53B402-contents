#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Australian 
  #electoral divisions dataset.
# Author: Harsh M Pareek, Arshh Relan, Benji Fleurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, relanarshh@gmail.com
# License: MIT
# Pre-requisites: 
  # - The `tidyverse` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
# 01-test_simulated_data.R

# Load necessary packages
library(testthat)
library(dplyr)

# Read the simulated data
simulated_data <- read.csv("data/simulated_polls.csv")

# Perform tests
test_that("Data has correct columns", {
  expected_cols <- c("poll_id", "pollster_id", "pollster", "start_date", "end_date", "sample_size",
                     "methodology", "population", "state", "candidate_name", "candidate_id", "pct")
  expect_true(all(expected_cols %in% colnames(simulated_data)))
})

test_that("Percentage sums to 100 per poll", {
  pct_sums <- simulated_data %>%
    group_by(poll_id) %>%
    summarise(total_pct = sum(pct))
  expect_true(all(abs(pct_sums$total_pct - 100) < 1e-6))
})

test_that("No missing values in key columns", {
  key_cols <- c("poll_id", "pollster_id", "candidate_name", "pct")
  for (col in key_cols) {
    expect_false(any(is.na(simulated_data[[col]])))
  }
})

cat("All tests passed for simulated data.\n")
