# 09-eda_trump_harris_post_aug23.R

#### Preamble ####
# Purpose: Exploratory Data Analysis of polls including both Trump and Harris after August 23rd
# Author: [Your Name]
# Date: [Current Date]
# Contact: [Your Email]
# License: [Appropriate License]

#### Workspace setup ####
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)

# Read the cleaned data from CSV
polling_data <- read_csv("data/02-analysis_data/cleaned_polling_data.csv", show_col_types = FALSE)

#### Data Preparation ####

# Clean candidate names
polling_data <- polling_data %>%
  mutate(
    candidate_name = str_trim(candidate_name),
    candidate_name = str_to_title(candidate_name)
  )

# Convert date columns to Date type if not already
polling_data <- polling_data %>%
  mutate(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date),
    election_date = as.Date(election_date)
  )

# Filter data for polls after August 23rd
polling_data_post_aug23 <- polling_data %>%
  filter(end_date >= as.Date("2024-08-23"))

# Filter data to include only Donald Trump and Kamala Harris
candidates_of_interest <- c("Donald Trump", "Kamala Harris")

polling_data_filtered <- polling_data_post_aug23 %>%
  filter(candidate_name %in% candidates_of_interest)

# Identify poll_ids where both Trump and Harris are included
polls_with_both <- polling_data_filtered %>%
  group_by(poll_id) %>%
  filter(all(candidates_of_interest %in% candidate_name)) %>%
  ungroup() %>%
  pull(poll_id) %>%
  unique()

# Check how many polls are included
num_polls <- length(polls_with_both)
cat("Number of polls including both Donald Trump and Kamala Harris after August 23rd:", num_polls, "\n")

if (num_polls == 0) {
  stop("No polls found that include both Donald Trump and Kamala Harris after August 23rd.")
}

# Filter the polling data to include only these polls
polling_data_both <- polling_data_filtered %>%
  filter(poll_id %in% polls_with_both)

#### Analysis ####

# Calculate summary statistics for each candidate in polls where both are present
summary_stats <- polling_data_both %>%
  group_by(candidate_name) %>%
  summarise(
    mean_pct = mean(pct_normalized, na.rm = TRUE),
    sd_pct = sd(pct_normalized, na.rm = TRUE),
    min_pct = min(pct_normalized, na.rm = TRUE),
    max_pct = max(pct_normalized, na.rm = TRUE),
    n_polls = n_distinct(poll_id)
  )

print("Summary statistics for Donald Trump and Kamala Harris in polls where both are included:")
print(summary_stats)

#### Visualization ####

# Create plots directory if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Plot the distribution of polling percentages
ggplot(polling_data_both, aes(x = pct_normalized, fill = candidate_name)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(title = "Distribution of Poll Percentages for Trump and Harris (Post Aug 23)",
       x = "Polling Percentage",
       y = "Count",
       fill = "Candidate") +
  theme_minimal() +
  theme(legend.position = "top")

# Save plot
ggsave("plots/trump_harris_poll_distribution_post_aug23.png", width = 8, height = 6)

# Time series plot of polling percentages over time
ggplot(polling_data_both, aes(x = end_date, y = pct_normalized, color = candidate_name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Polling Percentages Over Time for Trump and Harris (Post Aug 23)",
       x = "Poll End Date",
       y = "Polling Percentage",
       color = "Candidate") +
  theme_minimal()

# Save plot
ggsave("plots/trump_harris_poll_over_time_post_aug23.png", width = 10, height = 6)

#### Model Building ####

# Prepare the data
model_data <- polling_data_both %>%
  select(poll_id, candidate_name, pct_normalized, days_until_election, sample_size, methodology, population, state, end_date, election_date)

# Calculate 'days_until_election' if not present
if (!"days_until_election" %in% names(model_data)) {
  model_data <- model_data %>%
    mutate(
      days_until_election = as.numeric(difftime(election_date, end_date, units = "days"))
    )
}

# Convert categorical variables to factors
model_data <- model_data %>%
  mutate(
    candidate_name = factor(candidate_name),
    methodology = factor(methodology),
    population = factor(population),
    state = factor(state)
  )

#### Modeling ####

# Build a linear model predicting polling percentage based on candidate and other predictors
model <- lm(pct_normalized ~ candidate_name + days_until_election + sample_size + methodology + population + state, data = model_data)

# Summarize the model
model_summary <- summary(model)
print(model_summary)

#### Interpretation ####

# Extract coefficients for candidate_name
coefficients <- model_summary$coefficients
candidate_effect <- coefficients[grep("candidate_name", rownames(coefficients)), , drop = FALSE]

print("Effect of candidate (reference is Donald Trump):")
print(candidate_effect)

#### Conclusion ####

cat("Analysis complete. See output and plots for results.\n")
