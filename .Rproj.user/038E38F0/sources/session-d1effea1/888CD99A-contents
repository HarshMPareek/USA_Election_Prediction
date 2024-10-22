# 08-analyze_trump_harris_polls.R

#### Preamble ####
# Purpose: Analyze polls where both Donald Trump and Kamala Harris are included
# Author: [Your Name]
# Date: [Current Date]
# Contact: [Your Email]
# License: [Appropriate License]

#### Workspace setup ####
library(dplyr)
library(ggplot2)
library(readr)

# Read the cleaned data from CSV
polling_data <- read_csv("data/02-analysis_data/cleaned_polling_data.csv", show_col_types = FALSE)

#### Data Preparation ####

# Filter data to include only Donald Trump and Kamala Harris
candidates_of_interest <- c("Donald Trump", "Kamala Harris")

polling_data_filtered <- polling_data %>%
  filter(candidate_name %in% candidates_of_interest)

# Identify poll_ids where both Trump and Harris are included
polls_with_both <- polling_data_filtered %>%
  group_by(poll_id) %>%
  summarise(candidates_in_poll = list(unique(candidate_name))) %>%
  filter(all(candidates_of_interest %in% candidates_in_poll)) %>%
  pull(poll_id)

# Filter the polling data to include only these polls
polling_data_both <- polling_data_filtered %>%
  filter(poll_id %in% polls_with_both)

# Check how many polls are included
cat("Number of polls including both Donald Trump and Kamala Harris:", length(unique(polling_data_both$poll_id)), "\n")

#### Analysis ####

# Calculate summary statistics for each candidate in polls where both are present
summary_stats <- polling_data_both %>%
  group_by(candidate_name) %>%
  summarise(
    mean_pct = mean(pct_normalized),
    sd_pct = sd(pct_normalized),
    min_pct = min(pct_normalized),
    max_pct = max(pct_normalized),
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
  labs(title = "Distribution of Poll Percentages for Trump and Harris",
       x = "Polling Percentage",
       y = "Count",
       fill = "Candidate") +
  theme_minimal() +
  theme(legend.position = "top")

# Save plot
ggsave("plots/trump_harris_poll_distribution.png", width = 8, height = 6)

# Time series plot of polling percentages over time
ggplot(polling_data_both, aes(x = end_date, y = pct_normalized, color = candidate_name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Polling Percentages Over Time for Trump and Harris",
       x = "Poll End Date",
       y = "Polling Percentage",
       color = "Candidate") +
  theme_minimal()

# Save plot
ggsave("plots/trump_harris_poll_over_time.png", width = 10, height = 6)

#### Model Building ####

# Build a model comparing Trump and Harris using only polls where both are included

# Prepare the data
model_data <- polling_data_both %>%
  select(poll_id, candidate_name, pct_normalized, days_until_election, sample_size, methodology, population, state)

# Convert categorical variables to factors
model_data <- model_data %>%
  mutate(
    candidate_name = factor(candidate_name),
    methodology = factor(methodology),
    population = factor(population),
    state = factor(state)
  )

#### Recode Factors (if necessary) ####

# Recode 'state' levels
state_levels <- levels(model_data$state)
model_data <- model_data %>%
  mutate(
    state = factor(state, levels = state_levels)
  )

# Recode 'methodology' levels
methodology_levels <- levels(model_data$methodology)
model_data <- model_data %>%
  mutate(
    methodology = factor(methodology, levels = methodology_levels)
  )

# Recode 'population' levels
population_levels <- levels(model_data$population)
model_data <- model_data %>%
  mutate(
    population = factor(population, levels = population_levels)
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

# The coefficient for candidate_nameKamala Harris indicates the average difference in polling percentage between Harris and Trump, controlling for other variables.

#### Conclusion ####

cat("Analysis complete. See output and plots for results.\n")
