#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Harsh M Pareek, Arshh Relan, Benji Feurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, relanarshh@gmail.com
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(dplyr)
library(readr)

# Read the cleaned data from CSV
polling_data <- read_csv("data/02-analysis_data/cleaned_polling_data.csv", show_col_types = FALSE)

#### Data Preparation ####

# Include all candidates listed
candidates_to_include <- c("Donald Trump", "Joe Biden", "Kamala Harris", "Robert F. Kennedy",
                           "Jill Stein", "Cornel West", "Chase Oliver", "Ron DeSantis",
                           "Nikki Haley", "Gavin Newsom")

polling_data_filtered <- polling_data %>%
  filter(candidate_name %in% candidates_to_include)

# Create a dataset for modeling
model_data <- polling_data_filtered %>%
  select(pct, candidate_name, days_until_election, sample_size, methodology, population, state)

# Convert categorical variables to factors
model_data <- model_data %>%
  mutate(
    candidate_name = factor(candidate_name),
    methodology = factor(methodology),
    population = factor(population),
    state = factor(state)
  )

#### Recode Factors ####

# Combine rare levels in 'state' into 'Other'
state_counts <- table(model_data$state)
rare_states <- names(state_counts[state_counts < 30])  # Adjust threshold as needed

model_data <- model_data %>%
  mutate(
    state = if_else(state %in% rare_states, "Other", as.character(state)),
    state = factor(state)
  )

# Combine rare levels in 'methodology' into 'Other'
methodology_counts <- table(model_data$methodology)
frequent_methodologies <- names(methodology_counts[methodology_counts >= 30])  # Adjust threshold as needed

model_data <- model_data %>%
  mutate(
    methodology = if_else(methodology %in% frequent_methodologies, methodology, "Other"),
    methodology = factor(methodology, levels = c(frequent_methodologies, "Other"))
  )

# Combine rare levels in 'population' into 'Other'
population_counts <- table(model_data$population)
frequent_populations <- names(population_counts[population_counts >= 30])  # Adjust threshold as needed

model_data <- model_data %>%
  mutate(
    population = if_else(population %in% frequent_populations, population, "Other"),
    population = factor(population, levels = c(frequent_populations, "Other"))
  )

#### Modeling ####

# Build a combined linear regression model for all candidates
model <- lm(pct ~ candidate_name + days_until_election + sample_size + methodology + population + state, data = model_data)

# Check for rank deficiency
if (any(is.na(coef(model)))) {
  warning("Model coefficients contain NA values due to rank deficiency.")
} else {
  cat("Model coefficients are estimated without rank deficiency.\n")
}

# Save the model
dir.create("models", showWarnings = FALSE)
saveRDS(model, "models/combined_candidate_model.rds")