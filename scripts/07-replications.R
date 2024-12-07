#### Preamble ####
# Purpose: Replicated graphs from... [...UPDATE THIS...]
# Author: Harsh M Pareek, Arshh Relan, Benji Fleurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, arshh.relan@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(dplyr)
library(ggplot2)
library(readr)

# Load the model
model <- readRDS("models/combined_candidate_model.rds")

# Read the cleaned data from CSV
polling_data <- read_csv("data/02-analysis_data/cleaned_polling_data.csv", show_col_types = FALSE)

#### Data Preparation ####

# Include all candidates listed
candidates_to_include <- c("Donald Trump", "Joe Biden", "Kamala Harris", "Robert F. Kennedy",
                           "Jill Stein", "Cornel West", "Chase Oliver", "Ron DeSantis",
                           "Nikki Haley", "Gavin Newsom")

polling_data_filtered <- polling_data %>%
  filter(candidate_name %in% candidates_to_include)

# Prepare data for prediction
model_data <- polling_data_filtered %>%
  select(pct, candidate_name, days_until_election, sample_size, methodology, population, state)

# Convert categorical variables to factors with levels matching the model
model_data <- model_data %>%
  mutate(
    candidate_name = factor(candidate_name, levels = levels(model$model$candidate_name)),
    methodology = factor(methodology),
    population = factor(population),
    state = factor(state)
  )

# Recode factors to match levels in the model

# Recode 'state' levels
model_data <- model_data %>%
  mutate(
    state = if_else(!state %in% levels(model$model$state), "Other", as.character(state)),
    state = factor(state, levels = levels(model$model$state))
  )

# Recode 'methodology' levels
model_data <- model_data %>%
  mutate(
    methodology = if_else(!methodology %in% levels(model$model$methodology), "Other", as.character(methodology)),
    methodology = factor(methodology, levels = levels(model$model$methodology))
  )

# Recode 'population' levels
model_data <- model_data %>%
  mutate(
    population = if_else(!population %in% levels(model$model$population), "Other", as.character(population)),
    population = factor(population, levels = levels(model$model$population))
  )

#### Predictions ####

# Make predictions using the combined model
model_data$predicted_pct <- predict(model, newdata = model_data)

# Aggregate predictions by candidate
candidate_predictions <- model_data %>%
  group_by(candidate_name) %>%
  summarise(
    predicted_pct = mean(predicted_pct, na.rm = TRUE),
    n_polls = n()
  ) %>%
  arrange(desc(predicted_pct))

print("Predicted percentages by candidate:")
print(candidate_predictions)

# Forecast the winner
winner <- as.character(candidate_predictions$candidate_name[1])

cat("Predicted winner:", winner, "\n")

#### Visualization ####

# Create plots directory if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Plot predicted percentages
ggplot(candidate_predictions, aes(x = reorder(candidate_name, predicted_pct), y = predicted_pct, fill = candidate_name)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = "Predicted Average Poll Percentages by Candidate",
       x = "Candidate",
       y = "Predicted Percentage",
       fill = "Candidate") +
  theme_minimal() +
  theme(legend.position = "none")

# Save plot
ggsave("plots/predicted_percentages_by_candidate.png", width = 8, height = 6)