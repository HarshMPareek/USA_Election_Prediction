# ============================================================
# 09-eda_trump_harris.R
# ============================================================

#### Preamble ####
# Purpose: Exploratory Data Analysis of polls including both Trump and Harris
#          (All dates, aggregating congressional districts back to states)
# Author: [Your Name]
# Date: [Current Date]
# Contact: [Your Email]
# License: [Appropriate License]

#### Workspace setup ####

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(sf)
library(maps)       # Ensure maps is loaded before purrr
library(mapdata)
library(broom)
library(purrr)
library(scales)
library(ggrepel)

# Create directories for plots and outputs if they don't exist
dir.create("plots", showWarnings = FALSE)
dir.create("plots/geospatial", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/eda", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/slr", showWarnings = FALSE, recursive = TRUE)
dir.create("plots/simulations", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

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

# Proceed with all dates (Removed the filter for polls after August 23rd)
polling_data_all_dates <- polling_data

# Filter data to include only Donald Trump and Kamala Harris
candidates_of_interest <- c("Donald Trump", "Kamala Harris")

polling_data_filtered <- polling_data_all_dates %>%
  filter(candidate_name %in% candidates_of_interest)

# Aggregate Congressional Districts Back to States
# Function to map congressional districts to their parent states
map_cd_to_state <- function(state_name) {
  # Remove congressional district suffix if present (e.g., " Maine CD-1" -> "Maine")
  str_replace(state_name, " CD-\\d+", "")
}

polling_data_aggregated <- polling_data_filtered %>%
  mutate(
    state = map_cd_to_state(state)
  )

# Identify poll_ids where both Trump and Harris are included
polls_with_both <- polling_data_aggregated %>%
  group_by(poll_id) %>%
  filter(all(candidates_of_interest %in% candidate_name)) %>%
  ungroup() %>%
  pull(poll_id) %>%
  unique()

# Check how many polls are included
num_polls <- length(polls_with_both)
cat("Number of polls including both Donald Trump and Kamala Harris:", num_polls, "\n")

if (num_polls == 0) {
  stop("No polls found that include both Donald Trump and Kamala Harris.")
}

# Filter the polling data to include only these polls
polling_data_both <- polling_data_aggregated %>%
  filter(poll_id %in% polls_with_both)

#### Electoral Votes Adjustment ####

# Define the number of electoral votes per state for 2024 based on the 2020 Census
# Exclude congressional districts as they've been aggregated back to states
electoral_votes <- tibble(
  state = c(state.name, "District of Columbia"),
  electoral_votes = c(
    9,  # Alabama
    3,  # Alaska
    11, # Arizona
    6,  # Arkansas
    55, # California
    10, # Colorado
    7,  # Connecticut
    3,  # Delaware
    3,  # District of Columbia
    30, # Florida
    16, # Georgia
    4,  # Hawaii
    4,  # Idaho
    19, # Illinois
    11, # Indiana
    6,  # Iowa
    6,  # Kansas
    8,  # Kentucky
    8,  # Louisiana
    4,  # Maine
    10, # Maryland
    11, # Massachusetts
    15, # Michigan
    10, # Minnesota
    6,  # Mississippi
    10, # Missouri
    4,  # Montana
    5,  # Nebraska
    6,  # Nevada
    4,  # New Hampshire
    14, # New Jersey
    5,  # New Mexico
    28, # New York
    16, # North Carolina
    3,  # North Dakota
    17, # Ohio
    7,  # Oklahoma
    8,  # Oregon
    19, # Pennsylvania
    4,  # Rhode Island
    9,  # South Carolina
    3,  # South Dakota
    11, # Tennessee
    40, # Texas
    6,  # Utah
    3,  # Vermont
    13, # Virginia
    12, # Washington
    4,  # West Virginia
    10, # Wisconsin
    3   # Wyoming
  )
)

# Merge polling data with electoral votes
state_poll_evs <- polling_data_both %>%
  left_join(electoral_votes, by = "state")

# Verify all states have electoral votes
missing_evs <- state_poll_evs %>%
  filter(is.na(electoral_votes)) %>%
  pull(state) %>%
  unique()

if(length(missing_evs) > 0){
  cat("States with missing electoral votes:\n")
  print(missing_evs)
  # Exclude these states from further analysis
  state_poll_evs <- state_poll_evs %>%
    filter(!state %in% missing_evs)
} else {
  cat("All states have corresponding electoral votes.\n")
}

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

# Distribution of polling percentages
distribution_plot <- ggplot(polling_data_both, aes(x = pct_normalized, fill = candidate_name)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(title = "Distribution of Poll Percentages for Trump and Harris",
       x = "Polling Percentage",
       y = "Count",
       fill = "Candidate") +
  theme_minimal() +
  theme(legend.position = "top")

# Save distribution plot
ggsave("plots/eda/trump_harris_poll_distribution.png", distribution_plot, width = 8, height = 6)

# Time series plot of polling percentages over time
time_series_plot <- ggplot(polling_data_both, aes(x = end_date, y = pct_normalized, color = candidate_name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Polling Percentages Over Time for Trump and Harris",
       x = "Poll End Date",
       y = "Polling Percentage",
       color = "Candidate") +
  theme_minimal()

# Save time series plot
ggsave("plots/eda/trump_harris_poll_over_time.png", time_series_plot, width = 10, height = 6)

#### Model Building ####

# Prepare the data for modeling
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

#### Geospatial Visualization ####

# Load US states shapefile using maps and sf
# Explicitly specify the namespace to avoid conflicts
states_map <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  rename(state_lower = ID)

# Prepare polling data for mapping
# Calculate average polling percentage per state and candidate
state_poll_avg <- polling_data_both %>%
  group_by(state, candidate_name) %>%
  summarise(
    avg_pct = mean(pct_normalized, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = candidate_name, values_from = avg_pct) %>%
  rename(
    trump_pct = `Donald Trump`,
    harris_pct = `Kamala Harris`
  )

# Merge with state map data
map_data <- states_map %>%
  mutate(state = str_to_title(str_replace(state_lower, "-", " "))) %>%
  left_join(state_poll_avg, by = "state")

# Handle any missing states (if any remain after aggregation)
map_data <- map_data %>%
  filter(!is.na(trump_pct) & !is.na(harris_pct))

# Create a predicted winner column
map_data <- map_data %>%
  mutate(
    predicted_winner = case_when(
      harris_pct > trump_pct ~ "Kamala Harris",
      trump_pct > harris_pct ~ "Donald Trump",
      TRUE ~ "Tie"
    )
  )

# Replace "Tie" with random winner to avoid mapping issues
set.seed(123)  # For reproducibility
map_data <- map_data %>%
  rowwise() %>%
  mutate(
    predicted_winner = ifelse(predicted_winner == "Tie", 
                              sample(c("Kamala Harris", "Donald Trump"), 1), 
                              predicted_winner)
  ) %>%
  ungroup()

# Create a choropleth map
choropleth_map <- ggplot() +
  geom_sf(data = states_map, fill = "gray90", color = "white") +
  geom_sf(data = map_data, aes(fill = predicted_winner), color = "black") +
  scale_fill_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red"), 
                    name = "Predicted Winner") +
  labs(
    title = "Predicted State Winners for the 2024 U.S. Presidential Election",
    subtitle = "Based on Average Polling Percentages",
    caption = "Data Source: Polling Data",
    fill = "Winner"
  ) +
  theme_minimal()

# Display the choropleth map
print(choropleth_map)

# Save the choropleth map
ggsave("plots/geospatial/predicted_state_winners_map.png", choropleth_map, width = 14, height = 8)

#### Monte Carlo Simulations ####

# Objective: Estimate the probability of each candidate winning the Electoral College

# Define Electoral College votes per state (already defined above)
# Ensure 'electoral_votes' tibble includes only states and DC

# Merge polling data with electoral votes
state_poll_evs <- state_poll_avg %>%
  left_join(electoral_votes, by = "state")

# Verify all states have electoral votes
missing_evs <- state_poll_evs %>%
  filter(is.na(electoral_votes)) %>%
  pull(state) %>%
  unique()

if(length(missing_evs) > 0){
  cat("States with missing electoral votes:\n")
  print(missing_evs)
  # Exclude these states from further analysis
  state_poll_evs <- state_poll_evs %>%
    filter(!state %in% missing_evs)
} else {
  cat("All states have corresponding electoral votes.\n")
}

# Number of simulations
n_simulations <- 1000

# Initialize a dataframe to store simulation results
simulation_results <- tibble(
  simulation = integer(),
  trump_evs = integer(),
  harris_evs = integer(),
  winner = character()
)

# Function to simulate one election
simulate_one_election <- function(state_data){
  # Simulate polling percentages with randomness (assuming normal distribution)
  sim_trump <- rnorm(n = nrow(state_data), mean = state_data$trump_pct, sd = 2)
  sim_harris <- rnorm(n = nrow(state_data), mean = state_data$harris_pct, sd = 2)
  
  # Ensure percentages are within 0-100
  sim_trump <- pmax(sim_trump, 0)
  sim_harris <- pmax(sim_harris, 0)
  
  # Determine winner in each state
  state_winners <- ifelse(sim_harris > sim_trump, "Kamala Harris", "Donald Trump")
  
  # Handle ties randomly
  ties <- which(sim_harris == sim_trump)
  if(length(ties) > 0){
    state_winners[ties] <- sample(c("Kamala Harris", "Donald Trump"), length(ties), replace = TRUE)
  }
  
  # Sum electoral votes
  trump_evs <- sum(state_data$electoral_votes[state_winners == "Donald Trump"])
  harris_evs <- sum(state_data$electoral_votes[state_winners == "Kamala Harris"])
  
  # Determine overall winner
  overall_winner <- ifelse(harris_evs > trump_evs, "Kamala Harris", "Donald Trump")
  
  return(tibble(trump_evs = trump_evs, harris_evs = harris_evs, winner = overall_winner))
}

# Run simulations
for(i in 1:n_simulations){
  sim <- simulate_one_election(state_poll_evs)
  simulation_results <- bind_rows(simulation_results, sim %>% mutate(simulation = i))
  
  # Optional: Print progress every 100 simulations
  if(i %% 100 == 0){
    cat("Completed", i, "simulations\n")
  }
}

# Calculate win probabilities
win_probabilities <- simulation_results %>%
  group_by(winner) %>%
  summarise(
    probability = (n() / n_simulations) * 100,
    avg_evs = mean(if_else(winner == "Donald Trump", trump_evs, harris_evs)),
    median_evs = median(if_else(winner == "Donald Trump", trump_evs, harris_evs)),
    .groups = 'drop'
  )

print("Monte Carlo Simulation Results:")
print(win_probabilities)

# Save simulation results
write_csv(win_probabilities, "outputs/simulations/win_probabilities.csv")

# Plot win probabilities
simulation_prob_plot <- ggplot(win_probabilities, aes(x = reorder(winner, probability), y = probability, fill = winner)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Monte Carlo Simulation: Probability of Winning the Electoral College",
    x = "Candidate",
    y = "Win Probability (%)",
    fill = "Candidate"
  ) +
  theme_minimal()

# Save simulation probability plot
ggsave("plots/simulations/monte_carlo_win_probabilities.png", simulation_prob_plot, width = 10, height = 6)

#### Conclusion ####

cat("Comprehensive election prediction completed successfully.\n")
cat("Summary Statistics:\n")
print(summary_stats)
cat("Model Summary:\n")
print(model_summary)
cat("Monte Carlo Simulation Results:\n")
print(win_probabilities)
cat("Visualizations are saved in the 'plots' directory.\n")
