#### Preamble ####
# Purpose: Simulates a dataset of Australian electoral divisions, including the 
  #state and party that won each division.
# Author: Harsh M Pareek, Arshh Relan, Benji Feurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, relanarshh@gmail.com
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
# 00-simulate_data.R

# Load necessary packages
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Number of polls to simulate
num_polls <- 100

# Possible values
pollster_ids <- 1:10
pollsters <- paste("Pollster", pollster_ids)
states <- c(NA, state.name)  # All US states
methodologies <- c("Live Phone", "Online Panel", "IVR/Online", "IVR/Live Phone", "Online")
population_types <- c("lv", "rv", "a")
candidates <- c("Donald Trump", "Kamala Harris", "Jill Stein", "Cornel West", "Chase Oliver", "Robert F. Kennedy")
candidate_ids <- c(16651, 16661, 31116, 31097, 31790, 31042)

# Initialize empty data frame
poll_data <- data.frame()

for (i in 1:num_polls) {
  poll_id <- i
  pollster_id <- sample(pollster_ids, 1)
  pollster <- pollsters[pollster_id]
  start_date <- as.Date("2024-10-01") + sample(0:30, 1)
  end_date <- start_date + sample(1:5, 1)
  sample_size <- sample(500:2000, 1)
  methodology <- sample(methodologies, 1)
  population <- sample(population_types, 1)
  state <- sample(states, 1)
  
  # Generate percentages for candidates
  percentages <- runif(length(candidates), min=0, max=1)
  percentages <- percentages / sum(percentages) * 100
  
  # Create rows for each candidate
  poll_rows <- data.frame(
    poll_id = poll_id,
    pollster_id = pollster_id,
    pollster = pollster,
    start_date = start_date,
    end_date = end_date,
    sample_size = sample_size,
    methodology = methodology,
    population = population,
    state = state,
    candidate_name = candidates,
    candidate_id = candidate_ids,
    pct = percentages
  )
  
  poll_data <- rbind(poll_data, poll_rows)
}

# Save simulated data to CSV
dir.create("data", showWarnings = FALSE)
write.csv(poll_data, "data/00-simulated_data/simulated_polls.csv", row.names=FALSE)
