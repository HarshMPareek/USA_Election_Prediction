# -----------------------------
# 1. Workspace Setup
# -----------------------------

# Define required packages
required_packages <- c("tidyverse", "lubridate", "sf", "ggplot2", "leaflet", "tigris", "htmlwidgets")

# Function to install missing packages
install_if_missing <- function(packages) {
  installed <- rownames(installed.packages())
  for (pkg in packages) {
    if (!(pkg %in% installed)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)
library(leaflet)
library(tigris)
library(htmlwidgets)

# -----------------------------
# 2. Data Loading and Cleaning
# -----------------------------

# Read in the raw polling data from CSV
# Replace the file path with your actual data path
polling_data <- read_csv("data/01-raw_data/president_polls.csv", show_col_types = FALSE)

# Initial number of rows
cat("Initial number of rows:", nrow(polling_data), "\n")

# Filter for general election polls for U.S. President with non-missing percentages
polling_data <- polling_data %>%
  filter(
    office_type == "U.S. President",
    stage == "general",
    !is.na(pct)
  )

# Rows after filtering
cat("Rows after filtering for general election polls:", nrow(polling_data), "\n")

# Standardize candidate names
polling_data <- polling_data %>%
  mutate(
    candidate_name = case_when(
      candidate_name %in% c("Donald Trump Jr.", "Donald J. Trump") ~ "Donald Trump",
      candidate_name %in% c("Joseph R. Biden", "Joe Biden Jr.") ~ "Joe Biden",
      candidate_name %in% c("Kamala D. Harris") ~ "Kamala Harris",
      candidate_name %in% c("Robert F. Kennedy") ~ "Robert F. Kennedy",
      candidate_name %in% c("Jill Stein") ~ "Jill Stein",
      is.na(candidate_name) ~ "Undecided",
      candidate_name %in% c("Other", "No Answer", "Refused", "Undecided") ~ "Other",
      TRUE ~ candidate_name
    )
  )

# Remove rows with missing values in key columns
polling_data <- polling_data %>%
  filter(
    !is.na(sample_size),
    !is.na(methodology),
    !is.na(start_date),
    !is.na(end_date),
    !is.na(election_date)
  )

# Rows after removing missing values
cat("Rows after handling missing values:", nrow(polling_data), "\n")

# Convert date columns to Date type
polling_data <- polling_data %>%
  mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),
    election_date = mdy(election_date)
  )

# Ensure percentages are between 0 and 100
polling_data <- polling_data %>%
  filter(pct >= 0 & pct <= 100)

# Fill missing or empty state with 'National'
polling_data <- polling_data %>%
  mutate(
    state = if_else(is.na(state) | state == "", "National", state)
  )

# Create a variable for days until election
polling_data <- polling_data %>%
  mutate(
    days_until_election = as.numeric(election_date - end_date)
  )

# Remove rows with negative days until election
polling_data <- polling_data %>%
  filter(days_until_election >= 0)

# Normalize percentages within each poll
polling_data <- polling_data %>%
  group_by(poll_id) %>%
  mutate(
    total_pct = sum(pct, na.rm = TRUE),
    pct_normalized = (pct / total_pct) * 100
  ) %>%
  ungroup()

# Rows after normalization
cat("Rows after handling total percentages:", nrow(polling_data), "\n")

# -----------------------------
# 3. Defining Electoral College Data Correctly
# -----------------------------

# Define the number of electoral votes per state for 2024 based on the 2020 Census
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

# Verify the tibble has 51 entries
cat("Number of entries in electoral_votes:", nrow(electoral_votes), "\n")  # Should print 51
print(electoral_votes)

# -----------------------------
# 4. Merging Polling Data with Electoral Votes
# -----------------------------

# Define candidates of interest
candidates_of_interest <- c("Donald Trump", "Kamala Harris")

# Filter data for only the candidates of interest
cleaned_data_candidates <- polling_data %>%
  filter(candidate_name %in% candidates_of_interest)

# Calculate average polling percentage per state and candidate (long format)
state_poll_avg_long <- cleaned_data_candidates %>%
  group_by(state, candidate_name) %>%
  summarise(avg_pct = mean(pct_normalized, na.rm = TRUE), .groups = "drop")

# Calculate national averages for each candidate
national_avg <- cleaned_data_candidates %>%
  group_by(candidate_name) %>%
  summarise(national_avg = mean(pct_normalized, na.rm = TRUE), .groups = "drop")

# Merge national averages into state_poll_avg_long to handle missing values
state_poll_avg_long <- state_poll_avg_long %>%
  left_join(national_avg, by = "candidate_name") %>%
  mutate(
    avg_pct = if_else(is.na(avg_pct), national_avg, avg_pct)
  ) %>%
  select(state, candidate_name, avg_pct)

# Pivot to wide format
state_poll_avg <- state_poll_avg_long %>%
  pivot_wider(names_from = candidate_name, values_from = avg_pct)

# Merge with electoral votes
state_poll_avg <- state_poll_avg %>%
  left_join(electoral_votes, by = "state")

# Verify that all states have electoral votes
missing_electoral_votes <- state_poll_avg %>%
  filter(is.na(electoral_votes)) %>%
  select(state)

if (nrow(missing_electoral_votes) > 0) {
  cat("States with missing electoral votes after merging:\n")
  print(missing_electoral_votes)
} else {
  cat("All states have corresponding electoral votes after merging.\n")
}

# -----------------------------
# 5. Ensuring No NA in Candidate Columns
# -----------------------------

# Check for NA values in candidate polling percentages
na_kamala <- sum(is.na(state_poll_avg$`Kamala Harris`))
na_trump <- sum(is.na(state_poll_avg$`Donald Trump`))

cat("Number of NA values in Kamala Harris column:", na_kamala, "\n")
cat("Number of NA values in Donald Trump column:", na_trump, "\n")

# Handle NA values by imputing with national averages
state_poll_avg <- state_poll_avg %>%
  mutate(
    `Kamala Harris` = if_else(is.na(`Kamala Harris`), 
                              national_avg$national_avg[national_avg$candidate_name == "Kamala Harris"], 
                              `Kamala Harris`),
    `Donald Trump` = if_else(is.na(`Donald Trump`), 
                             national_avg$national_avg[national_avg$candidate_name == "Donald Trump"], 
                             `Donald Trump`)
  )

# Verify again for any remaining NA
na_kamala_after <- sum(is.na(state_poll_avg$`Kamala Harris`))
na_trump_after <- sum(is.na(state_poll_avg$`Donald Trump`))

cat("Number of NA values in Kamala Harris column after imputation:", na_kamala_after, "\n")
cat("Number of NA values in Donald Trump column after imputation:", na_trump_after, "\n")

# If there are still NA values (unlikely), assign default value of 50
state_poll_avg <- state_poll_avg %>%
  mutate(
    `Kamala Harris` = if_else(is.na(`Kamala Harris`), 50, `Kamala Harris`),
    `Donald Trump` = if_else(is.na(`Donald Trump`), 50, `Donald Trump`)
  )

# Final check
na_kamala_final <- sum(is.na(state_poll_avg$`Kamala Harris`))
na_trump_final <- sum(is.na(state_poll_avg$`Donald Trump`))

cat("Final number of NA values in Kamala Harris column:", na_kamala_final, "\n")
cat("Final number of NA values in Donald Trump column:", na_trump_final, "\n")

# -----------------------------
# 6. Building a Generalized Linear Model (Logistic Regression)
# -----------------------------

# Predict the winner in each state based on higher average polling percentage
state_poll_avg <- state_poll_avg %>%
  mutate(
    predicted_winner = if_else(`Kamala Harris` > `Donald Trump`, "Kamala Harris", "Donald Trump")
  )

# Create a binary outcome: 1 if Kamala Harris wins the state, 0 otherwise
model_data <- state_poll_avg %>%
  mutate(
    winner = if_else(predicted_winner == "Kamala Harris", 1, 0)
  )

# Inspect the model_data
head(model_data)

# Build the logistic regression model
# Predicting the probability of Kamala Harris winning based on her polling percentage
glm_model <- glm(winner ~ `Kamala Harris`, data = model_data, family = binomial)

# Summary of the model
summary(glm_model)

# Predict probabilities
model_data <- model_data %>%
  mutate(
    predicted_prob = predict(glm_model, type = "response"),
    predicted_winner_model = if_else(predicted_prob > 0.5, "Kamala Harris", "Donald Trump")
  )

# -----------------------------
# 7. Electoral Votes Aggregation Based on Model Predictions
# -----------------------------

# Sum electoral votes for each candidate based on model predictions
electoral_result_model <- model_data %>%
  group_by(predicted_winner_model) %>%
  summarise(total_electoral_votes = sum(electoral_votes, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_electoral_votes))

print(electoral_result_model)

# Determine the overall predicted winner based on the model
overall_winner_model <- electoral_result_model %>%
  slice(1) %>%
  pull(predicted_winner_model)

cat("Predicted Election Winner based on GLM:", overall_winner_model, "\n")

# -----------------------------
# 8. Simulation for Uncertainty Estimation
# -----------------------------

# Number of simulations
num_simulations <- 1000

# Initialize a dataframe to store simulation results
simulation_results <- tibble(winner = character(), electoral_votes = integer())

# Function to simulate a single election outcome with error handling
simulate_election <- function(state_poll_avg) {
  sim_winners <- state_poll_avg %>%
    rowwise() %>%
    mutate(
      # Ensure probabilities sum to 1 and handle any potential issues
      prob_kamala = `Kamala Harris` / 100,
      prob_trump = `Donald Trump` / 100,
      prob_sum = prob_kamala + prob_trump,
      prob_kamala = prob_kamala / prob_sum,
      prob_trump = prob_trump / prob_sum,
      # Sample winner based on probabilities
      winner = sample(c("Kamala Harris", "Donald Trump"), 1, prob = c(prob_kamala, prob_trump))
    ) %>%
    ungroup()
  
  # Sum electoral votes for each candidate
  sim_result <- sim_winners %>%
    group_by(winner) %>%
    summarise(total_electoral_votes = sum(electoral_votes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_electoral_votes)) %>%
    slice(1)
  
  return(tibble(winner = sim_result$winner, electoral_votes = sim_result$total_electoral_votes))
}

# Run simulations
for (i in 1:num_simulations) {
  sim <- simulate_election(state_poll_avg)
  simulation_results <- bind_rows(simulation_results, sim)
  
  # Optional: Print progress every 100 simulations
  if (i %% 100 == 0) {
    cat("Completed", i, "simulations\n")
  }
}

# Analyze simulation outcomes
simulation_summary <- simulation_results %>%
  group_by(winner) %>%
  summarise(probability = (n() / num_simulations) * 100, .groups = "drop") %>%
  arrange(desc(probability))

print(simulation_summary)

# -----------------------------
# 9. Geospatial Analysis and Enhanced Visualizations
# -----------------------------

# 9A. Choropleth Map: Predicted State Winners based on GLM

# Load US states shapefile using tigris
options(tigris_use_cache = TRUE) # Cache shapefiles for faster access
states_shapefile <- states(cb = TRUE, resolution = "20m") %>%
  st_transform(crs = 4326) %>%  # Ensure it's in WGS84
  mutate(state = str_to_title(NAME))

# Merge with model polling data
states_map_data_model <- states_shapefile %>%
  left_join(model_data, by = "state")

# Handle "District of Columbia" if necessary
states_map_data_model <- states_map_data_model %>%
  mutate(state = if_else(state == "District Of Columbia", "District of Columbia", state))

# Create a choropleth map using ggplot2 and sf
choropleth_map_model <- ggplot(states_map_data_model) +
  geom_sf(aes(fill = predicted_winner_model), color = "white") +
  scale_fill_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red"),
                    name = "Predicted Winner") +
  theme_minimal() +
  labs(
    title = "Predicted State Winners for the 2024 U.S. Presidential Election (GLM)",
    subtitle = paste("Overall Predicted Winner:", overall_winner_model),
    caption = "Data Source: Polling Data"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the choropleth map
print(choropleth_map_model)

# Save the choropleth map
dir.create("plots", showWarnings = FALSE, recursive = TRUE)  # Ensure the 'plots' directory exists
ggsave("plots/predicted_state_winners_map_GLM.png", plot = choropleth_map_model, width = 12, height = 8)

# 9B. Interactive Leaflet Map: Predicted State Winners based on GLM

# Create a color palette
state_palette_model <- colorFactor(
  palette = c("blue", "red"),
  domain = c("Kamala Harris", "Donald Trump")
)

# Create an interactive leaflet map
leaflet_map_model <- leaflet(states_map_data_model) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~state_palette_model(predicted_winner_model),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste0(
      "<strong>", state, "</strong><br>",
      "Predicted Winner: ", predicted_winner_model, "<br>",
      "Electoral Votes: ", electoral_votes
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  addLegend(
    pal = state_palette_model, 
    values = ~predicted_winner_model,
    opacity = 0.7, 
    title = "Predicted Winner",
    position = "bottomright"
  )

# Display the interactive map
print(leaflet_map_model)

# Save the interactive map as HTML
saveWidget(leaflet_map_model, "plots/predicted_state_winners_map_GLM.html", selfcontained = TRUE)

# 9C. Enhanced Visualizations: Electoral Vote Distribution and Simulation Probabilities

# 9C-1. Electoral Vote Distribution Bar Chart based on GLM
electoral_vote_plot_model <- ggplot(electoral_result_model, 
                                    aes(x = reorder(predicted_winner_model, -total_electoral_votes), 
                                        y = total_electoral_votes, 
                                        fill = predicted_winner_model)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red")) +
  labs(
    title = "Predicted Electoral Votes by Candidate (GLM)",
    x = "Candidate",
    y = "Total Electoral Votes",
    fill = "Candidate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the electoral vote distribution plot
print(electoral_vote_plot_model)

# Save the electoral vote distribution plot
ggsave("plots/predicted_electoral_votes_by_candidate_GLM.png", plot = electoral_vote_plot_model, width = 10, height = 6)

# 9C-2. Simulation Probability Bar Chart
simulation_probability_plot <- ggplot(simulation_summary, 
                                      aes(x = reorder(winner, -probability), 
                                          y = probability, 
                                          fill = winner)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Kamala Harris" = "blue", "Donald Trump" = "red")) +
  labs(
    title = "Simulation-Based Probability of Winning",
    x = "Candidate",
    y = "Probability (%)",
    fill = "Candidate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Display the simulation probability plot
print(simulation_probability_plot)

# Save the simulation probability plot
ggsave("plots/simulation_probability_of_winning_GLM.png", plot = simulation_probability_plot, width = 10, height = 6)

# -----------------------------
# 10. Final Output
# -----------------------------

cat("Comprehensive election prediction completed successfully.\n")
cat("Predicted Electoral Votes based on GLM:\n")
print(electoral_result_model)
cat("Overall Predicted Election Winner based on GLM:", overall_winner_model, "\n")
cat("Simulation Summary:\n")
print(simulation_summary)
cat("Visualizations are saved in the 'plots' directory.\n")
