#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Harsh M Pareek, Arshh Relan, Benji Fleurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, arshh.relan@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)  # Added for pivot_wider

# Read in the data from CSV
polling_data <- read_csv("data/01-raw_data/president_polls.csv", show_col_types = FALSE)

# Check initial number of rows
cat("Initial number of rows:", nrow(polling_data), "\n")

# Filter for general election polls
polling_data <- polling_data %>%
  filter(
    office_type == "U.S. President",
    stage == "general",
    !is.na(pct)
  )

# Check the number of rows after filtering for general election polls
cat("Rows after filtering for general election polls:", nrow(polling_data), "\n")

#### Candidate Name Cleaning ####

# Standardize candidate names, including missing ones
polling_data <- polling_data %>%
  mutate(
    candidate_name = case_when(
      candidate_name %in% c("Donald Trump Jr.", "Donald J. Trump") ~ "Donald Trump",
      candidate_name %in% c("Joseph R. Biden", "Joe Biden Jr.") ~ "Joe Biden",
      candidate_name %in% c("Kamala D. Harris") ~ "Kamala Harris",
      is.na(candidate_name) ~ "Undecided",
      candidate_name %in% c("Other", "No Answer", "Refused", "Undecided") ~ "Other",
      TRUE ~ candidate_name
    )
  )

#### Handle Missing Values in Key Columns ####

# Remove rows with missing values in key columns
polling_data <- polling_data %>%
  filter(
    !is.na(sample_size),
    !is.na(methodology),
    !is.na(start_date),
    !is.na(end_date),
    !is.na(election_date)
  )

# Check the number of rows after handling missing values
cat("Rows after handling missing values:", nrow(polling_data), "\n")

#### Date Conversion ####

# Convert date columns to Date type using lubridate's mdy()
polling_data <- polling_data %>%
  mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),
    election_date = mdy(election_date)
  )

#### Additional Cleaning Steps ####

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

# Remove rows with negative days until election (if any)
polling_data <- polling_data %>%
  filter(days_until_election >= 0)

#### Handling Polls Where Percentages Don't Sum to 100 ####

# Normalize percentages within each poll
polling_data <- polling_data %>%
  group_by(poll_id) %>%
  mutate(
    total_pct = sum(pct, na.rm = TRUE),
    pct_normalized = (pct / total_pct) * 100
  ) %>%
  ungroup()

# Check the number of rows after handling total percentages
cat("Rows after handling total percentages:", nrow(polling_data), "\n")

#### Define Electoral College Data ####

# Define the number of electoral votes per state for 2024
# Ensure these numbers are accurate based on the latest census data
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

#### Save Cleaned Data ####

# Ensure the directory exists
dir.create("data/02-analysis_data", showWarnings = FALSE, recursive = TRUE)

# Save the cleaned data back to CSV
write_csv(polling_data, "data/02-analysis_data/cleaned_polling_data.csv")
