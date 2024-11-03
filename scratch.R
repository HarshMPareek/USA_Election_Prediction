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

glimpse(polling_data)

electoral_votes <- tibble(
  state = c(state.name, "District of Columbia"),
  electoral_votes = c(
    55, 29, 20, 20, 20, 29, 20, 20, 20, 20, 
    20, 20, 11, 20, 20, 20, 20, 20, 20, 20, 
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 
    3
  )
)

# Verify the tibble has 51 entries
print(nrow(electoral_votes))  # Should print 51
print(electoral_votes)