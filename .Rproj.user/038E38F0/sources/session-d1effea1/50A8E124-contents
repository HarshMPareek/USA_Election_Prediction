#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Harsh M Pareek, Arshh Relan, Benji Feurence
# Date: 22 October 2024
# Contact: harsh.pareek@mail.utoronto.ca, b.fleurence@mail.utoronto.ca, relanarshh@gmail.com
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(ggplot2)
library(dplyr)
library(readr)
library(scales)

# Read the cleaned data from CSV
polling_data <- read_csv("data/02-analysis_data/cleaned_polling_data.csv", show_col_types = FALSE)

#### EDA ####

# Summary statistics by candidate
summary_stats <- polling_data %>%
  group_by(candidate_name) %>%
  summarise(
    mean_pct = mean(pct),
    sd_pct = sd(pct),
    min_pct = min(pct),
    max_pct = max(pct),
    n_polls = n()
  ) %>%
  arrange(desc(n_polls))

print(summary_stats)

# Save summary statistics to a CSV file
write_csv(summary_stats, "data/02-analysis_data/summary_stats_by_candidate.csv")

#### Visualizations ####

# Create plots directory if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Plot candidate percentages over time using facets
ggplot(polling_data, aes(x = end_date, y = pct)) +
  geom_point(alpha = 0.5, size = 1, color = "blue") +
  geom_smooth(
    method = 'loess',
    se = FALSE,
    span = 0.5,
    na.rm = TRUE
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Candidate Poll Percentages Over Time",
       x = "End Date of Poll",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ candidate_name, scales = "free_y")

# Save plot
ggsave("plots/candidate_percentages_over_time.png", width = 12, height = 10)

# Histogram of polls by state (Top 20 states)
polling_data %>%
  count(state) %>%
  arrange(desc(n)) %>%
  top_n(20, n) %>%
  ggplot(aes(x = reorder(state, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  coord_flip() +
  labs(title = "Top 20 States by Number of Polls",
       x = "State",
       y = "Number of Polls") +
  theme_minimal()

# Save plot
ggsave("plots/polls_by_state.png", width = 8, height = 6)

# Distribution of percentages per candidate
ggplot(polling_data, aes(x = pct, fill = candidate_name)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.6) +
  labs(title = "Distribution of Poll Percentages per Candidate",
       x = "Percentage",
       y = "Count",
       fill = "Candidate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot
ggsave("plots/percentage_distribution_per_candidate.png", width = 10, height = 6)
