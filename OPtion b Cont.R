# Explore Physical Capability Data Structure
cat("\n========== PHYSICAL CAPABILITY DATA EXPLORATION ==========\n")

# Basic structure
glimpse(phys_cap)

# Check date format and clean
phys_cap <- phys_cap %>%
  mutate(
    date = dmy(testDate),
    movement = tolower(movement)
  ) %>%
  filter(!is.na(date), !is.na(benchmarkPct))

# What types of movements do we have?
cat("\n========== MOVEMENT TYPES ==========\n")
phys_cap %>% 
  count(movement, sort = TRUE) %>%
  print()

# Summary statistics by movement
cat("\n========== MOVEMENT SUMMARIES ==========\n")
phys_cap %>%
  group_by(movement) %>%
  summarise(
    n_tests = n(),
    mean_pct = round(mean(benchmarkPct, na.rm = TRUE), 1),
    median_pct = round(median(benchmarkPct, na.rm = TRUE), 1),
    min_pct = round(min(benchmarkPct, na.rm = TRUE), 1),
    max_pct = round(max(benchmarkPct, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  print()

# Date range for capability testing
cat("\n========== DATE RANGE ==========\n")
cat("Physical capability testing from:", 
    as.character(min(phys_cap$date)), "to", 
    as.character(max(phys_cap$date)), "\n")
cat("Total testing days:", length(unique(phys_cap$date)), "\n")






#linking post amtch to dates 
# Create Post-Match Recovery Analysis for Overlap Period
cat("\n========== POST-MATCH RECOVERY ANALYSIS ==========\n")

# Filter matches to overlap period only
overlap_matches <- match_days %>%
  filter(date >= as.Date("2023-07-02"), date <= as.Date("2025-03-12"))

cat("Overlap matches available:", nrow(overlap_matches), "\n")

# Create recovery windows for overlap matches
create_recovery_windows <- function(match_date, capability_data, window_days = 14) {
  
  recovery_period <- capability_data %>%
    filter(
      date > match_date,  # After the match
      date <= (match_date + window_days)  # Within recovery window
    ) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
  
  return(recovery_period)
}

# Process first 5 overlap matches to test
cat("\nTesting recovery windows for first 5 matches...\n")

recovery_data <- map_dfr(1:min(5, nrow(overlap_matches)), function(i) {
  match_date <- overlap_matches$date[i]
  create_recovery_windows(match_date, daily_capability, window_days = 14)
})

cat("Recovery data created:\n")
cat("Matches processed:", length(unique(recovery_data$match_date)), "\n")
cat("Total recovery observations:", nrow(recovery_data), "\n")
cat("Movements tracked:", paste(unique(recovery_data$movement), collapse = ", "), "\n")

# Show example recovery trajectory
if(nrow(recovery_data) > 0) {
  cat("\n========== EXAMPLE RECOVERY TRAJECTORY ==========\n")
  
  first_match <- min(recovery_data$match_date)
  cat("Match date:", as.character(first_match), "\n")
  
  # Show recovery pattern for each movement
  recovery_example <- recovery_data %>%
    filter(match_date == first_match) %>%
    select(movement, days_post_match, daily_score) %>%
    arrange(movement, days_post_match)
  
  if(nrow(recovery_example) > 0) {
    print(recovery_example)
  } else {
    cat("No recovery data found for this match - checking next match...\n")
  }
  
  # Summary stats
  cat("\n========== RECOVERY DATA SUMMARY ==========\n")
  recovery_data %>%
    group_by(movement) %>%
    summarise(
      n_observations = n(),
      mean_score = round(mean(daily_score), 3),
      recovery_days_tracked = n_distinct(days_post_match),
      .groups = "drop"
    ) %>%
    print()
  
} else {
  cat("No recovery data found. Let's check why...\n")
  
  # Debug: check specific match
  test_match <- overlap_matches$date[1]
  cat("Testing match date:", as.character(test_match), "\n")
  
  # Check what capability data exists after this match
  post_match_caps <- daily_capability %>%
    filter(date > test_match, date <= (test_match + 14)) %>%
    arrange(date)
  
  cat("Capability tests after this match:", nrow(post_match_caps), "\n")
  if(nrow(post_match_caps) > 0) {
    head(post_match_caps) %>% print()
  }
}








# Granular Physical Capability Analysis - All Movement/Quality/Expression Combinations
cat("\n========== GRANULAR CAPABILITY ANALYSIS ==========\n")

# Explore the full structure of capability data
cat("Full capability data structure:\n")
glimpse(phys_cap)

# Create unique capability combinations
capability_combinations <- phys_cap %>%
  select(movement, quality, expression) %>%
  distinct() %>%
  arrange(movement, quality, expression)

cat("\nTotal unique movement/quality/expression combinations:", nrow(capability_combinations), "\n\n")

# Show all combinations
cat("All capability combinations available:\n")
capability_combinations %>%
  mutate(
    capability_id = paste(movement, quality, expression, sep = "_")
  ) %>%
  print(n = 50)

# Create daily scores for ALL capability combinations
daily_capability_granular <- phys_cap %>%
  mutate(
    capability_id = paste(movement, quality, expression, sep = "_")
  ) %>%
  group_by(date, capability_id, movement, quality, expression) %>%
  summarise(
    daily_score = mean(benchmarkPct),
    n_tests = n(),
    .groups = "drop"
  )

# Summary statistics for each capability combination
cat("\n========== CAPABILITY COMBINATION SUMMARY ==========\n")
capability_summary <- daily_capability_granular %>%
  group_by(capability_id, movement, quality, expression) %>%
  summarise(
    testing_days = n(),
    mean_score = round(mean(daily_score), 3),
    min_score = round(min(daily_score), 3),
    max_score = round(max(daily_score), 3),
    .groups = "drop"
  ) %>%
  arrange(movement, quality, expression)

print(capability_summary)

# Check data availability by movement category
cat("\n========== DATA AVAILABILITY BY MOVEMENT ==========\n")
movement_summary <- daily_capability_granular %>%
  group_by(movement) %>%
  summarise(
    unique_capabilities = n_distinct(capability_id),
    total_observations = n(),
    testing_days = n_distinct(date),
    avg_score = round(mean(daily_score), 3),
    .groups = "drop"

    
    
    
    
    
    
    
    # Granular Physical Capability Analysis - All Movement/Quality/Expression Combinations
    cat("\n========== GRANULAR CAPABILITY ANALYSIS ==========\n")
    
    # Explore the full structure of capability data
    cat("Full capability data structure:\n")
    glimpse(phys_cap)
    
    # Create unique capability combinations
    capability_combinations <- phys_cap %>%
      select(movement, quality, expression) %>%
      distinct() %>%
      arrange(movement, quality, expression)
    
    cat("\nTotal unique movement/quality/expression combinations:", nrow(capability_combinations), "\n\n")
    
    # Show all combinations
    cat("All capability combinations available:\n")
    capability_combinations %>%
      mutate(
        capability_id = paste(movement, quality, expression, sep = "_")
      ) %>%
      print(n = 50)
    
    # Create daily scores for ALL capability combinations
    daily_capability_granular <- phys_cap %>%
      mutate(
        capability_id = paste(movement, quality, expression, sep = "_")
      ) %>%
      group_by(date, capability_id, movement, quality, expression) %>%
      summarise(
        daily_score = mean(benchmarkPct),
        n_tests = n(),
        .groups = "drop"
      )
    
    # Summary statistics for each capability combination
    cat("\n========== CAPABILITY COMBINATION SUMMARY ==========\n")
    capability_summary <- daily_capability_granular %>%
      group_by(capability_id, movement, quality, expression) %>%
      summarise(
        testing_days = n(),
        mean_score = round(mean(daily_score), 3),
        min_score = round(min(daily_score), 3),
        max_score = round(max(daily_score), 3),
        .groups = "drop"
      ) %>%
      arrange(movement, quality, expression)
    
    print(capability_summary)
    
    # Check data availability by movement category
    cat("\n========== DATA AVAILABILITY BY MOVEMENT ==========\n")
    movement_summary <- daily_capability_granular %>%
      group_by(movement) %>%
      summarise(
        unique_capabilities = n_distinct(capability_id),
        total_observations = n(),
        testing_days = n_distinct(date),
        avg_score = round(mean(daily_score), 3),
        .groups = "drop"
      )        
    
    
    
    
    # Continue with the recovery analysis from where it left off
print(movement_summary)

# Check data availability by expression type
cat("\n========== DATA AVAILABILITY BY EXPRESSION ==========\n")
expression_summary <- daily_capability_granular %>%
  group_by(expression) %>%
  summarise(
    unique_capabilities = n_distinct(capability_id),
    total_observations = n(),
    avg_score = round(mean(daily_score), 3),
    .groups = "drop"
  )

print(expression_summary)

# Create overlap matches
overlap_matches <- match_days %>%
  filter(date >= as.Date("2023-07-02"), date <= as.Date("2025-03-12"))

cat("\n========== POST-MATCH RECOVERY WINDOWS (GRANULAR) ==========\n")
cat("Overlap matches available:", nrow(overlap_matches), "\n")

# Create recovery function for granular data
create_granular_recovery <- function(match_date, capability_data, window_days = 14) {
  
  recovery_period <- capability_data %>%
    filter(
      date > match_date,
      date <= (match_date + window_days)
    ) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
  
  return(recovery_period)
}

# Process first 10 matches with granular capabilities
cat("Processing granular recovery data for first 10 matches...\n")

recovery_data_granular <- map_dfr(1:min(10, nrow(overlap_matches)), function(i) {
  match_date <- overlap_matches$date[i]
  create_granular_recovery(match_date, daily_capability_granular, window_days = 14)
})

cat("\n========== GRANULAR RECOVERY DATASET SUMMARY ==========\n")
cat("Matches processed:", length(unique(recovery_data_granular$match_date)), "\n")
cat("Total recovery observations:", nrow(recovery_data_granular), "\n")
cat("Unique capabilities tracked:", n_distinct(recovery_data_granular$capability_id), "\n")

# Show capabilities tracked in recovery
recovery_capabilities <- recovery_data_granular %>%
  group_by(capability_id, movement, quality, expression) %>%
  summarise(
    n_observations = n(),
    mean_score = round(mean(daily_score), 3),
    matches_tracked = n_distinct(match_date),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observations))

print(recovery_capabilities)
        










# Process FULL Dataset - All 93 Overlap Matches
cat("\n========== PROCESSING FULL RECOVERY DATASET ==========\n")

# Get all overlap matches
overlap_matches <- match_days %>%
  filter(date >= as.Date("2023-07-02"), date <= as.Date("2025-03-12"))

cat("Total overlap matches to process:", nrow(overlap_matches), "\n")

# Process ALL matches (this may take a moment)
cat("Processing all", nrow(overlap_matches), "matches...\n")
cat("This will create the complete recovery dataset for time series analysis.\n\n")

# Create recovery function
create_granular_recovery <- function(match_date, capability_data, window_days = 14) {
  recovery_period <- capability_data %>%
    filter(
      date > match_date,
      date <= (match_date + window_days)
    ) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
  return(recovery_period)
}

# Process ALL overlap matches
recovery_data_full <- map_dfr(1:nrow(overlap_matches), function(i) {
  if(i %% 10 == 0) cat("Processed", i, "matches...\n")  # Progress indicator
  match_date <- overlap_matches$date[i]
  create_granular_recovery(match_date, daily_capability_granular, window_days = 14)
})

cat("\n========== FULL RECOVERY DATASET SUMMARY ==========\n")
cat("Total matches processed:", length(unique(recovery_data_full$match_date)), "\n")
cat("Total recovery observations:", nrow(recovery_data_full), "\n")
cat("Unique capabilities tracked:", n_distinct(recovery_data_full$capability_id), "\n")
cat("Date range:", as.character(min(recovery_data_full$date)), 
    "to", as.character(max(recovery_data_full$date)), "\n\n")

# Summary by capability with full dataset
full_recovery_summary <- recovery_data_full %>%
  group_by(capability_id, movement, quality, expression) %>%
  summarise(
    n_observations = n(),
    mean_score = round(mean(daily_score), 3),
    matches_tracked = n_distinct(match_date),
    date_range_days = as.numeric(max(date) - min(date)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observations))

cat("========== FULL CAPABILITY RECOVERY SUMMARY ==========\n")
print(full_recovery_summary)

# Check data completeness for time series modeling
cat("\n========== TIME SERIES READINESS ==========\n")
ts_ready_capabilities <- full_recovery_summary %>%
  filter(
    n_observations >= 100,  # Minimum for decent time series
    matches_tracked >= 20   # Good match coverage
  )

cat("Capabilities ready for time series forecasting:\n")
print(ts_ready_capabilities)

# Save the full dataset for analysis
cat("\n========== SAVING FULL DATASET ==========\n")
write_csv(recovery_data_full, "recovery_data_full.csv")
cat("Full recovery dataset saved as 'recovery_data_full.csv'\n")
cat("This is your main dataset for time series forecasting analysis.\n")



# Verify the 93 Overlap Matches
cat("\n========== VERIFYING 93 OVERLAP MATCHES ==========\n")

# Recreate overlap_matches to check
overlap_matches <- match_days %>%
  filter(date >= as.Date("2023-07-02"), date <= as.Date("2025-03-12"))

cat("Total overlap matches:", nrow(overlap_matches), "\n")
cat("Date range:", as.character(min(overlap_matches$date)), 
    "to", as.character(max(overlap_matches$date)), "\n\n")

# Show the actual matches
cat("All 93 overlap matches:\n")
overlap_matches %>%
  select(date, opposition_full, season) %>%
  arrange(date) %>%
  mutate(match_number = row_number()) %>%
  print(n = 100)  # Print all matches

# Check if these matches actually have post-match capability data
cat("\n========== CHECKING POST-MATCH DATA AVAILABILITY ==========\n")

# For each match, check if there's capability data in the following 14 days
match_data_check <- overlap_matches %>%
  mutate(
    has_post_match_data = map_lgl(date, function(match_date) {
      post_match_caps <- daily_capability_granular %>%
        filter(date > match_date, date <= (match_date + 14))
      return(nrow(post_match_caps) > 0)
    })
  )

cat("Matches with post-match capability data:", sum(match_data_check$has_post_match_data), "out of", nrow(match_data_check), "\n")

# Show which matches DON'T have post-match data
matches_without_data <- match_data_check %>%
  filter(!has_post_match_data) %>%
  select(date, opposition_full)

if(nrow(matches_without_data) > 0) {
  cat("\nMatches WITHOUT post-match capability data:\n")
  print(matches_without_data)
} else {
  cat("\nAll 93 matches have post-match capability data available!\n")
}






# Analyze Days Between the 93 Matches
cat("\n========== DAYS BETWEEN MATCHES ANALYSIS ==========\n")

# Calculate intervals between consecutive matches
match_intervals <- overlap_matches %>%
  arrange(date) %>%
  mutate(
    next_match_date = lead(date),
    days_to_next_match = as.numeric(next_match_date - date)
  ) %>%
  filter(!is.na(days_to_next_match))  # Remove last match (no next match)

cat("Total match intervals analyzed:", nrow(match_intervals), "\n\n")

# Summary statistics
cat("SUMMARY STATISTICS:\n")
cat("Mean days between matches:", round(mean(match_intervals$days_to_next_match), 1), "\n")
cat("Median days between matches:", median(match_intervals$days_to_next_match), "\n")
cat("Minimum days between matches:", min(match_intervals$days_to_next_match), "\n")
cat("Maximum days between matches:", max(match_intervals$days_to_next_match), "\n")
cat("Standard deviation:", round(sd(match_intervals$days_to_next_match), 1), "\n\n")

# Distribution of intervals
cat("DISTRIBUTION OF DAYS BETWEEN MATCHES:\n")
intervals_table <- match_intervals %>%
  count(days_to_next_match, sort = TRUE) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    cumulative_pct = round(cumsum(n) / sum(n) * 100, 1)
  )

print(intervals_table)

# Key insights
cat("\n========== KEY INSIGHTS ==========\n")
sshort_intervals <- sum(match_intervals$days_to_next_match <= 4)
short_intervals <- sum(match_intervals$days_to_next_match <= 5)
week_intervals <- sum(match_intervals$days_to_next_match == 7)
long_intervals <- sum(match_intervals$days_to_next_match > 10)

cat("Matches with ≤4 days to next match:", short_intervals, 
    "(", round(sshort_intervals/nrow(match_intervals)*100, 1), "%)\n")
cat("Matches with ≤5 days to next match:", short_intervals, 
    "(", round(short_intervals/nrow(match_intervals)*100, 1), "%)\n")
cat("Matches with exactly 7 days to next match:", week_intervals, 
    "(", round(week_intervals/nrow(match_intervals)*100, 1), "%)\n")
cat("Matches with >10 days to next match:", long_intervals, 
    "(", round(long_intervals/nrow(match_intervals)*100, 1), "%)\n\n")

# Show examples of very short intervals (busy periods)
cat("EXAMPLES OF SHORT INTERVALS (≤3 days):\n")
short_examples <- match_intervals %>%
  filter(days_to_next_match <= 3) %>%
  
  
  
  
  
  
  # Adjusted Recovery Strategy - 3-Day Window
  cat("\n========== ADJUSTED RECOVERY STRATEGY ==========\n")
cat("Based on match frequency analysis:\n")
cat("- 50% of matches have ≤4 days to next match\n")
cat("- Using 3-day recovery window to avoid next match contamination\n")
cat("- This captures immediate post-match recovery only\n\n")

# Recreate recovery data with 3-day window
create_short_recovery <- function(match_date, capability_data, window_days = 3) {
  recovery_period <- capability_data %>%
    filter(
      date > match_date,
      date <= (match_date + window_days)
    ) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
  return(recovery_period)
}

cat("Processing 3-day recovery windows for all 93 matches...\n")

# Process with shorter window
recovery_data_3day <- map_dfr(1:nrow(overlap_matches), function(i) {
  if(i %% 20 == 0) cat("Processed", i, "matches...\n")
  match_date <- overlap_matches$date[i]
  create_short_recovery(match_date, daily_capability_granular, window_days = 3)
})

cat("\n========== 3-DAY RECOVERY DATASET SUMMARY ==========\n")
cat("Total recovery observations:", nrow(recovery_data_3day), "\n")
cat("Matches processed:", length(unique(recovery_data_3day$match_date)), "\n")
cat("Unique capabilities:", n_distinct(recovery_data_3day$capability_id), "\n")

# Summary by capability
recovery_3day_summary <- recovery_data_3day %>%
  group_by(capability_id, movement, quality, expression) %>%
  summarise(
    n_observations = n(),
    mean_score = round(mean(daily_score), 3),
    matches_tracked = n_distinct(match_date),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observations))

print(recovery_3day_summary)

# Check data completeness for 3-day windows
cat("\n========== 3-DAY WINDOW DATA COMPLETENESS ==========\n")
completeness_3day <- recovery_data_3day %>%
  group_by(match_date) %>%
  summarise(
    total_capabilities = n_distinct(capability_id),
    total_observations = n(),
    days_with_data = n_distinct(days_post_match),
    .groups = "drop"
  )

cat("Average capabilities per match:", round(mean(completeness_3day$total_capabilities), 1), "\n")
cat("Average days with data per match:", round(mean(completeness_3day$days_with_data), 1), "\n")
cat("Matches with all 3 days of data:", sum(completeness_3day$days_with_data == 3), "out of", nrow(completeness_3day), "\n")

# This gives us a more realistic dataset for forecasting immediate post-match recovery
cat("\n========== REVISED RESEARCH QUESTION ==========\n")
cat("Can we forecast immediate post-match capability recovery (days 1-3)\n")
cat("using GPS training load to optimize rapid return-to-training protocols?\n\n")
cat("This approach:\n")
cat("✓ Avoids next-match contamination\n")
cat("✓ Focuses on acute recovery patterns\n") 
cat("✓ Provides actionable 72-hour forecasts\n")
cat("✓ Matches real-world coaching decisions\n")
  select(date, opposition_full, next_match_date, days_to_next_match) %>%
  arrange(days_to_next_match)

if(nrow(short_examples) > 0) {
  print(short_examples)
} else {
  cat("No matches with ≤3 days intervals found.\n")
}

# Recommended recovery window based on this analysis
cat("\n========== RECOVERY WINDOW RECOMMENDATION ==========\n")
median_interval <- median(match_intervals$days_to_next_match)
recommended_window <- min(5, floor(median_interval * 0.7))

cat("Based on match frequency:\n")
cat("Median interval:", median_interval, "days\n")
cat("Recommended recovery window:", recommended_window, "days\n")
cat("This captures pure recovery without interference from next match preparation."\n))





# Check what data objects exist
cat("Checking available data objects:\n")

# Check if recovery_data_3day exists
if(exists("recovery_data_3day")) {
  cat("recovery_data_3day exists with", nrow(recovery_data_3day), "rows\n")
  cat("Unique capabilities available:\n")
  print(unique(recovery_data_3day$capability_id))
} else {
  cat("recovery_data_3day does not exist\n")
  cat("Need to recreate the 3-day recovery dataset\n")
}

# Check if daily_capability_granular exists
if(exists("daily_capability_granular")) {
  cat("\ndaily_capability_granular exists with", nrow(daily_capability_granular), "rows\n")
} else {
  cat("\ndaily_capability_granular does not exist\n")
}

# Check if overlap_matches exists
if(exists("overlap_matches")) {
  cat("overlap_matches exists with", nrow(overlap_matches), "rows\n")
} else {
  cat("overlap_matches does not exist\n")
}







# Fixed Time Series with Correct Capability Name
cat("Building time series with correct capability name\n")

# Use the correct capability name (with space)
target_capability <- "jump_take off_dynamic"
cat("Target capability:", target_capability, "\n")

# Filter data
ts_data_3day <- recovery_data_3day %>%
  filter(capability_id == target_capability) %>%
  arrange(date) %>%
  select(date, daily_score, match_date, days_post_match)

cat("Filtered observations:", nrow(ts_data_3day), "\n")

# Create daily time series
daily_ts_3day <- ts_data_3day %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(daily_score, .direction = "down") %>%
  filter(!is.na(daily_score)) %>%
  as_tsibble(index = date)

cat("Daily time series created with", nrow(daily_ts_3day), "observations\n")

# Basic visualization
plot_ts <- daily_ts_3day %>%
  autoplot(daily_score) +
  labs(
    title = "Jump Takeoff Recovery - 3-Day Post-Match Windows",
    x = "Date",
    y = "Capability Score (% baseline)"
  ) +
  theme_minimal()

print(plot_ts)

# Train/test split
split_point <- floor(nrow(daily_ts_3day) * 0.8)
split_date <- daily_ts_3day$date[split_point]

train_data <- daily_ts_3day %>% filter(date <= split_date)
test_data <- daily_ts_3day %>% filter(date > split_date)

cat("Training observations:", nrow(train_data), "\n")
cat("Test observations:", nrow(test_data), "\n")

# Build models using fpp3 methods
models <- train_data %>%
  model(
    naive = NAIVE(daily_score),
    mean_fc = MEAN(daily_score),
    drift = RW(daily_score ~ drift()),
    arima = ARIMA(daily_score),
    ets = ETS(daily_score),
    linear = TSLM(daily_score ~ trend())
  )

# Generate forecasts
forecasts <- models %>% forecast(h = nrow(test_data))

# Check accuracy
accuracy_results <- forecasts %>%
  accuracy(test_data) %>%
  arrange(RMSE)

cat("Model accuracy results:\n")
print(accuracy_results)

# Plot forecasts
forecast_plot <- forecasts %>%
  autoplot(daily_ts_3day, level = 95) +
  labs(
    title = "Jump Takeoff Recovery Forecasting",
    x = "Date",
    y = "Capability Score"
  ) +
  theme_minimal()

print(forecast_plot)
