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








#######




# Add GPS External Regressors to Time Series Models
cat("Adding GPS match intensity as external regressors\n")

# Step 1: Get GPS data for overlap matches
gps_match_data <- overlap_matches %>%
  select(date, distance, distance_over_21, distance_over_24, distance_over_27,
         accel_decel_over_2_5, accel_decel_over_3_5, accel_decel_over_4_5,
         peak_speed, day_duration) %>%
  rename(match_date = date)

cat("GPS match data prepared for", nrow(gps_match_data), "matches\n")

# Step 2: Create recovery data with GPS predictors
recovery_with_gps <- recovery_data_3day %>%
  filter(capability_id == target_capability) %>%
  left_join(gps_match_data, by = "match_date") %>%
  arrange(date) %>%
  select(date, daily_score, match_date, days_post_match, 
         distance, distance_over_27, accel_decel_over_3_5, peak_speed)

cat("Recovery data with GPS predictors:\n")
cat("Observations:", nrow(recovery_with_gps), "\n")
cat("GPS variables added: distance, distance_over_27, accel_decel_over_3_5, peak_speed\n")

# Step 3: Create daily time series with GPS predictors
daily_ts_with_gps <- recovery_with_gps %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  # Fill GPS values forward (match intensity affects following days)
  fill(distance, distance_over_27, accel_decel_over_3_5, peak_speed, .direction = "down") %>%
  fill(daily_score, .direction = "down") %>%
  filter(!is.na(daily_score)) %>%
  as_tsibble(index = date)

cat("Daily time series with GPS created:", nrow(daily_ts_with_gps), "observations\n")

# Step 4: Train/test split (same as before for comparison)
train_gps <- daily_ts_with_gps %>% filter(date <= split_date)
test_gps <- daily_ts_with_gps %>% filter(date > split_date)

cat("Training observations:", nrow(train_gps), "\n")
cat("Test observations:", nrow(test_gps), "\n")

# Step 5: Build models with GPS external regressors
cat("Building models with GPS external regressors...\n")

models_with_gps <- train_gps %>%
  model(
    # Original best model for comparison
    drift_only = RW(daily_score ~ drift()),
    
    # Models with GPS predictors (using TSLM for external regressors)
    gps_distance = TSLM(daily_score ~ distance),
    gps_high_speed = TSLM(daily_score ~ distance_over_27),
    gps_acceleration = TSLM(daily_score ~ accel_decel_over_3_5),
    gps_combined = TSLM(daily_score ~ distance_over_27 + accel_decel_over_3_5),
    
    # Advanced: ARIMA with external regressors
    arima_gps = ARIMA(daily_score ~ distance_over_27 + accel_decel_over_3_5)
  )

# Step 6: Generate forecasts
forecasts_gps <- models_with_gps %>%
  forecast(new_data = test_gps)

# Step 7: Compare accuracy
accuracy_gps <- forecasts_gps %>%
  accuracy(test_gps) %>%
  arrange(RMSE)

cat("Model accuracy with GPS regressors:\n")
print(accuracy_gps)

# Step 8: Visualize improved forecasts
forecast_plot_gps <- forecasts_gps %>%
  autoplot(daily_ts_with_gps, level = 95) +
  labs(
    title = "Jump Takeoff Forecasting: GPS Enhanced Models",
    subtitle = "Comparing models with and without GPS match intensity",
    x = "Date",
    y = "Capability Score"
  ) +
  theme_minimal()

print(forecast_plot_gps)






# Test GPS Predictors on Sprint Max Velocity (Different Movement Pattern)
cat("Testing GPS predictors on sprint capability\n")

# Select sprint capability 
sprint_capability <- "sprint_max velocity_dynamic"
cat("Testing capability:", sprint_capability, "\n")

# Check if this capability has good data coverage
sprint_coverage <- recovery_3day_summary %>%
  filter(capability_id == sprint_capability)
print(sprint_coverage)

# Create time series for sprint capability
sprint_data <- recovery_data_3day %>%
  filter(capability_id == sprint_capability) %>%
  left_join(gps_match_data, by = "match_date") %>%
  arrange(date) %>%
  select(date, daily_score, match_date, days_post_match, 
         distance, distance_over_27, accel_decel_over_3_5, peak_speed)

# Create daily time series
sprint_ts <- sprint_data %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(distance, distance_over_27, accel_decel_over_3_5, peak_speed, .direction = "down") %>%
  fill(daily_score, .direction = "down") %>%
  filter(!is.na(daily_score)) %>%
  as_tsibble(index = date)

cat("Sprint time series created:", nrow(sprint_ts), "observations\n")

# Train/test split (using same split date for comparison)
sprint_train <- sprint_ts %>% filter(date <= split_date)
sprint_test <- sprint_ts %>% filter(date > split_date)

# Build models for sprint
sprint_models <- sprint_train %>%
  model(
    drift_only = RW(daily_score ~ drift()),
    gps_distance = TSLM(daily_score ~ distance),
    gps_high_speed = TSLM(daily_score ~ distance_over_27),
    gps_combined = TSLM(daily_score ~ distance_over_27 + accel_decel_over_3_5),
    arima_gps = ARIMA(daily_score ~ distance_over_27 + accel_decel_over_3_5)
  )

# Generate forecasts
sprint_forecasts <- sprint_models %>%
  forecast(new_data = sprint_test)

# Check accuracy
sprint_accuracy <- sprint_forecasts %>%
  accuracy(sprint_test) %>%
  arrange(RMSE)

cat("Sprint capability - Model accuracy:\n")
print(sprint_accuracy)

# Compare with jump results
cat("\nComparison: Jump vs Sprint GPS predictor performance\n")
cat("Jump - Best model: drift_only (RMSE: 0.00604)\n")
cat("Sprint - Best model:", sprint_accuracy$.model[1], "(RMSE:", round(sprint_accuracy$RMSE[1], 5), ")\n")

# Quick visualization
sprint_plot <- sprint_forecasts %>%
  autoplot(sprint_ts, level = 95) +
  labs(
    title = "Sprint Max Velocity Forecasting",
    subtitle = "Testing GPS predictors on different capability",
    x = "Date",
    y = "Sprint Capability Score"
  ) +
  theme_minimal()

print(sprint_plot)












# Create Composite Player Readiness Score
cat("Creating composite player readiness score from multiple capabilities\n")

# Select key capabilities representing different movement qualities
key_capabilities <- c("jump_take off_dynamic", "sprint_max velocity_dynamic", 
                      "agility_deceleration_dynamic", "upper body_pull_dynamic")

cat("Selected capabilities for composite score:\n")
for(cap in key_capabilities) {
  coverage <- recovery_3day_summary %>% filter(capability_id == cap)
  cat("-", cap, ":", coverage$n_observations, "observations\n")
}

# Create composite dataset
composite_data <- recovery_data_3day %>%
  filter(capability_id %in% key_capabilities) %>%
  select(date, capability_id, daily_score, match_date, days_post_match) %>%
  pivot_wider(names_from = capability_id, values_from = daily_score, 
              names_prefix = "cap_") %>%
  # Clean column names
  rename_with(~str_replace_all(.x, c(" " = "_", "-" = "_")), starts_with("cap_")) %>%
  # Create composite readiness score (average of all capabilities)
  rowwise() %>%
  mutate(
    composite_score = mean(c_across(starts_with("cap_")), na.rm = TRUE),
    capabilities_available = sum(!is.na(c_across(starts_with("cap_"))))
  ) %>%
  # Only keep days with at least 3 capabilities tested
  filter(capabilities_available >= 3) %>%
  arrange(date)

cat("Composite readiness dataset created:\n")
cat("Observations:", nrow(composite_data), "\n")
cat("Date range:", as.character(min(composite_data$date)), "to", 
    as.character(max(composite_data$date)), "\n")

# Add GPS predictors to composite data
composite_with_gps <- composite_data %>%
  left_join(gps_match_data, by = "match_date") %>%
  select(date, composite_score, match_date, days_post_match,
         distance, distance_over_27, accel_decel_over_3_5, peak_speed)

# Create time series for composite score
composite_ts <- composite_with_gps %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(everything(), .direction = "down") %>%
  filter(!is.na(composite_score)) %>%
  as_tsibble(index = date)

cat("Composite time series created:", nrow(composite_ts), "observations\n")

# Train/test split
comp_train <- composite_ts %>% filter(date <= split_date)
comp_test <- composite_ts %>% filter(date > split_date)

# Build models for composite readiness
composite_models <- comp_train %>%
  model(
    drift_only = RW(composite_score ~ drift()),
    arima = ARIMA(composite_score),
    ets = ETS(composite_score),
    gps_combined = TSLM(composite_score ~ distance_over_27 + accel_decel_over_3_5),
    arima_gps = ARIMA(composite_score ~ distance_over_27 + accel_decel_over_3_5)
  )

# Generate forecasts
composite_forecasts <- composite_models %>%
  forecast(new_data = comp_test)

# Check accuracy
composite_accuracy <- composite_forecasts %>%
  accuracy(comp_test) %>%
  arrange(RMSE)

cat("Composite Player Readiness - Model accuracy:\n")
print(composite_accuracy)

# Visualize composite readiness forecasting
composite_plot <- composite_forecasts %>%
  autoplot(composite_ts, level = 95) +
  labs(
    title = "Composite Player Readiness Forecasting",
    subtitle = "Overall physical capability across multiple movement patterns",
    x = "Date",
    y = "Composite Readiness Score"
  ) +
  theme_minimal()

print(composite_plot)

# Summary comparison across all approaches
cat("\n========== METHODOLOGY SUMMARY ==========\n")
cat("Jump capability - Best RMSE:", round(min(accuracy_gps$RMSE), 5), "\n")
cat("Sprint capability - Best RMSE:", round(min(sprint_accuracy$RMSE), 5), "\n") 
cat("Composite readiness - Best RMSE:", round(min(composite_accuracy$RMSE), 5), "\n")
cat("GPS predictors: Consistently perform worse than simple drift models\n")









# Test Alternative GPS Effect Approaches
cat("Testing alternative approaches to detect GPS effects\n")

# Test 1: Extreme GPS days only (top 25% intensity matches)
gps_extremes <- gps_match_data %>%
  mutate(
    high_intensity = distance_over_27 > quantile(distance_over_27, 0.75, na.rm = TRUE),
    extreme_gps = ifelse(high_intensity, distance_over_27, NA)
  )

# Test 2: Lagged effects (GPS predicting Day 2 recovery specifically)
recovery_day2_only <- recovery_data_3day %>%
  filter(capability_id == "jump_take off_dynamic", days_post_match == 2) %>%
  left_join(gps_match_data, by = "match_date") %>%
  select(date, daily_score, distance_over_27, accel_decel_over_3_5)

cat("Day 2 recovery data:", nrow(recovery_day2_only), "observations\n")

# Simple correlation test
if(nrow(recovery_day2_only) > 10) {
  cor_distance <- cor(recovery_day2_only$daily_score, 
                      recovery_day2_only$distance_over_27, 
                      use = "complete.obs")
  cor_accel <- cor(recovery_day2_only$daily_score, 
                   recovery_day2_only$accel_decel_over_3_5, 
                   use = "complete.obs")
  
  cat("Day 2 correlations:\n")
  cat("GPS distance_over_27 vs recovery:", round(cor_distance, 3), "\n")
  cat("GPS accelerations vs recovery:", round(cor_accel, 3), "\n")
}

# Test 3: Threshold model (only high intensity matches)
threshold_data <- recovery_data_3day %>%
  filter(capability_id == "jump_take off_dynamic") %>%
  left_join(gps_extremes, by = "match_date") %>%
  filter(!is.na(extreme_gps)) %>%  # Only high intensity matches
  arrange(date)

cat("High intensity matches only:", nrow(threshold_data), "observations\n")

if(nrow(threshold_data) > 50) {
  # Quick linear model test
  threshold_model <- lm(daily_score ~ extreme_gps + days_post_match, 
                        data = threshold_data)
  cat("High intensity model p-value:", 
      round(summary(threshold_model)$coefficients[2,4], 4), "\n")
}






# Test GPS × Time Interaction Effects
cat("Testing GPS × days_post_match interaction effects\n")

# Create interaction dataset
interaction_data <- recovery_data_3day %>%
  filter(capability_id == "jump_take off_dynamic") %>%
  left_join(gps_match_data, by = "match_date") %>%
  filter(!is.na(distance_over_27)) %>%
  mutate(
    # Standardize GPS variables for easier interpretation
    gps_high_speed_z = scale(distance_over_27)[,1],
    gps_accel_z = scale(accel_decel_over_3_5)[,1]
  )

cat("Interaction dataset:", nrow(interaction_data), "observations\n")

# Test 1: GPS × days_post_match interaction
interaction_model1 <- lm(daily_score ~ gps_high_speed_z * days_post_match, 
                         data = interaction_data)

cat("\nInteraction Model 1: GPS High Speed × Days Post-Match\n")
summary_model1 <- summary(interaction_model1)
cat("Interaction p-value:", round(summary_model1$coefficients[4,4], 4), "\n")
cat("R-squared:", round(summary_model1$r.squared, 4), "\n")

# Test 2: GPS accelerations × days_post_match interaction  
interaction_model2 <- lm(daily_score ~ gps_accel_z * days_post_match,
                         data = interaction_data)

cat("\nInteraction Model 2: GPS Accelerations × Days Post-Match\n")
summary_model2 <- summary(interaction_model2)
cat("Interaction p-value:", round(summary_model2$coefficients[4,4], 4), "\n")
cat("R-squared:", round(summary_model2$r.squared, 4), "\n")

# Test 3: Combined interaction model
interaction_model3 <- lm(daily_score ~ (gps_high_speed_z + gps_accel_z) * days_post_match,
                         data = interaction_data)

cat("\nInteraction Model 3: Combined GPS × Days Post-Match\n")
summary_model3 <- summary(interaction_model3)
print(round(summary_model3$coefficients[,4], 4))  # Show all p-values

# Visualize interaction effect (if significant)
if(summary_model1$coefficients[4,4] < 0.1) {
  cat("\nCreating interaction visualization...\n")
  
  # Create prediction data for visualization
  pred_data <- expand.grid(
    gps_high_speed_z = c(-1, 0, 1),  # Low, Medium, High GPS
    days_post_match = 1:3
  )
  
  pred_data$predicted_recovery <- predict(interaction_model1, pred_data)
  pred_data$gps_level <- factor(pred_data$gps_high_speed_z, 
                                labels = c("Low GPS", "Medium GPS", "High GPS"))
  
  interaction_plot <- ggplot(pred_data, aes(x = days_post_match, y = predicted_recovery, 
                                            color = gps_level)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "GPS × Time Interaction Effect",
      subtitle = "How GPS intensity affects recovery trajectory",
      x = "Days Post-Match",
      y = "Predicted Jump Capability",
      color = "GPS Intensity"
    ) +
    theme_minimal()
  
  print(interaction_plot)
} else {
  cat("No significant interaction effects found\n")
}

# Summary of findings
cat("\n========== INTERACTION EFFECTS SUMMARY ==========\n")
cat("GPS High Speed × Time p-value:", round(summary_model1$coefficients[4,4], 4), "\n")
cat("GPS Accelerations × Time p-value:", round(summary_model2$coefficients[4,4], 4), "\n")

if(min(summary_model1$coefficients[4,4], summary_model2$coefficients[4,4]) < 0.05) {
  cat("SIGNIFICANT INTERACTION FOUND: GPS intensity affects recovery trajectory\n")
} else {
  cat("No significant interactions: GPS doesn't change recovery slope\n")
}




# Test Heart Rate Zone Predictors
cat("Testing heart rate zones as recovery predictors\n")

# Check available HR data in overlap matches
hr_data <- overlap_matches %>%
  select(date, hr_zone_1_hms, hr_zone_2_hms, hr_zone_3_hms, 
         hr_zone_4_hms, hr_zone_5_hms) %>%
  rename(match_date = date) %>%
  # Convert HMS to minutes
  mutate(
    hr_zone_1_min = as.numeric(hr_zone_1_hms) / 60,
    hr_zone_2_min = as.numeric(hr_zone_2_hms) / 60,
    hr_zone_3_min = as.numeric(hr_zone_3_hms) / 60,
    hr_zone_4_min = as.numeric(hr_zone_4_hms) / 60,
    hr_zone_5_min = as.numeric(hr_zone_5_hms) / 60,
    # Calculate high intensity HR (zones 4+5)
    hr_high_intensity = hr_zone_4_min + hr_zone_5_min,
    # Calculate total HR time
    hr_total_time = hr_zone_1_min + hr_zone_2_min + hr_zone_3_min + hr_zone_4_min + hr_zone_5_min
  ) %>%
  select(match_date, hr_zone_4_min, hr_zone_5_min, hr_high_intensity, hr_total_time)

cat("HR data summary:\n")
summary(hr_data %>% select(-match_date))

# Create recovery data with HR predictors
recovery_hr <- recovery_data_3day %>%
  filter(capability_id == "jump_take off_dynamic") %>%
  left_join(hr_data, by = "match_date") %>%
  left_join(gps_match_data, by = "match_date") %>%
  filter(!is.na(hr_high_intensity))

cat("Recovery observations with HR data:", nrow(recovery_hr), "\n")

# Test 1: HR zones vs GPS distance correlation
if("distance_over_27" %in% names(recovery_hr)) {
  cor_hr_gps <- cor(recovery_hr$hr_high_intensity, recovery_hr$distance_over_27, use = "complete.obs")
  cat("Correlation HR high intensity vs GPS distance >27:", round(cor_hr_gps, 3), "\n")
} else {
  cat("GPS data not available for correlation\n")
}

# Test 2: HR as predictor for Day 2 recovery
hr_day2 <- recovery_hr %>%
  filter(days_post_match == 2) %>%
  select(daily_score, hr_high_intensity, hr_zone_4_min, hr_zone_5_min)

if(nrow(hr_day2) > 10) {
  cor_hr_recovery <- cor(hr_day2$daily_score, hr_day2$hr_high_intensity, use = "complete.obs")
  cat("HR high intensity vs Day 2 recovery correlation:", round(cor_hr_recovery, 3), "\n")
  
  cor_zone4 <- cor(hr_day2$daily_score, hr_day2$hr_zone_4_min, use = "complete.obs")
  cor_zone5 <- cor(hr_day2$daily_score, hr_day2$hr_zone_5_min, use = "complete.obs")
  
  cat("HR Zone 4 vs Day 2 recovery correlation:", round(cor_zone4, 3), "\n")
  cat("HR Zone 5 vs Day 2 recovery correlation:", round(cor_zone5, 3), "\n")
}

# Test 3: HR linear model
hr_model <- lm(daily_score ~ hr_high_intensity + days_post_match, data = recovery_hr)
hr_summary <- summary(hr_model)

cat("\nHR Linear Model Results:\n")
cat("HR coefficient p-value:", round(hr_summary$coefficients[2,4], 4), "\n")
cat("R-squared:", round(hr_summary$r.squared, 4), "\n")

# Test 4: HR extreme threshold (like GPS test)
hr_extreme_threshold <- quantile(recovery_hr$hr_high_intensity, 0.75, na.rm = TRUE)
hr_extreme_data <- recovery_hr %>%
  filter(hr_high_intensity > hr_extreme_threshold)

cat("High HR matches (top 25%):", nrow(hr_extreme_data), "observations\n")

if(nrow(hr_extreme_data) > 20) {
  hr_extreme_model <- lm(daily_score ~ hr_high_intensity + days_post_match, 
                         data = hr_extreme_data)
  hr_extreme_summary <- summary(hr_extreme_model)
  cat("High HR model p-value:", round(hr_extreme_summary$coefficients[2,4], 4), "\n")
}

# Test 5: Compare HR vs GPS in same model
combined_hr_gps <- recovery_hr %>%
  left_join(gps_match_data, by = "match_date") %>%
  filter(!is.na(distance_over_27))

if(nrow(combined_hr_gps) > 50) {
  combined_model <- lm(daily_score ~ hr_high_intensity + distance_over_27 + days_post_match, 
                       data = combined_hr_gps)
  combined_summary <- summary(combined_model)
  
  cat("\nCombined HR + GPS Model:\n")
  cat("HR p-value:", round(combined_summary$coefficients[2,4], 4), "\n")
  cat("GPS p-value:", round(combined_summary$coefficients[3,4], 4), "\n")
  cat("R-squared:", round(combined_summary$r.squared, 4), "\n")
}

cat("\n========== HR TESTING SUMMARY ==========\n")
cat("HR provides different physiological stress measure than GPS distance\n")
cat("Testing complete - compare results with GPS findings\n")

