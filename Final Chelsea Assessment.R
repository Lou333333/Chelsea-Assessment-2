#part 1 ====
# Load required libraries
library(tidyverse)
library(lubridate)
library(fpp3)
library(dplyr)

# Load d# Load d# Load datasets
gps <- read_csv("Data/CFC GPS Data.csv")
phys_cap <- read_csv("Data/CFC Physical Capability Data_.csv")
recovery <- read_csv("Data/CFC Recovery status Data.csv")
priority <- read_csv("Data/CFC Individual Priority Areas.csv")

# structure check
glimpse(gps)
glimpse(phys_cap)


# Clean capability data
phys_cap <- phys_cap %>%
  mutate(
    date = dmy(testDate),
    capability_id = paste(movement, quality, expression, sep = "_")
  ) %>%
  filter(!is.na(date), !is.na(benchmarkPct))

# Basic summary
summary(phys_cap$benchmarkPct)


# Create summary of all 20 capability combinations
capability_overview <- phys_cap %>%
  group_by(capability_id, movement, quality, expression) %>%
  summarise(
    n_tests = n(),
    mean_score = round(mean(benchmarkPct), 3),
    date_range_days = as.numeric(max(date) - min(date)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_tests))

print(capability_overview)


# Clean GPS data and identify matches
gps <- gps %>% 
  mutate(
    date = dmy(date),
    is_match = (md_minus_code == 0)
  )

match_days <- gps %>% filter(is_match)

# Find overlap period (matches with capability data)
overlap_matches <- match_days %>%
  filter(date >= min(phys_cap$date), date <= max(phys_cap$date))

# Summary
nrow(match_days)  # Total matches
nrow(overlap_matches)  # Matches with capability data available


# Capability score distributions by movement type
p1 <- phys_cap %>%
  ggplot(aes(x = benchmarkPct, fill = movement)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~movement, scales = "free_y") +
  labs(title = "Capability Score Distributions by Movement Type",
       x = "Capability Score (% of baseline)", y = "Count") +
  theme_minimal()

print(p1)


# Plot 2: Match day GPS intensity distributions
p2 <- overlap_matches %>%
  select(distance, distance_over_27, accel_decel_over_3_5, peak_speed) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Match Day GPS Intensity Distributions",
       x = "Value", y = "Count") +
  theme_minimal()

print(p2)


# Document key numbers for Phase 1
cat("Phase 1 Summary:\n")
cat("- Total capability observations:", nrow(phys_cap), "\n")
cat("- Unique capabilities:", n_distinct(phys_cap$capability_id), "\n")
cat("- Total matches:", nrow(match_days), "\n")
cat("- Overlap matches:", nrow(overlap_matches), "\n")
cat("- Capability score range:", round(min(phys_cap$benchmarkPct), 3), "to", round(max(phys_cap$benchmarkPct), 3), "\n")
cat("- Date range:", as.character(min(phys_cap$date)), "to", as.character(max(phys_cap$date)), "\n")



#Part 2 ====

# Calculate days between consecutive matches
match_intervals <- overlap_matches %>%
  arrange(date) %>%
  mutate(
    next_match_date = lead(date),
    days_to_next_match = as.numeric(next_match_date - date)
  ) %>%
  filter(!is.na(days_to_next_match))

# Key statistics
median(match_intervals$days_to_next_match)
sum(match_intervals$days_to_next_match <= 4)  # Matches with ≤4 days to next


# Function to create post-match recovery windows
create_recovery_windows <- function(match_date, capability_data, window_days = 3) {
  capability_data %>%
    filter(date > match_date, date <= (match_date + window_days)) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
}


# Create daily capability scores
daily_capability <- phys_cap %>%
  group_by(date, capability_id, movement, quality, expression) %>%
  summarise(daily_score = mean(benchmarkPct), .groups = "drop")

# Process all matches to create recovery dataset
recovery_data <- map_dfr(1:nrow(overlap_matches), function(i) {
  match_date <- overlap_matches$date[i]
  create_recovery_windows(match_date, daily_capability, window_days = 3)
})


# Recovery dataset summary
nrow(recovery_data)
n_distinct(recovery_data$capability_id)
n_distinct(recovery_data$match_date)

# Show recovery data structure
head(recovery_data)

# Summary by capability
recovery_summary <- recovery_data %>%
  group_by(capability_id) %>%
  summarise(
    n_observations = n(),
    matches_tracked = n_distinct(match_date),
    .groups = "drop"
  ) %>%
  arrange(desc(n_observations))

print(recovery_summary)

# Example recovery trajectory for one match
first_match <- min(recovery_data$match_date)

recovery_example <- recovery_data %>%
  filter(match_date == first_match) %>%
  select(capability_id, days_post_match, daily_score) %>%
  arrange(capability_id, days_post_match)

head(recovery_example, 12)  # Show first 12 rows


#Part 3 ====
# Create GPS and HR predictors from overlap matches
gps_hr_predictors <- overlap_matches %>%
  select(date, distance, distance_over_21, distance_over_24, distance_over_27,
         accel_decel_over_2_5, accel_decel_over_3_5, accel_decel_over_4_5,
         peak_speed, day_duration,
         hr_zone_1_hms, hr_zone_2_hms, hr_zone_3_hms, hr_zone_4_hms, hr_zone_5_hms) %>%
  mutate(
    # Convert ALL HR zones to minutes
    hr_zone_1_min = as.numeric(hr_zone_1_hms) / 60,
    hr_zone_2_min = as.numeric(hr_zone_2_hms) / 60,
    hr_zone_3_min = as.numeric(hr_zone_3_hms) / 60,
    hr_zone_4_min = as.numeric(hr_zone_4_hms) / 60,
    hr_zone_5_min = as.numeric(hr_zone_5_hms) / 60,
    hr_high_intensity = hr_zone_4_min + hr_zone_5_min
  ) %>%
  select(-ends_with("_hms")) %>%
  rename(match_date = date)
# Check predictor summary
summary(gps_hr_predictors %>% select(distance_over_27, accel_decel_over_3_5, hr_high_intensity))

# Test with jump_take off_dynamic (good data coverage)
test_capability <- "jump_take off_dynamic"

test_ts_data <- recovery_data %>%
  filter(capability_id == test_capability) %>%
  left_join(gps_hr_predictors, by = "match_date") %>%
  arrange(date) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(everything(), .direction = "down") %>%
  filter(!is.na(daily_score)) %>%
  as_tsibble(index = date)

nrow(test_ts_data)  # Should be around 577 observations


# Function to forecast any capability
forecast_capability <- function(capability_name) {
  
  # Create time series data
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_predictors, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  # Skip if insufficient data
  if(nrow(ts_data) < 100) {
    return(list(capability = capability_name, status = "insufficient_data"))
  }
  
  # Train/test split
  split_point <- floor(nrow(ts_data) * 0.8)
  split_date <- ts_data$date[split_point]
  
  train <- ts_data %>% filter(date <= split_date)
  test <- ts_data %>% filter(date > split_date)
  
  # Time series models
  models <- train %>%
    model(
      naive = NAIVE(daily_score),
      drift = RW(daily_score ~ drift()),
      arima = ARIMA(daily_score),
      ets = ETS(daily_score)
    )
  
  # Generate forecasts and calculate accuracy
  forecasts <- models %>% forecast(h = nrow(test))
  accuracy_results <- forecasts %>% accuracy(test) %>% arrange(RMSE)
  
  return(list(
    capability = capability_name,
    status = "success",
    best_model = accuracy_results$.model[1],
    best_rmse = accuracy_results$RMSE[1],
    accuracy = accuracy_results
  ))
}


# Test the function
test_result <- forecast_capability("jump_take off_dynamic")
test_result$best_model
test_result$best_rmse


#Part 4 ====
# Get all capabilities with sufficient data (from recovery_summary)
capabilities_to_analyze <- recovery_summary %>%
  filter(n_observations >= 100) %>%
  pull(capability_id)

length(capabilities_to_analyze)  # Should be around 14-15 capabilities

# Run systematic analysis
all_results <- map(capabilities_to_analyze, forecast_capability)
names(all_results) <- capabilities_to_analyze


# Compile results
results_summary <- map_dfr(all_results, function(result) {
  if(result$status != "success") {
    return(tibble(capability = result$capability, status = result$status))
  }
  
  tibble(
    capability = result$capability,
    best_model = result$best_model,
    best_rmse = round(result$best_rmse, 5),
    status = result$status
  )
})

print(results_summary)


# Add movement type to results
results_with_movement <- results_summary %>%
  separate(capability, into = c("movement", "quality", "expression"), sep = "_", remove = FALSE) %>%
  arrange(best_rmse)

# Best performing capabilities (lowest RMSE)
head(results_with_movement, 5)

# Model preferences by movement type
results_with_movement %>%
  count(movement, best_model) %>%
  pivot_wider(names_from = best_model, values_from = n, values_fill = 0)


# Function to test GPS/HR predictors against time series
test_external_predictors <- function(capability_name) {
  
  # Get the time series data
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_predictors, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  # Train/test split
  split_point <- floor(nrow(ts_data) * 0.8)
  split_date <- ts_data$date[split_point]
  
  train <- ts_data %>% filter(date <= split_date)
  test <- ts_data %>% filter(date > split_date)
  
  # Get best time series RMSE
  ts_result <- all_results[[capability_name]]
  best_ts_rmse <- ts_result$best_rmse
  
  # Test GPS predictor
  gps_model <- lm(daily_score ~ distance_over_27 + days_post_match, data = train)
  gps_pred <- predict(gps_model, test)
  gps_rmse <- sqrt(mean((test$daily_score - gps_pred)^2, na.rm = TRUE))
  
  # Test HR predictor
  hr_model <- lm(daily_score ~ hr_high_intensity + days_post_match, data = train)
  hr_pred <- predict(hr_model, test)
  hr_rmse <- sqrt(mean((test$daily_score - hr_pred)^2, na.rm = TRUE))
  
  return(tibble(
    capability = capability_name,
    best_ts_rmse = best_ts_rmse,
    gps_rmse = round(gps_rmse, 5),
    hr_rmse = round(hr_rmse, 5)
  ))
}

# Test on top 5 best-performing capabilities
top_5_caps <- head(results_with_movement$capability, 5)
external_results <- map_dfr(top_5_caps, test_external_predictors)
print(external_results)


# Calculate summary statistics
cat("Phase 4 Results Summary:\n")
cat("- Capabilities analyzed:", nrow(results_summary), "\n")
cat("- Average RMSE:", round(mean(results_summary$best_rmse), 4), "\n")
cat("- Best capability:", results_with_movement$capability[1], "RMSE:", results_with_movement$best_rmse[1], "\n")

# Model distribution
model_counts <- table(results_summary$best_model)
cat("- Model preferences: Drift =", model_counts["drift"], ", ARIMA =", model_counts["arima"], ", ETS =", model_counts["ets"], "\n")

# External predictor effectiveness
external_better <- sum(external_results$gps_rmse < external_results$best_ts_rmse)
cat("- GPS improved forecasting:", external_better, "out of", nrow(external_results), "top capabilities\n")



#Part 4.5: ARIMA with External Regressors (GPS/HR Comparison) ====

#Part 4.5: ARIMA with External Regressors (ALL CAPABILITIES) ====

# Function to test ARIMA with GPS/HR external regressors (same as before)
test_arima_xreg <- function(capability_name) {
  
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_predictors, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  if(nrow(ts_data) < 100) return(NULL)
  
  split_point <- floor(nrow(ts_data) * 0.8)
  split_date <- ts_data$date[split_point]
  
  train <- ts_data %>% filter(date <= split_date)
  test <- ts_data %>% filter(date > split_date)
  
  ts_result <- all_results[[capability_name]]
  best_ts_rmse <- ts_result$best_rmse
  best_ts_model <- ts_result$best_model
  
  arima_gps_rmse <- tryCatch({
    model_gps <- train %>% model(ARIMA(daily_score ~ distance_over_27))
    forecast_gps <- model_gps %>% forecast(new_data = test)
    accuracy(forecast_gps, test)$RMSE
  }, error = function(e) {
    cat("GPS model failed for", capability_name, "\n")
    NA
  })
  
  arima_hr_rmse <- tryCatch({
    model_hr <- train %>% model(ARIMA(daily_score ~ hr_high_intensity))
    forecast_hr <- model_hr %>% forecast(new_data = test)
    accuracy(forecast_hr, test)$RMSE
  }, error = function(e) {
    cat("HR model failed for", capability_name, "\n")
    NA
  })
  
  return(tibble(
    capability = capability_name,
    best_ts_model = best_ts_model,
    time_series_rmse = best_ts_rmse,
    arima_gps_rmse = round(arima_gps_rmse, 5),
    arima_hr_rmse = round(arima_hr_rmse, 5)
  ))
}

# Test ALL 19 capabilities
cat("\nTesting ARIMA with external regressors on ALL capabilities...\n")
cat("This may take 2-3 minutes...\n\n")

xreg_results_all <- map_dfr(capabilities_to_analyze, test_arima_xreg)
xreg_results_all <- xreg_results_all %>% filter(!is.na(time_series_rmse))

print(xreg_results_all)

# Summary statistics
cat("\n=== OVERALL RESULTS ===\n")
cat("Capabilities tested:", nrow(xreg_results_all), "\n")
cat("Average Time Series RMSE:", round(mean(xreg_results_all$time_series_rmse, na.rm = TRUE), 5), "\n")
cat("Average ARIMA+GPS RMSE:", round(mean(xreg_results_all$arima_gps_rmse, na.rm = TRUE), 5), "\n")
cat("Average ARIMA+HR RMSE:", round(mean(xreg_results_all$arima_hr_rmse, na.rm = TRUE), 5), "\n\n")

gps_improvements_all <- sum(xreg_results_all$arima_gps_rmse < xreg_results_all$time_series_rmse, na.rm = TRUE)
hr_improvements_all <- sum(xreg_results_all$arima_hr_rmse < xreg_results_all$time_series_rmse, na.rm = TRUE)

cat("GPS improved forecasting:", gps_improvements_all, "out of", nrow(xreg_results_all), 
    "(", round(100 * gps_improvements_all / nrow(xreg_results_all), 1), "%)\n")
cat("HR improved forecasting:", hr_improvements_all, "out of", nrow(xreg_results_all), 
    "(", round(100 * hr_improvements_all / nrow(xreg_results_all), 1), "%)\n\n")

# Movement type breakdown
movement_breakdown <- xreg_results_all %>%
  separate(capability, into = c("movement", "quality", "expression"), sep = "_", remove = FALSE) %>%
  group_by(movement) %>%
  summarise(
    n = n(),
    ts_rmse = round(mean(time_series_rmse), 5),
    gps_rmse = round(mean(arima_gps_rmse, na.rm = TRUE), 5),
    hr_rmse = round(mean(arima_hr_rmse, na.rm = TRUE), 5),
    gps_wins = sum(arima_gps_rmse < time_series_rmse, na.rm = TRUE),
    .groups = "drop"
  )

cat("=== RESULTS BY MOVEMENT TYPE ===\n")
print(movement_breakdown)

##############4.5 again


#Part 4.5: ARIMA with ALL GPS/HR External Regressors ====

test_arima_all_predictors <- function(capability_name) {
  
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_predictors, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  if(nrow(ts_data) < 100) return(NULL)
  
  split_point <- floor(nrow(ts_data) * 0.8)
  train <- ts_data %>% slice(1:split_point)
  test <- ts_data %>% slice((split_point+1):n())
  
  # Get baseline time series RMSE
  ts_result <- all_results[[capability_name]]
  best_ts_rmse <- ts_result$best_rmse
  
  # Helper function to test a predictor
  test_predictor <- function(formula_str) {
    tryCatch({
      model <- train %>% model(ARIMA(as.formula(formula_str)))
      forecast_result <- model %>% forecast(new_data = test)
      accuracy(forecast_result, test)$RMSE
    }, error = function(e) NA)
  }
  
  # Test ALL GPS distance metrics
  dist_total <- test_predictor("daily_score ~ distance")
  dist_21 <- test_predictor("daily_score ~ distance_over_21")
  dist_24 <- test_predictor("daily_score ~ distance_over_24")
  dist_27 <- test_predictor("daily_score ~ distance_over_27")
  
  # Test ALL acceleration/deceleration metrics
  accel_25 <- test_predictor("daily_score ~ accel_decel_over_2_5")
  accel_35 <- test_predictor("daily_score ~ accel_decel_over_3_5")
  accel_45 <- test_predictor("daily_score ~ accel_decel_over_4_5")
  
  # Test other GPS metrics
  peak_spd <- test_predictor("daily_score ~ peak_speed")
  duration <- test_predictor("daily_score ~ day_duration")
  
  # Test ALL HR zone metrics (need to convert from hms to minutes first if not already done)
  # Using the minutes versions you created
  hr_z1 <- test_predictor("daily_score ~ hr_zone_1_min")
  hr_z2 <- test_predictor("daily_score ~ hr_zone_2_min")
  hr_z3 <- test_predictor("daily_score ~ hr_zone_3_min")
  hr_z4 <- test_predictor("daily_score ~ hr_zone_4_min")
  hr_z5 <- test_predictor("daily_score ~ hr_zone_5_min")
  hr_high <- test_predictor("daily_score ~ hr_high_intensity")
  
  return(tibble(
    capability = capability_name,
    time_series = best_ts_rmse,
    # Distance metrics (4)
    gps_total_dist = round(dist_total, 5),
    gps_dist_21 = round(dist_21, 5),
    gps_dist_24 = round(dist_24, 5),
    gps_dist_27 = round(dist_27, 5),
    # Accel/Decel metrics (3)
    gps_accel_25 = round(accel_25, 5),
    gps_accel_35 = round(accel_35, 5),
    gps_accel_45 = round(accel_45, 5),
    # Other GPS (2)
    gps_peak_speed = round(peak_spd, 5),
    gps_duration = round(duration, 5),
    # ALL HR zones (6)
    hr_zone_1 = round(hr_z1, 5),
    hr_zone_2 = round(hr_z2, 5),
    hr_zone_3 = round(hr_z3, 5),
    hr_zone_4 = round(hr_z4, 5),
    hr_zone_5 = round(hr_z5, 5),
    hr_zones_45 = round(hr_high, 5)
  ))
}

# Run on top 5 capabilities
cat("\n========================================\n")
cat("Testing ALL 17 GPS/HR metrics individually\n")
cat("========================================\n\n")

all_predictors_results <- map_dfr(capabilities_to_analyze, test_arima_all_predictors)
print(all_predictors_results)

# Calculate average RMSE for each predictor
cat("\n=== AVERAGE RMSE BY PREDICTOR (RANKED) ===\n")
avg_rmse <- all_predictors_results %>%
  summarise(across(time_series:hr_zones_45, ~mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "avg_rmse") %>%
  arrange(avg_rmse)
print(avg_rmse)

# Count wins for each predictor
cat("\n=== HOW MANY TIMES EACH PREDICTOR BEAT TIME SERIES ===\n")
wins_count <- all_predictors_results %>%
  summarise(
    gps_total_dist = sum(gps_total_dist < time_series, na.rm = TRUE),
    gps_dist_21 = sum(gps_dist_21 < time_series, na.rm = TRUE),
    gps_dist_24 = sum(gps_dist_24 < time_series, na.rm = TRUE),
    gps_dist_27 = sum(gps_dist_27 < time_series, na.rm = TRUE),
    gps_accel_25 = sum(gps_accel_25 < time_series, na.rm = TRUE),
    gps_accel_35 = sum(gps_accel_35 < time_series, na.rm = TRUE),
    gps_accel_45 = sum(gps_accel_45 < time_series, na.rm = TRUE),
    gps_peak_speed = sum(gps_peak_speed < time_series, na.rm = TRUE),
    gps_duration = sum(gps_duration < time_series, na.rm = TRUE),
    hr_zone_1 = sum(hr_zone_1 < time_series, na.rm = TRUE),
    hr_zone_2 = sum(hr_zone_2 < time_series, na.rm = TRUE),
    hr_zone_3 = sum(hr_zone_3 < time_series, na.rm = TRUE),
    hr_zone_4 = sum(hr_zone_4 < time_series, na.rm = TRUE),
    hr_zone_5 = sum(hr_zone_5 < time_series, na.rm = TRUE),
    hr_zones_45 = sum(hr_zones_45 < time_series, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "wins") %>%
  arrange(desc(wins))

print(wins_count)

# Identify the "winners" (predictors that won at least once)
cat("\n=== PREDICTORS THAT WON AT LEAST ONCE ===\n")
winners <- wins_count %>% filter(wins > 0)
print(winners)

cat("\n=== SUMMARY ===\n")
cat("Total capabilities tested:", nrow(all_predictors_results), "\n")
cat("Total predictors tested: 17\n")
cat("Time Series baseline RMSE:", round(mean(all_predictors_results$time_series), 5), "\n")
cat("\nBest performing external predictor (lowest avg RMSE):\n")
print(head(avg_rmse, 1))
cat("\nPredictor that won most often:\n")
print(head(wins_count, 1))
cat("\nNumber of predictors that beat time series at least once:", nrow(winners), "\n")

#Part 4.6: Combined GPS/HR Model ====

test_combined_predictors <- function(capability_name) {
  
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_predictors, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  if(nrow(ts_data) < 100) return(NULL)
  
  split_point <- floor(nrow(ts_data) * 0.8)
  train <- ts_data %>% slice(1:split_point)
  test <- ts_data %>% slice((split_point+1):n())
  
  ts_result <- all_results[[capability_name]]
  best_ts_rmse <- ts_result$best_rmse
  
  # Best single predictor
  best_single <- all_predictors_results %>%
    filter(capability == capability_name) %>%
    select(gps_dist_27) %>%
    pull()
  
  # Test combined model: Top 3 predictors
  combined_rmse <- tryCatch({
    model <- train %>% model(
      ARIMA(daily_score ~ distance_over_27 + distance_over_24 + accel_decel_over_4_5)
    )
    forecast_result <- model %>% forecast(new_data = test)
    accuracy(forecast_result, test)$RMSE
  }, error = function(e) NA)
  
  # Test combined with HR too
  combined_hr_rmse <- tryCatch({
    model <- train %>% model(
      ARIMA(daily_score ~ distance_over_27 + hr_high_intensity)
    )
    forecast_result <- model %>% forecast(new_data = test)
    accuracy(forecast_result, test)$RMSE
  }, error = function(e) NA)
  
  return(tibble(
    capability = capability_name,
    time_series = best_ts_rmse,
    best_single = round(best_single, 5),
    combined_gps = round(combined_rmse, 5),
    combined_gps_hr = round(combined_hr_rmse, 5)
  ))
}

# Test combined models
cat("\n=== TESTING COMBINED PREDICTORS ===\n")
combined_results <- map_dfr(capabilities_to_analyze, test_combined_predictors)
print(combined_results)

# Summary
cat("\n=== COMBINED MODEL RESULTS ===\n")
cat("Time Series avg:", round(mean(combined_results$time_series), 5), "\n")
cat("Best Single avg:", round(mean(combined_results$best_single, na.rm = TRUE), 5), "\n")
cat("Combined GPS avg:", round(mean(combined_results$combined_gps, na.rm = TRUE), 5), "\n")
cat("Combined GPS+HR avg:", round(mean(combined_results$combined_gps_hr, na.rm = TRUE), 5), "\n\n")

cat("Combined GPS better than single:", 
    sum(combined_results$combined_gps < combined_results$best_single, na.rm = TRUE), 
    "out of", nrow(combined_results), "\n")
cat("Combined GPS better than time series:", 
    sum(combined_results$combined_gps < combined_results$time_series, na.rm = TRUE), 
    "out of", nrow(combined_results), "\n")

####################

#Part 5 ====

#Part 5 ====
# Create composite time series directly (not using the function)
composite_ts_data <- composite_data %>%
  left_join(gps_hr_predictors, by = "match_date") %>%
  arrange(date) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(everything(), .direction = "down") %>%
  filter(!is.na(composite_score)) %>%
  as_tsibble(index = date)

# Train/test split for composite
split_point <- floor(nrow(composite_ts_data) * 0.8)
split_date <- composite_ts_data$date[split_point]

train_comp <- composite_ts_data %>% filter(date <= split_date)
test_comp <- composite_ts_data %>% filter(date > split_date)

# Build models for composite
comp_models <- train_comp %>%
  model(
    naive = NAIVE(composite_score),
    drift = RW(composite_score ~ drift()),
    arima = ARIMA(composite_score),
    ets = ETS(composite_score)
  )

# Generate forecasts and check accuracy
comp_forecasts <- comp_models %>% forecast(h = nrow(test_comp))
comp_accuracy <- comp_forecasts %>% accuracy(test_comp) %>% arrange(RMSE)

print(comp_accuracy)

# Test GPS/HR predictors on composite score using ARIMA
gps_comp_model <- train_comp %>% model(ARIMA(composite_score ~ distance_over_27))
gps_comp_forecast <- gps_comp_model %>% forecast(new_data = test_comp)
gps_comp_rmse <- accuracy(gps_comp_forecast, test_comp)$RMSE

hr_comp_model <- train_comp %>% model(ARIMA(composite_score ~ hr_high_intensity))
hr_comp_forecast <- hr_comp_model %>% forecast(new_data = test_comp)
hr_comp_rmse <- accuracy(hr_comp_forecast, test_comp)$RMSE

cat("Composite Results:\n")
cat("- Best time series RMSE:", round(comp_accuracy$RMSE[1], 5), "\n")
cat("- GPS predictor RMSE:", round(gps_comp_rmse, 5), "\n") 
cat("- HR predictor RMSE:", round(hr_comp_rmse, 5), "\n")




#Part 6: PLots

# Capability distributions by movement type
capability_dist_plot <- phys_cap %>%
  separate(capability_id, into = c("movement", "quality", "expression"), sep = "_", remove = FALSE) %>%
  ggplot(aes(x = benchmarkPct, fill = movement)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  facet_wrap(~movement, scales = "free_y") +
  labs(title = "Physical Capability Score Distributions by Movement Type",
       subtitle = "9,839 observations across 20 capability combinations",
       x = "Capability Score (% of baseline)", 
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 12))

print(capability_dist_plot)




# Create publication-quality results table
top_capabilities_plot <- results_with_movement %>%
  head(10) %>%
  mutate(capability_clean = str_replace_all(capability, "_", " "),
         capability_clean = str_to_title(capability_clean)) %>%
  ggplot(aes(x = reorder(capability_clean, -best_rmse), y = best_rmse, fill = movement)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 10 Most Predictable Physical Capabilities",
       subtitle = "Lower RMSE = Better forecasting accuracy",
       x = "Physical Capability", 
       y = "RMSE (Root Mean Square Error)",
       fill = "Movement Type") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text.y = element_text(size = 10))

print(top_capabilities_plot)


# Create the key comparison plot showing time series superiority
# Enhanced comparison plot with TOP 5 CAPABILITIES ONLY
comparison_plot_top5 <- xreg_results_all %>%
  # Filter to top 5 capabilities only
  filter(capability %in% top_5_caps) %>%
  pivot_longer(cols = c(time_series_rmse, arima_gps_rmse, arima_hr_rmse), 
               names_to = "method", values_to = "rmse") %>%
  mutate(method = case_when(
    method == "time_series_rmse" ~ "Time Series Only",
    method == "arima_gps_rmse" ~ "ARIMA + Best GPS", 
    method == "arima_hr_rmse" ~ "ARIMA + Best HR"
  )) %>%
  mutate(method = factor(method, levels = c("Time Series Only", "ARIMA + Best GPS", "ARIMA + Best HR"))) %>%
  filter(!is.na(rmse)) %>%
  ggplot(aes(x = method, y = rmse)) +
  geom_violin(aes(fill = method), alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  geom_jitter(size = 3, alpha = 0.6, width = 0.1) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "black", shape = 18) +
  scale_fill_manual(values = c("Time Series Only" = "#1f77b4", 
                               "ARIMA + Best GPS" = "#ff7f0e", 
                               "ARIMA + Best HR" = "#d62728")) +
  labs(title = "Time Series Methods Outperform External Predictors",
       subtitle = "Comprehensive test: 17 GPS/HR metrics across top 5 capabilities",
       x = "", 
       y = "RMSE",
       caption = "Lower = Better | Diamond = Mean | Best single predictor shown per capability") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.caption = element_text(size = 8.5, hjust = 0.5, color = "gray50", margin = margin(t = 10)),
    panel.grid.major.x = element_blank()
  )
print(comparison_plot_top5)





# Model effectiveness by movement type
model_movement_plot <- results_with_movement %>%
  count(movement, best_model) %>%
  ggplot(aes(x = movement, y = n, fill = best_model)) +
  geom_col(position = "fill") +
  labs(title = "Model Preferences by Movement Type",
       subtitle = "Different movement patterns require different forecasting approaches",
       x = "Movement Type", 
       y = "Proportion of Capabilities",
       fill = "Best Model") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(text = element_text(size = 12))

print(model_movement_plot)


# Create example forecast for jump takeoff capability
jump_example <- recovery_data %>%
  filter(capability_id == "jump_take off_dynamic") %>%
  left_join(gps_hr_predictors, by = "match_date") %>%
  arrange(date) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  fill(everything(), .direction = "down") %>%
  filter(!is.na(daily_score)) %>%
  as_tsibble(index = date) %>%
  slice_tail(n = 60)  # Last 60 days for clean visualization

# Fit model and forecast
jump_fit <- jump_example %>%
  model(drift = RW(daily_score ~ drift()))

jump_forecast <- jump_fit %>%
  forecast(h = 7)

# Plot
forecast_plot <- jump_forecast %>%
  autoplot(jump_example) +
  labs(title = "7-Day Capability Score Forecast Example",
       subtitle = "Jump Takeoff Dynamic - Drift Model (RMSE: 0.00604)",
       x = "Date", y = "Capability Score (% baseline)") +
  theme_minimal()

print(forecast_plot)


# Create summary statistics table for presentation
summary_stats <- tibble(
  Method = c("Time Series (Best)", "GPS Predictors", "HR Predictors"),
  `Average RMSE` = c(round(mean(external_results$best_ts_rmse), 4),
                     round(mean(external_results$gps_rmse), 4),
                     round(mean(external_results$hr_rmse), 4)),
  `Improvement Factor` = c("1.0x (Baseline)", 
                           paste0(round(mean(external_results$gps_rmse)/mean(external_results$best_ts_rmse), 1), "x worse"),
                           paste0(round(mean(external_results$hr_rmse)/mean(external_results$best_ts_rmse), 1), "x worse"))
)

print(summary_stats)






# Composite score time series with all three methods
composite_comparison <- composite_ts_data %>%
  slice_tail(n = 90) %>%  # Last 90 days
  ggplot(aes(x = date, y = composite_score)) +
  geom_line(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  labs(title = "Composite Capability Score Over Time",
       subtitle = "Combined top 4 capabilities show predictable patterns",
       x = "Date", y = "Composite Score") +
  theme_minimal()

print(composite_comparison)


# Create composite time series plot
composite_comparison <- composite_data %>%
  slice_tail(n = 90) %>%  # Last 90 days
  ggplot(aes(x = date, y = composite_score)) +
  geom_line(linewidth = 1, alpha = 0.7, color = "steelblue") +  # Changed size to linewidth
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Composite Capability Score Over Time",
       subtitle = "Combined top 4 capabilities show predictable temporal patterns",
       x = "Date", y = "Composite Score (% baseline)") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray50")
  )
print(composite_comparison)


# Seasonal patterns in capability scores
seasonal_plot <- recovery_data %>%
  mutate(month = month(date, label = TRUE)) %>%
  ggplot(aes(x = month, y = daily_score)) +
  geom_boxplot() +
  facet_wrap(~str_extract(capability_id, "^[^_]+")) +
  labs(title = "Seasonal Patterns in Capability Scores")
print(seasonal_plot)


# Individual capability recovery trajectories
individual_recovery <- recovery_data %>%
  group_by(capability_id, days_post_match) %>%
  summarise(
    mean_score = mean(daily_score, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  # Only include capabilities with data for all 3 days
  group_by(capability_id) %>%
  filter(n() == 3) %>%
  ungroup() %>%
  # Clean up capability names for display
  mutate(capability_clean = str_replace_all(capability_id, "_", " "),
         capability_clean = str_to_title(capability_clean)) %>%
  ggplot(aes(x = days_post_match, y = mean_score)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  facet_wrap(~capability_clean, scales = "free_y") +
  labs(title = "Post-Match Recovery by Individual Capability",
       subtitle = "Mean capability scores across Days 1-3 post-match",
       x = "Days Post-Match", y = "Capability Score (% baseline)") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

print(individual_recovery)


# Four movement types only, averaged across all capabilities
movement_recovery <- recovery_data %>%
  separate(capability_id, into = c("movement", "quality", "expression"), sep = "_") %>%
  group_by(movement, days_post_match) %>%
  summarise(
    mean_score = mean(daily_score, na.rm = TRUE),
    se_score = sd(daily_score, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = days_post_match, y = mean_score)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 1.5) +
  geom_ribbon(aes(ymin = mean_score - se_score, ymax = mean_score + se_score), 
              alpha = 0.2, fill = "steelblue") +
  facet_wrap(~str_to_title(movement), scales = "free_y") +
  labs(title = "Post-Match Recovery by Movement Type",
       subtitle = "Mean capability scores across Days 1-3 post-match",
       x = "Days Post-Match", y = "Capability Score (% baseline)") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(movement_recovery)













