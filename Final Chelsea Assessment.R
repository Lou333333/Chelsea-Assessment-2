#part 1 ====
# Load required libraries
library(tidyverse)
library(lubridate)
library(fpp3)

# Load datasets
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
    # Convert HR zones to minutes
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


# Test GPS/HR predictors on composite score
gps_comp_model <- lm(composite_score ~ distance_over_27 + days_post_match, data = train_comp)
gps_comp_pred <- predict(gps_comp_model, test_comp)
gps_comp_rmse <- sqrt(mean((test_comp$composite_score - gps_comp_pred)^2, na.rm = TRUE))

hr_comp_model <- lm(composite_score ~ hr_high_intensity + days_post_match, data = train_comp)
hr_comp_pred <- predict(hr_comp_model, test_comp)
hr_comp_rmse <- sqrt(mean((test_comp$composite_score - hr_comp_pred)^2, na.rm = TRUE))

cat("Composite Results:\n")
cat("- Best time series RMSE:", round(comp_accuracy$RMSE[1], 5), "\n")
cat("- GPS predictor RMSE:", round(gps_comp_rmse, 5), "\n") 
cat("- HR predictor RMSE:", round(hr_comp_rmse, 5), "\n")

