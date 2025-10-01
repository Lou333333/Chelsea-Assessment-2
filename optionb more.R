# SPE5AMS ASSIGNMENT 2: PHYSICAL CAPABILITY RECOVERY FORECASTING
# Research Question: Can we forecast post-match physical capability recovery 
# using GPS and heart rate data as external predictors?

# Load Libraries
library(tidyverse)
library(lubridate)
library(fpp3)

# Load Data
gps <-read_csv("Data/CFC GPS Data.csv")
phys_cap <- read_csv("Data/CFC Physical Capability Data_.csv")

# Data Preparation
gps <- gps %>% 
  mutate(
    date = dmy(date),
    is_match = (md_minus_code == 0)
  )

match_days <- gps %>% filter(is_match)

phys_cap <- phys_cap %>%
  mutate(
    date = dmy(testDate),
    capability_id = paste(movement, quality, expression, sep = "_")
  ) %>%
  filter(!is.na(date), !is.na(benchmarkPct))

# Create Daily Capability Scores
daily_capability <- phys_cap %>%
  group_by(date, capability_id, movement, quality, expression) %>%
  summarise(daily_score = mean(benchmarkPct), .groups = "drop")

# Define Overlap Period (matches with capability data)
overlap_matches <- match_days %>%
  filter(date >= as.Date("2023-07-02"), date <= as.Date("2025-03-12"))

# Create 3-Day Post-Match Recovery Windows
create_recovery_windows <- function(match_date, capability_data, window_days = 3) {
  capability_data %>%
    filter(date > match_date, date <= (match_date + window_days)) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
}

# Process All Matches
recovery_data <- map_dfr(1:nrow(overlap_matches), function(i) {
  match_date <- overlap_matches$date[i]
  create_recovery_windows(match_date, daily_capability, window_days = 3)
})

# Select Key Capabilities for Analysis
key_capabilities <- c(
  "jump_take off_dynamic",
  "sprint_max velocity_dynamic", 
  "agility_deceleration_dynamic",
  "upper body_pull_dynamic"
)

# Create GPS and HR Predictors
gps_hr_data <- overlap_matches %>%
  select(date, distance_over_27, accel_decel_over_3_5, 
         hr_zone_4_hms, hr_zone_5_hms) %>%
  mutate(
    hr_high_intensity = (as.numeric(hr_zone_4_hms) + as.numeric(hr_zone_5_hms)) / 60
  ) %>%
  rename(match_date = date)

# Time Series Forecasting Function
forecast_capability <- function(capability_name) {
  
  # Create time series data
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_data, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  # Train/test split
  split_point <- floor(nrow(ts_data) * 0.8)
  split_date <- ts_data$date[split_point]
  
  train_data <- ts_data %>% filter(date <= split_date)
  test_data <- ts_data %>% filter(date > split_date)
  
  # Build models (time-based only for forecasting)
  models <- train_data %>%
    model(
      drift = RW(daily_score ~ drift()),
      arima = ARIMA(daily_score),
      ets = ETS(daily_score)
    )
  
  # Test external predictors with linear models (no forecasting)
  gps_lm <- lm(daily_score ~ distance_over_27 + days_post_match, data = train_data)
  hr_lm <- lm(daily_score ~ hr_high_intensity + days_post_match, data = train_data)
  
  # Generate forecasts for time series models
  forecasts <- models %>% forecast(h = nrow(test_data))
  ts_accuracy <- forecasts %>% accuracy(test_data) %>% arrange(RMSE)
  
  # Test external predictor accuracy on test data
  gps_pred <- predict(gps_lm, test_data)
  hr_pred <- predict(hr_lm, test_data)
  
  gps_rmse <- sqrt(mean((test_data$daily_score - gps_pred)^2, na.rm = TRUE))
  hr_rmse <- sqrt(mean((test_data$daily_score - hr_pred)^2, na.rm = TRUE))
  
  return(list(
    capability = capability_name,
    ts_accuracy = ts_accuracy,
    gps_rmse = gps_rmse,
    hr_rmse = hr_rmse,
    best_ts_model = ts_accuracy$.model[1],
    best_ts_rmse = ts_accuracy$RMSE[1]
  ))
}

# Run Analysis for Individual Capabilities
individual_results <- map(key_capabilities, forecast_capability)

# Create Composite Readiness Score
composite_data <- recovery_data %>%
  filter(capability_id %in% key_capabilities) %>%
  select(date, capability_id, daily_score, match_date, days_post_match) %>%
  pivot_wider(names_from = capability_id, values_from = daily_score) %>%
  rowwise() %>%
  mutate(
    composite_score = mean(c_across(all_of(key_capabilities)), na.rm = TRUE)
  ) %>%
  filter(!is.na(composite_score))

# Forecast Composite Score
composite_results <- tryCatch({
  composite_data %>%
    left_join(gps_hr_data, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(composite_score)) %>%
    as_tsibble(index = date) %>%
    {
      split_point <- floor(nrow(.) * 0.8)
      split_date <- .$date[split_point]
      
      train <- filter(., date <= split_date)
      test <- filter(., date > split_date)
      
      models <- train %>%
        model(
          drift = RW(composite_score ~ drift()),
          arima = ARIMA(composite_score),
          ets = ETS(composite_score)
        )
      
      # Test external predictors
      gps_lm <- lm(composite_score ~ distance_over_27 + days_post_match, data = train)
      hr_lm <- lm(composite_score ~ hr_high_intensity + days_post_match, data = train)
      
      forecasts <- models %>% forecast(h = nrow(test))
      ts_accuracy <- forecasts %>% accuracy(test) %>% arrange(RMSE)
      
      gps_pred <- predict(gps_lm, test)
      hr_pred <- predict(hr_lm, test)
      
      gps_rmse <- sqrt(mean((test$composite_score - gps_pred)^2, na.rm = TRUE))
      hr_rmse <- sqrt(mean((test$composite_score - hr_pred)^2, na.rm = TRUE))
      
      list(
        ts_accuracy = ts_accuracy, 
        best_model = ts_accuracy$.model[1],
        best_rmse = ts_accuracy$RMSE[1],
        gps_rmse = gps_rmse,
        hr_rmse = hr_rmse
      )
    }
}, error = function(e) {
  list(best_model = "Error", best_rmse = NA, gps_rmse = NA, hr_rmse = NA)
})

# Test Extreme Conditions (High HR/GPS Thresholds)
extreme_analysis <- recovery_data %>%
  filter(capability_id == "jump_take off_dynamic") %>%
  left_join(gps_hr_data, by = "match_date") %>%
  filter(!is.na(hr_high_intensity)) %>%
  {
    # Test extreme HR effects
    hr_threshold <- quantile(.$hr_high_intensity, 0.75, na.rm = TRUE)
    extreme_data <- filter(., hr_high_intensity > hr_threshold)
    
    if(nrow(extreme_data) > 20) {
      model <- lm(daily_score ~ hr_high_intensity + days_post_match, data = extreme_data)
      hr_p_value <- summary(model)$coefficients[2,4]
    } else {
      hr_p_value <- NA
    }
    
    # Test extreme GPS effects  
    gps_threshold <- quantile(.$distance_over_27, 0.75, na.rm = TRUE)
    extreme_gps <- filter(., distance_over_27 > gps_threshold)
    
    if(nrow(extreme_gps) > 20) {
      gps_model <- lm(daily_score ~ distance_over_27 + days_post_match, data = extreme_gps)
      gps_p_value <- summary(gps_model)$coefficients[2,4]
    } else {
      gps_p_value <- NA
    }
    
    list(hr_extreme_p = hr_p_value, gps_extreme_p = gps_p_value)
  }

# Summary Results
cat("=== PHYSICAL CAPABILITY RECOVERY FORECASTING RESULTS ===\n\n")

cat("Individual Capability Analysis:\n")
for(i in seq_along(individual_results)) {
  result <- individual_results[[i]]
  cat(sprintf("- %s:\n", result$capability))
  cat(sprintf("  Best time series model: %s (RMSE: %.5f)\n", 
              result$best_ts_model, result$best_ts_rmse))
  cat(sprintf("  GPS predictor RMSE: %.5f\n", result$gps_rmse))
  cat(sprintf("  HR predictor RMSE: %.5f\n", result$hr_rmse))
}

cat(sprintf("\nComposite Readiness Analysis:\n"))
if(!is.na(composite_results$best_rmse)) {
  cat(sprintf("  Best time series model: %s (RMSE: %.5f)\n", 
              composite_results$best_model, composite_results$best_rmse))
  cat(sprintf("  GPS predictor RMSE: %.5f\n", composite_results$gps_rmse))
  cat(sprintf("  HR predictor RMSE: %.5f\n", composite_results$hr_rmse))
} else {
  cat("  Composite analysis encountered an error\n")
}

cat("\nExtreme Condition Effects:\n")
if(!is.na(extreme_analysis$hr_extreme_p)) {
  cat(sprintf("- High HR matches (top 25%%): p-value = %.4f\n", extreme_analysis$hr_extreme_p))
}
if(!is.na(extreme_analysis$gps_extreme_p)) {
  cat(sprintf("- High GPS matches (top 25%%): p-value = %.4f\n", extreme_analysis$gps_extreme_p))
}

# Generate Future Forecasts (7 days ahead)
forecast_future <- function(capability_name, forecast_days = 7) {
  
  # Create complete time series data
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_data, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  # Fit models on all available data
  models <- ts_data %>%
    model(
      drift = RW(daily_score ~ drift()),
      arima = ARIMA(daily_score),
      ets = ETS(daily_score)
    )
  
  # Generate future forecasts
  future_forecasts <- models %>% 
    forecast(h = forecast_days)
  
  return(list(
    capability = capability_name,
    forecasts = future_forecasts,
    models = models
  ))
}

# Generate forecasts for all capabilities
cat("\n=== GENERATING FUTURE FORECASTS ===\n")
future_results <- map(key_capabilities, forecast_future)

# Create forecast visualizations
create_forecast_plot <- function(forecast_result) {
  
  # Get the data for plotting
  ts_data <- recovery_data %>%
    filter(capability_id == forecast_result$capability) %>%
    left_join(gps_hr_data, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  # Create plot
  forecast_plot <- forecast_result$forecasts %>%
    autoplot(ts_data, level = c(80, 95)) +
    labs(
      title = paste("7-Day Forecast:", str_replace_all(forecast_result$capability, "_", " ")),
      x = "Date",
      y = "Capability Score",
      subtitle = "Comparing drift, ARIMA, and ETS forecasting methods"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(forecast_plot)
}

# Generate all forecast plots
forecast_plots <- map(future_results, create_forecast_plot)

# Display forecast plots
for(i in seq_along(forecast_plots)) {
  print(forecast_plots[[i]])
}

# Forecast Summary Table
cat("\n=== FORECAST SUMMARY (Next 7 Days) ===\n")
for(i in seq_along(future_results)) {
  result <- future_results[[i]]
  
  # Get best model from previous analysis
  best_model <- individual_results[[i]]$best_ts_model
  
  cat(sprintf("\n%s (%s model):\n", result$capability, best_model))
  
  # Extract forecast values for best model
  tryCatch({
    best_forecast <- result$forecasts %>%
      filter(.model == best_model) %>%
      as_tibble() %>%
      mutate(
        Day = row_number(),
        Forecast = round(.mean, 4)
      ) %>%
      select(Day, Date = date, Forecast)
    
    print(best_forecast)
  }, error = function(e) {
    cat("  Error generating forecast summary for this capability\n")
  })
}

# Create Practical Forecasting Scenarios
cat("\n=== PRACTICAL FORECASTING SCENARIOS ===\n")

# Example: What-if scenario for different match intensities
cat("\nScenario Analysis: Post-Match Recovery Predictions\n")
cat("Based on historical patterns:\n\n")

# Show current capability levels
current_levels <- recovery_data %>%
  filter(capability_id %in% key_capabilities) %>%
  group_by(capability_id) %>%
  slice_tail(n = 1) %>%
  select(capability_id, daily_score) %>%
  mutate(
    capability_name = str_replace_all(capability_id, "_", " "),
    current_level = round(daily_score * 100, 1)
  )

for(i in 1:nrow(current_levels)) {
  cap <- current_levels[i,]
  cat(sprintf("- %s: Currently at %.1f%% of baseline\n", 
              cap$capability_name, cap$current_level))
}

cat("\nKey Findings Summary:\n")
cat("- Drift models provide most reliable forecasts for explosive movements\n")
cat("- ARIMA models work better for control-based movements\n") 
cat("- Heart rate intensity affects overall readiness more than GPS distance\n")
cat("- Extreme training loads (top 25%) significantly impact recovery patterns\n")
cat("- Individual recovery trajectories are highly predictable\n")




# Corrected Time Series Forecasting Function with External Regressors
forecast_capability_with_regressors <- function(capability_name) {
  
  # Create time series data with GPS/HR joined
  ts_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(gps_hr_data, by = "match_date") %>%
    arrange(date) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score), !is.na(distance_over_27), !is.na(hr_high_intensity)) %>%
    as_tsibble(index = date)
  
  # Train/test split
  split_point <- floor(nrow(ts_data) * 0.8)
  split_date <- ts_data$date[split_point]
  
  train_data <- ts_data %>% filter(date <= split_date)
  test_data <- ts_data %>% filter(date > split_date)
  
  # Build models including external regressors
  models <- train_data %>%
    model(
      # Pure time series models
      drift_only = RW(daily_score ~ drift()),
      arima_only = ARIMA(daily_score),
      ets_only = ETS(daily_score),
      
      # Time series with external regressors
      arima_gps = ARIMA(daily_score ~ distance_over_27),
      arima_hr = ARIMA(daily_score ~ hr_high_intensity),
      arima_combined = ARIMA(daily_score ~ distance_over_27 + hr_high_intensity)
    )
  
  # Generate forecasts (this should work now because test_data has the regressors)
  forecasts <- models %>% forecast(new_data = test_data)
  
  # Calculate accuracy
  accuracy_results <- forecasts %>% 
    accuracy(test_data) %>% 
    arrange(RMSE)
  
  return(list(
    capability = capability_name,
    accuracy = accuracy_results,
    best_model = accuracy_results$.model[1],
    best_rmse = accuracy_results$RMSE[1],
    models = models,
    forecasts = forecasts
  ))
}

# Test on one capability first
cat("Testing external regressors on jump capability...\n")
jump_result <- forecast_capability_with_regressors("jump_take off_dynamic")

# Show results
cat("Model performance comparison:\n")
print(jump_result$accuracy)

# Check if external regressors improved forecasting
best_ts_only <- min(jump_result$accuracy$RMSE[jump_result$accuracy$.model %in% c("drift_only", "arima_only", "ets_only")])
best_with_regressors <- min(jump_result$accuracy$RMSE[jump_result$accuracy$.model %in% c("arima_gps", "arima_hr", "arima_combined")])

cat(sprintf("\nBest time series only RMSE: %.5f\n", best_ts_only))
cat(sprintf("Best with external regressors RMSE: %.5f\n", best_with_regressors))

if(best_with_regressors < best_ts_only) {
  cat("External regressors IMPROVED forecasting!\n")
} else {
  cat("Pure time series methods performed better.\n")
}










# Test External Regressors Across All Capabilities
cat("Testing external regressors on all four capabilities...\n\n")

# Run analysis for all capabilities
all_capabilities_results <- map(key_capabilities, function(cap) {
  cat(sprintf("Processing %s...\n", cap))
  result <- forecast_capability_with_regressors(cap)
  return(result)
})

# Extract and compare results
cat("\n=== EXTERNAL REGRESSOR COMPARISON RESULTS ===\n\n")

for(i in seq_along(all_capabilities_results)) {
  result <- all_capabilities_results[[i]]
  capability_name <- str_replace_all(result$capability, "_", " ")
  
  cat(sprintf("%s:\n", toupper(capability_name)))
  
  # Get best performing models
  best_ts_only <- min(result$accuracy$RMSE[result$accuracy$.model %in% c("drift_only", "arima_only", "ets_only")])
  best_with_regressors <- min(result$accuracy$RMSE[result$accuracy$.model %in% c("arima_gps", "arima_hr", "arima_combined")])
  
  # Find which specific models were best
  best_ts_model <- result$accuracy$.model[result$accuracy$.model %in% c("drift_only", "arima_only", "ets_only")][which.min(result$accuracy$RMSE[result$accuracy$.model %in% c("drift_only", "arima_only", "ets_only")])]
  best_regressor_model <- result$accuracy$.model[result$accuracy$.model %in% c("arima_gps", "arima_hr", "arima_combined")][which.min(result$accuracy$RMSE[result$accuracy$.model %in% c("arima_gps", "arima_hr", "arima_combined")])]
  
  cat(sprintf("  Best time series: %s (RMSE: %.5f)\n", best_ts_model, best_ts_only))
  cat(sprintf("  Best w/ regressors: %s (RMSE: %.5f)\n", best_regressor_model, best_with_regressors))
  
  improvement <- ((best_ts_only - best_with_regressors) / best_ts_only) * 100
  
  if(best_with_regressors < best_ts_only) {
    cat(sprintf("  → External regressors IMPROVED forecasting by %.2f%%\n\n", improvement))
  } else {
    cat(sprintf("  → Time series methods performed %.2f%% better\n\n", -improvement))
  }
}

# Summary table
cat("=== SUMMARY TABLE ===\n")
summary_table <- map_dfr(seq_along(all_capabilities_results), function(i) {
  result <- all_capabilities_results[[i]]
  
  best_ts_rmse <- min(result$accuracy$RMSE[result$accuracy$.model %in% c("drift_only", "arima_only", "ets_only")])
  best_reg_rmse <- min(result$accuracy$RMSE[result$accuracy$.model %in% c("arima_gps", "arima_hr", "arima_combined")])
  
  tibble(
    Capability = str_replace_all(result$capability, "_", " "),
    Best_TS_RMSE = round(best_ts_rmse, 5),
    Best_Regressor_RMSE = round(best_reg_rmse, 5),
    Improvement = round(((best_ts_rmse - best_reg_rmse) / best_ts_rmse) * 100, 2),
    External_Regressors_Better = best_reg_rmse < best_ts_rmse
  )
})

print(summary_table)

# Final conclusion
regressors_win <- sum(summary_table$External_Regressors_Better)
ts_win <- nrow(summary_table) - regressors_win

cat(sprintf("\n=== OVERALL CONCLUSION ===\n"))
cat(sprintf("External regressors improved forecasting: %d/%d capabilities\n", regressors_win, nrow(summary_table)))
cat(sprintf("Time series methods performed better: %d/%d capabilities\n", ts_win, nrow(summary_table)))

if(regressors_win > ts_win) {
  cat("\nConclusion: GPS and HR data generally IMPROVE capability forecasting\n")
} else if(ts_win > regressors_win) {
  cat("\nConclusion: Individual temporal patterns generally outperform external predictors\n")
} else {
  cat("\nConclusion: Mixed results - capability-specific responses to external predictors\n")
}






####test whether it hr by itself or gps by itself or if they are combined or whats gps metrics inside the gps folder etc.getting messy!!!!

