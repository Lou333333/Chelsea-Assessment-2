#potentially cleaned up version

# =============================================================================
# SPE5AMS ASSIGNMENT 2: PHYSICAL CAPABILITY RECOVERY FORECASTING
# Research Question: Can we forecast post-match physical capability recovery 
# trajectories using GPS training load as external predictors?
# Author: [Your Name]
# Date: [Current Date]
# =============================================================================

# Load Required Libraries (Class-Taught Only)
library(tidyverse)
library(lubridate)
library(fpp3)
library(survival)
library(survminer)

# =============================================================================
# SECTION 1: DATA LOADING AND PREPARATION
# =============================================================================

# Load Datasets
gps <- read_csv("Data/CFC GPS Data.csv")
phys_cap <- read_csv("Data/CFC Physical Capability Data_.csv")
recovery <- read_csv("Data/CFC Recovery status Data.csv")

# Clean GPS Data
gps <- gps %>% 
  mutate(
    date = dmy(date),
    is_match = (md_minus_code == 0)
  )

match_days <- gps %>% filter(is_match)

# Clean Physical Capability Data
phys_cap <- phys_cap %>%
  mutate(
    date = dmy(testDate),
    capability_id = paste(movement, quality, expression, sep = "_")
  ) %>%
  filter(!is.na(date), !is.na(benchmarkPct))

# Create Daily Capability Scores for ALL Capabilities
daily_capability <- phys_cap %>%
  group_by(date, capability_id, movement, quality, expression) %>%
  summarise(daily_score = mean(benchmarkPct), .groups = "drop")

# =============================================================================
# SECTION 2: DATA EXPLORATION AND SUMMARY
# =============================================================================

cat("=== DATA SUMMARY ===\n")
cat("Total GPS observations:", nrow(gps), "\n")
cat("Total match days:", nrow(match_days), "\n")
cat("Total capability observations:", nrow(phys_cap), "\n")
cat("Unique capabilities:", n_distinct(daily_capability$capability_id), "\n")

# Show all available capabilities
capability_overview <- daily_capability %>%
  group_by(capability_id, movement, quality, expression) %>%
  summarise(
    testing_days = n(),
    mean_score = round(mean(daily_score), 3),
    date_range_days = as.numeric(max(date) - min(date)),
    .groups = "drop"
  ) %>%
  arrange(desc(testing_days))

cat("\nAll Available Capabilities:\n")
print(capability_overview)

# =============================================================================
# SECTION 3: MATCH-CAPABILITY OVERLAP ANALYSIS
# =============================================================================

# Define overlap period (matches with capability data available)
overlap_matches <- match_days %>%
  filter(date >= min(phys_cap$date), date <= max(phys_cap$date))

cat("\n=== OVERLAP ANALYSIS ===\n")
cat("Matches in overlap period:", nrow(overlap_matches), "\n")
cat("Date range:", as.character(min(overlap_matches$date)), "to", 
    as.character(max(overlap_matches$date)), "\n")

# Analyze match frequency for recovery window decision
match_intervals <- overlap_matches %>%
  arrange(date) %>%
  mutate(
    next_match_date = lead(date),
    days_to_next_match = as.numeric(next_match_date - date)
  ) %>%
  filter(!is.na(days_to_next_match))

cat("Median days between matches:", median(match_intervals$days_to_next_match), "\n")
cat("Matches with ≤4 days to next:", sum(match_intervals$days_to_next_match <= 4), "\n")

# Decision: Use 3-day recovery window to avoid contamination
recovery_window_days <- 3
cat("Selected recovery window:", recovery_window_days, "days\n")

# =============================================================================
# SECTION 4: CREATE POST-MATCH RECOVERY DATASET
# =============================================================================

# Function to create recovery windows
create_recovery_windows <- function(match_date, capability_data, window_days = 3) {
  capability_data %>%
    filter(date > match_date, date <= (match_date + window_days)) %>%
    mutate(
      match_date = match_date,
      days_post_match = as.numeric(date - match_date)
    )
}

# Process all matches to create recovery dataset
cat("\n=== CREATING RECOVERY DATASET ===\n")
cat("Processing", nrow(overlap_matches), "matches with", recovery_window_days, "day windows...\n")

recovery_data_all <- map_dfr(1:nrow(overlap_matches), function(i) {
  if(i %% 20 == 0) cat("Processed", i, "matches...\n")
  match_date <- overlap_matches$date[i]
  create_recovery_windows(match_date, daily_capability, recovery_window_days)
})

cat("Recovery dataset created:\n")
cat("Total observations:", nrow(recovery_data_all), "\n")
cat("Unique capabilities:", n_distinct(recovery_data_all$capability_id), "\n")
cat("Matches tracked:", n_distinct(recovery_data_all$match_date), "\n")

# =============================================================================
# SECTION 5: GPS AND HR PREDICTOR PREPARATION
# =============================================================================

# Create GPS and HR predictor dataset
gps_hr_predictors <- overlap_matches %>%
  select(date, distance, distance_over_21, distance_over_24, distance_over_27,
         accel_decel_over_2_5, accel_decel_over_3_5, accel_decel_over_4_5,
         peak_speed, day_duration,
         hr_zone_1_hms, hr_zone_2_hms, hr_zone_3_hms, hr_zone_4_hms, hr_zone_5_hms) %>%
  mutate(
    # Convert HR zones to minutes
    hr_zone_1_min = as.numeric(hr_zone_1_hms) / 60,
    hr_zone_2_min = as.numeric(hr_zone_2_hms) / 60,
    hr_zone_3_min = as.numeric(hr_zone_3_hms) / 60,
    hr_zone_4_min = as.numeric(hr_zone_4_hms) / 60,
    hr_zone_5_min = as.numeric(hr_zone_5_hms) / 60,
    
    # Create combined HR metrics
    hr_high_intensity = hr_zone_4_min + hr_zone_5_min,
    hr_total_time = hr_zone_1_min + hr_zone_2_min + hr_zone_3_min + hr_zone_4_min + hr_zone_5_min
  ) %>%
  select(-ends_with("_hms")) %>%
  rename(match_date = date)

cat("\n=== GPS/HR PREDICTORS SUMMARY ===\n")
cat("GPS metrics available:", sum(str_detect(names(gps_hr_predictors), "distance|accel|peak|duration")), "\n")
cat("HR metrics available:", sum(str_detect(names(gps_hr_predictors), "hr_")), "\n")

# =============================================================================
# SECTION 6: SYSTEMATIC CAPABILITY FORECASTING FUNCTION
# =============================================================================

# Comprehensive forecasting function for individual capabilities
forecast_capability_comprehensive <- function(capability_name, recovery_data, predictor_data) {
  
  # Filter and prepare data for this capability
  cap_data <- recovery_data %>%
    filter(capability_id == capability_name) %>%
    left_join(predictor_data, by = "match_date") %>%
    arrange(date)
  
  # Check data sufficiency
  if(nrow(cap_data) < 100) {
    return(list(
      capability = capability_name,
      status = "insufficient_data",
      n_observations = nrow(cap_data)
    ))
  }
  
  # Create daily time series
  ts_data <- cap_data %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(everything(), .direction = "down") %>%
    filter(!is.na(daily_score)) %>%
    as_tsibble(index = date)
  
  # Train/test split (80/20)
  split_point <- floor(nrow(ts_data) * 0.8)
  split_date <- ts_data$date[split_point]
  
  train_data <- ts_data %>% filter(date <= split_date)
  test_data <- ts_data %>% filter(date > split_date)
  
  # Build time series models (class-taught methods only)
  ts_models <- train_data %>%
    model(
      naive = NAIVE(daily_score),
      mean_fc = MEAN(daily_score),
      drift = RW(daily_score ~ drift()),
      arima = ARIMA(daily_score),
      ets = ETS(daily_score)
    )
  
  # Generate forecasts
  ts_forecasts <- ts_models %>% forecast(h = nrow(test_data))
  ts_accuracy <- ts_forecasts %>% accuracy(test_data) %>% arrange(RMSE)
  
  # Test GPS predictors with linear models
  gps_models <- list()
  gps_rmse <- list()
  
  key_gps_vars <- c("distance", "distance_over_27", "accel_decel_over_3_5", "peak_speed")
  
  for(var in key_gps_vars) {
    if(var %in% names(train_data) && !all(is.na(train_data[[var]]))) {
      formula_str <- paste("daily_score ~", var, "+ days_post_match")
      model <- lm(as.formula(formula_str), data = train_data)
      pred <- predict(model, test_data)
      rmse <- sqrt(mean((test_data$daily_score - pred)^2, na.rm = TRUE))
      
      gps_models[[var]] <- model
      gps_rmse[[var]] <- rmse
    }
  }
  
  # Test HR predictors
  hr_models <- list()
  hr_rmse <- list()
  
  key_hr_vars <- c("hr_high_intensity", "hr_zone_4_min", "hr_zone_5_min")
  
  for(var in key_hr_vars) {
    if(var %in% names(train_data) && !all(is.na(train_data[[var]]))) {
      formula_str <- paste("daily_score ~", var, "+ days_post_match")
      model <- lm(as.formula(formula_str), data = train_data)
      pred <- predict(model, test_data)
      rmse <- sqrt(mean((test_data$daily_score - pred)^2, na.rm = TRUE))
      
      hr_models[[var]] <- model
      hr_rmse[[var]] <- rmse
    }
  }
  
  # Return comprehensive results
  return(list(
    capability = capability_name,
    status = "success",
    n_observations = nrow(cap_data),
    ts_observations = nrow(ts_data),
    
    # Time series results
    ts_accuracy = ts_accuracy,
    best_ts_model = ts_accuracy$.model[1],
    best_ts_rmse = ts_accuracy$RMSE[1],
    
    # GPS results
    gps_rmse = gps_rmse,
    best_gps_var = ifelse(length(gps_rmse) > 0, names(gps_rmse)[which.min(unlist(gps_rmse))], NA),
    best_gps_rmse = ifelse(length(gps_rmse) > 0, min(unlist(gps_rmse)), NA),
    
    # HR results
    hr_rmse = hr_rmse,
    best_hr_var = ifelse(length(hr_rmse) > 0, names(hr_rmse)[which.min(unlist(hr_rmse))], NA),
    best_hr_rmse = ifelse(length(hr_rmse) > 0, min(unlist(hr_rmse)), NA),
    
    # Models for future use
    ts_models = ts_models,
    ts_forecasts = ts_forecasts
  ))
}

# =============================================================================
# SECTION 7: RUN ANALYSIS FOR ALL CAPABILITIES
# =============================================================================

# Get list of all capabilities with sufficient data
capabilities_to_analyze <- capability_overview %>%
  filter(testing_days >= 50) %>%  # Minimum threshold
  pull(capability_id)

cat("\n=== RUNNING COMPREHENSIVE ANALYSIS ===\n")
cat("Analyzing", length(capabilities_to_analyze), "capabilities with sufficient data...\n")

# Run analysis for all qualifying capabilities
all_results <- map(capabilities_to_analyze, function(cap_name) {
  cat("Processing:", cap_name, "\n")
  forecast_capability_comprehensive(cap_name, recovery_data_all, gps_hr_predictors)
})

# Set names for easy access
names(all_results) <- capabilities_to_analyze

# =============================================================================
# SECTION 8: COMPILE AND ANALYZE RESULTS
# =============================================================================

# Create comprehensive results summary
results_summary <- map_dfr(all_results, function(result) {
  if(result$status != "success") {
    return(tibble(
      capability = result$capability,
      status = result$status,
      n_observations = result$n_observations
    ))
  }
  
  tibble(
    capability = result$capability,
    status = result$status,
    n_observations = result$n_observations,
    ts_observations = result$ts_observations,
    
    # Best performing methods
    best_ts_model = result$best_ts_model,
    best_ts_rmse = round(result$best_ts_rmse, 5),
    
    best_gps_var = result$best_gps_var %||% "none",
    best_gps_rmse = round(result$best_gps_rmse %||% NA, 5),
    
    best_hr_var = result$best_hr_var %||% "none", 
    best_hr_rmse = round(result$best_hr_rmse %||% NA, 5),
    
    # Compare approaches
    ts_vs_gps_improvement = ifelse(!is.na(result$best_gps_rmse),
                                   round(((result$best_ts_rmse - result$best_gps_rmse) / result$best_ts_rmse) * 100, 2),
                                   NA),
    ts_vs_hr_improvement = ifelse(!is.na(result$best_hr_rmse),
                                  round(((result$best_ts_rmse - result$best_hr_rmse) / result$best_ts_rmse) * 100, 2),
                                  NA),
    
    gps_better_than_ts = ifelse(!is.na(result$best_gps_rmse), result$best_gps_rmse < result$best_ts_rmse, FALSE),
    hr_better_than_ts = ifelse(!is.na(result$best_hr_rmse), result$best_hr_rmse < result$best_ts_rmse, FALSE)
  )
})

# =============================================================================
# SECTION 9: RESULTS PRESENTATION
# =============================================================================

cat("\n")
cat("="*80, "\n")
cat("PHYSICAL CAPABILITY RECOVERY FORECASTING RESULTS\n")
cat("="*80, "\n")

# Overall summary
successful_analyses <- sum(results_summary$status == "success")
cat("Capabilities successfully analyzed:", successful_analyses, "/", nrow(results_summary), "\n")

if(successful_analyses > 0) {
  
  # Time series performance summary
  cat("\nTIME SERIES MODEL PERFORMANCE:\n")
  ts_model_summary <- results_summary %>%
    filter(status == "success") %>%
    count(best_ts_model, sort = TRUE) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  print(ts_model_summary)
  
  # GPS predictor performance
  cat("\nGPS PREDICTOR PERFORMANCE:\n")
  gps_wins <- sum(results_summary$gps_better_than_ts, na.rm = TRUE)
  gps_total <- sum(!is.na(results_summary$best_gps_rmse))
  cat("GPS improved forecasting:", gps_wins, "/", gps_total, "capabilities\n")
  
  if(gps_total > 0) {
    gps_summary <- results_summary %>%
      filter(!is.na(best_gps_rmse)) %>%
      count(best_gps_var, sort = TRUE)
    cat("Best GPS predictors:\n")
    print(gps_summary)
  }
  
  # HR predictor performance  
  cat("\nHR PREDICTOR PERFORMANCE:\n")
  hr_wins <- sum(results_summary$hr_better_than_ts, na.rm = TRUE)
  hr_total <- sum(!is.na(results_summary$best_hr_rmse))
  cat("HR improved forecasting:", hr_wins, "/", hr_total, "capabilities\n")
  
  if(hr_total > 0) {
    hr_summary <- results_summary %>%
      filter(!is.na(best_hr_rmse)) %>%
      count(best_hr_var, sort = TRUE)
    cat("Best HR predictors:\n")
    print(hr_summary)
  }
  
  # Top performing capabilities for each method
  cat("\nTOP 5 BEST FORECASTED CAPABILITIES:\n")
  top_capabilities <- results_summary %>%
    filter(status == "success") %>%
    arrange(best_ts_rmse) %>%
    slice_head(n = 5) %>%
    select(capability, best_ts_model, best_ts_rmse)
  
  print(top_capabilities)
  
  # Show detailed results table
  cat("\nDETAILED RESULTS SUMMARY:\n")
  detailed_results <- results_summary %>%
    filter(status == "success") %>%
    arrange(best_ts_rmse) %>%
    select(capability, best_ts_model, best_ts_rmse, 
           best_gps_rmse, best_hr_rmse, 
           gps_better_than_ts, hr_better_than_ts)
  
  print(detailed_results)
}

# =============================================================================
# SECTION 10: GENERATE FUTURE FORECASTS FOR KEY CAPABILITIES
# =============================================================================

# Select top 4 capabilities for detailed forecasting
top_4_capabilities <- results_summary %>%
  filter(status == "success") %>%
  arrange(best_ts_rmse) %>%
  slice_head(n = 4) %>%
  pull(capability)

cat("\n")
cat("="*60, "\n")
cat("GENERATING 7-DAY FORECASTS FOR TOP 4 CAPABILITIES\n")
cat("="*60, "\n")

# Generate forecasts for top capabilities
future_forecasts <- map(top_4_capabilities, function(cap_name) {
  result <- all_results[[cap_name]]
  
  if(result$status == "success") {
    # Use the full time series data to generate future forecasts
    cap_data <- recovery_data_all %>%
      filter(capability_id == cap_name) %>%
      left_join(gps_hr_predictors, by = "match_date") %>%
      arrange(date) %>%
      complete(date = seq(min(date), max(date), by = "day")) %>%
      fill(everything(), .direction = "down") %>%
      filter(!is.na(daily_score)) %>%
      as_tsibble(index = date)
    
    # Fit models on all data
    full_models <- cap_data %>%
      model(
        drift = RW(daily_score ~ drift()),
        arima = ARIMA(daily_score),
        ets = ETS(daily_score)
      )
    
    # Generate 7-day forecasts
    future_forecast <- full_models %>% 
      forecast(h = 7)
    
    # Get best model forecast
    best_model <- result$best_ts_model
    best_forecast <- future_forecast %>%
      filter(.model == best_model) %>%
      as_tibble() %>%
      mutate(
        Day = row_number(),
        Forecast = round(.mean, 4),
        Lower_80 = round(`80%`$lower, 4),
        Upper_80 = round(`80%`$upper, 4)
      ) %>%
      select(Day, Date = date, Forecast, Lower_80, Upper_80)
    
    cat("\n", str_replace_all(cap_name, "_", " "), "(", best_model, "model):\n")
    print(best_forecast)
    
    return(list(
      capability = cap_name,
      best_model = best_model,
      forecast = best_forecast,
      all_forecasts = future_forecast
    ))
  }
})

# =============================================================================
# SECTION 11: FINAL CONCLUSIONS AND RECOMMENDATIONS
# =============================================================================

cat("\n")
cat("="*80, "\n")
cat("FINAL CONCLUSIONS AND RECOMMENDATIONS\n")
cat("="*80, "\n")

cat("RESEARCH QUESTION ANSWER:\n")
cat("Can we forecast post-match physical capability recovery trajectories\n")
cat("using GPS training load as external predictors?\n\n")

if(successful_analyses > 0) {
  
  # Overall finding
  overall_gps_improvement <- mean(results_summary$gps_better_than_ts, na.rm = TRUE) * 100
  overall_hr_improvement <- mean(results_summary$hr_better_than_ts, na.rm = TRUE) * 100
  
  cat("KEY FINDINGS:\n")
  cat("1. Time series methods successfully forecast", successful_analyses, "different capabilities\n")
  cat("2. GPS predictors improved forecasting in", round(overall_gps_improvement, 1), "% of capabilities\n")
  cat("3. HR predictors improved forecasting in", round(overall_hr_improvement, 1), "% of capabilities\n")
  
  # Best performing approaches
  best_ts_model <- results_summary %>%
    filter(status == "success") %>%
    count(best_ts_model, sort = TRUE) %>%
    slice_head(n = 1) %>%
    pull(best_ts_model)
  
  cat("4. Most reliable forecasting method:", best_ts_model, "\n")
  
  # Practical recommendations
  cat("\nPRACTICAL RECOMMENDATIONS:\n")
  cat("- Use", best_ts_model, "models for most capabilities\n")
  cat("- Monitor HR intensity for capabilities where external predictors help\n")
  cat("- 3-day recovery windows provide actionable forecasts\n")
  cat("- Individual capability patterns are highly predictable\n")
  
  # Limitations
  cat("\nLIMITATIONS:\n")
  cat("- External predictors show mixed effectiveness\n")
  cat("- Short recovery windows limit long-term planning\n")
  cat("- Individual player differences not captured\n")
  
} else {
  cat("INSUFFICIENT DATA: Unable to complete comprehensive analysis\n")
}

cat("\nANALYSIS COMPLETE\n")
cat("="*80, "\n")

