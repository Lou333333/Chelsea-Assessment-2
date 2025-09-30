# SPE5AMS ASSIGNMENT 2: FORECASTING GPS INTENSITY & INJURY RISK ====
# Louis Boyd: 22668371
# Due Oct 20 2025


#Load librarys====
library(tidyverse)    # Data manipulation and visualization
library(lubridate)    # Date handling
library(survival)     # Survival analysis 
library(survminer)    # Survival plots
library(fpp3)         # Time series forecasting 


#Load datasets ====
#1. GPS Data 
gps <- read_csv("Data/CFC GPS Data.csv")

# 2. Recovery Status Data 
recovery <- read_csv("Data/CFC Recovery status Data.csv")

# 3. Physical Capability Data 
phys_cap <- read_csv("Data/CFC Physical Capability Data_.csv")

# 4. Individual Priority Areas 
priority <- read_csv("Data/CFC Individual Priority Areas.csv")


#Part 1 ====
##Data EDA====

## GPS ====

cat("\n========== GPS DATA STRUCTURE ==========\n")
glimpse(gps)

cat("\n========== GPS DATA SUMMARY ==========\n")
summary(gps)

# Check for missing values
cat("\n========== GPS MISSING VALUES ==========\n")
colSums(is.na(gps))

# Identify key GPS metrics for analysis
cat("\n========== GPS COLUMN NAMES ==========\n")
names(gps)

gps <- gps %>% 
  mutate(
    date = dmy(date),
    is_match = (md_minus_code == 0))
    
  
match_days <- gps %>% filter(is_match)
training_days <- gps %>% filter(!is_match)


summary(match_days)

# Basic counts
cat("Total observations:", nrow(gps), "\n")
cat("Match days:", nrow(match_days), "\n")
cat("Date range:", as.character(min(gps$date)), "to", as.character(max(gps$date)), "\n\n")

# Summary statistics for matches
summary(match_days %>% select(distance:peak_speed))

# Check for missing values\
colSums(is.na(match_days))



## Recovery ====

cat("\n========== RECOVERY DATA STRUCTURE ==========\n")
glimpse(recovery)

cat("\n========== RECOVERY DATA SUMMARY ==========\n")
summary(recovery)

# Check unique metrics in recovery data
cat("\n========== UNIQUE RECOVERY METRICS ==========\n")
recovery %>% 
  count(metric) %>% 
  print(n = 50)




# EXTRACT EMBOSS SCORE
cat("\n========== EMBOSS BASELINE SCORE ANALYSIS ==========\n")

emboss_data <- recovery %>% 
  filter(metric == "emboss_baseline_score") %>% 
  mutate(
    sessionDate = dmy(sessionDate),
    value_num = as.numeric(value)
  )

#missing data for emboss
emboss_status <- emboss_data %>% 
  mutate(
    status = case_when(
      is.na(value_num) ~ "Missing (NA)",
      value_num == 0 ~ "Zero (baseline)",
      value_num > 0 ~ "Positive (above baseline)",
      value_num < 0 ~ "Negative (below baseline)",
      TRUE ~ "Other"
    )
  ) %>% 
  count(status) %>% 
  mutate(percentage = round(n / sum(n) * 100, 1))


valid_emboss <- emboss_data %>% filter(!is.na(value_num))



##Visulisations ====

# 1. Match GPS over time
p1 <- ggplot(match_days, aes(x = date, y = distance)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Match-Day Total Distance Over Time",
       x = "Date", y = "Distance (m)")
print(p1)



# 2. GPS distributions
p2 <- match_days %>% 
  select(distance, distance_over_21, distance_over_24, distance_over_27, 
         accel_decel_over_2_5, accel_decel_over_3_5, accel_decel_over_4_5,
         day_duration, peak_speed) %>%
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "GPS Metrics Distribution",
       x = "Value", y = "Count")

print(p2)

# Heart rate zones distribution
p2_hr <- match_days %>% 
  select(hr_zone_1_hms, hr_zone_2_hms, hr_zone_3_hms, hr_zone_4_hms, hr_zone_5_hms) %>%
  mutate(across(everything(), ~as.numeric(.) / 60)) %>%  # Convert to minutes
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Heart Rate Zone Distribution (Match Days)",
       x = "Time (minutes)", y = "Count")
print(p2_hr)


# 3. EMBOSS over time
p3 <- ggplot(valid_emboss, aes(x = sessionDate, y = value_num)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "EMBOSS Recovery Score Over Time",
       x = "Date", y = "EMBOSS Score")

print(p3)

# 4. EMBOSS distribution
p4 <- ggplot(valid_emboss, aes(x = value_num)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "EMBOSS Score Distribution",
       x = "EMBOSS Score", y = "Count")

print(p4)













##PArt 2 ====

######## i have now changed this to work with gps_emboss maybe that will be better? _ try out - i havent done that yet

## Redefine Intolerance - SEVERE THRESHOLD ====
cat("\n========== DEFINING SEVERE INTOLERANCE THRESHOLD ==========\n")

# Compare different threshold options
threshold_options <- tibble(
  method = c("Below Baseline (0)", 
             "10th Percentile", 
             "25th Percentile",
             "Mean - 1 SD"),
  threshold_value = c(
    0,
    quantile(valid_emboss$value_num, 0.10),
    quantile(valid_emboss$value_num, 0.25),
    mean(valid_emboss$value_num) - sd(valid_emboss$value_num)
  )
)

print(threshold_options)

# Choose 25th percentile as SEVERE intolerance
# (Bottom quartile = worst recovery days)
threshold_severe <- quantile(valid_emboss$value_num, 0.25)

cat("\nSelected Threshold (25th percentile):", round(threshold_severe, 4), "\n")
cat("This captures the bottom 25% of recovery days\n\n")

# Recreate intolerance events with severe threshold
emboss_events_severe <- valid_emboss %>% 
  mutate(
    intolerance_event = ifelse(value_num <= threshold_severe, 1, 0)
  )

# Summary
n_events_severe <- sum(emboss_events_severe$intolerance_event)
event_rate_severe <- mean(emboss_events_severe$intolerance_event) * 100

cat("Severe Intolerance Events:", n_events_severe, "\n")
cat("Event rate:", round(event_rate_severe, 1), "%\n\n")

emboss_events_severe %>% 
  group_by(intolerance_event) %>% 
  summarise(
    n = n(),
    mean_score = round(mean(value_num), 4),
    sd_score = round(sd(value_num), 4),
    min_score = round(min(value_num), 4),
    max_score = round(max(value_num), 4)
  ) %>% 
  print()

# Visualize new threshold
p_severe <- ggplot(emboss_events_severe, aes(x = value_num, fill = factor(intolerance_event))) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white") +
  geom_vline(xintercept = threshold_severe, linetype = "dashed", 
             color = "red", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dotted", 
             color = "black", size = 0.8) +
  annotate("text", x = threshold_severe - 0.05, y = Inf, 
           label = "Severe Intolerance", 
           vjust = 2, hjust = 1, color = "red", fontface = "bold") +
  annotate("text", x = 0.02, y = Inf, 
           label = "Baseline", 
           vjust = 1, hjust = 0, color = "black", size = 3) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Normal Recovery", "Severe Intolerance"),
    name = "Status"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "Severe Training Load Intolerance Definition",
    subtitle = paste0("Threshold = ", round(threshold_severe, 3), 
                      " (25th percentile) | N = ", nrow(emboss_events_severe), 
                      " | Events = ", n_events_severe, " (", round(event_rate_severe, 1), "%)"),
    x = "EMBOSS Recovery Score",
    y = "Frequency",
    caption = "Severe intolerance = bottom 25% of recovery days"
  )

print(p_severe)


## Recreate Survival Dataset with Severe Threshold ====

cat("\n========== RECREATING SURVIVAL DATASET (SEVERE INTOLERANCE) ==========\n")

# Prepare EMBOSS data with severe threshold
emboss_for_survival_severe <- emboss_events_severe %>% 
  select(sessionDate, value_num, intolerance_event) %>% 
  rename(date = sessionDate)




###error occuring here now 

# Use same function, just with new emboss data
cat("Processing", nrow(match_days), "matches with severe intolerance threshold...\n")

survival_metrics_severe <- map_df(1:nrow(match_days), function(i) {
  match_date <- match_days$date[i]
  metrics <- calculate_survival_metrics(match_date, emboss_for_survival_severe, followup_days)
  tibble(
    match_id = i,
    time = metrics$time,
    status = metrics$status,
    n_followup_days = metrics$n_followup
  )
})

# Combine with GPS data # i want to include hr zones here ?
survival_data_severe <- match_days %>% 
  mutate(match_id = row_number()) %>% 
  left_join(survival_metrics_severe, by = "match_id") %>% 
  select(match_id, date, opposition_full, season,
         distance, distance_over_21, distance_over_24, distance_over_27,
         accel_decel_over_2_5, accel_decel_over_3_5, accel_decel_over_4_5,
         day_duration, peak_speed,
         time, status, n_followup_days)

# Clean dataset
survival_data_severe_clean <- survival_data_severe %>% 
  filter(!is.na(time)) %>% 
  mutate(
    time = pmax(time, 1),
    time = pmin(time, followup_days)
  )

# Summary statistics
cat("\n========== SEVERE INTOLERANCE SURVIVAL DATASET ==========\n")
cat("Total matches:", nrow(match_days), "\n")
cat("Matches with follow-up:", nrow(survival_data_severe_clean), "\n")
cat("Matches excluded:", nrow(match_days) - nrow(survival_data_severe_clean), "\n\n")

cat("Event Summary:\n")
survival_data_severe_clean %>% 
  count(status) %>% 
  mutate(
    event_type = ifelse(status == 1, "Severe Intolerance", "Normal Recovery (Censored)"),
    percentage = round(n / sum(n) * 100, 1)
  ) %>% 
  select(event_type, n, percentage) %>% 
  print()

cat("\nTime-to-Event Summary:\n")
summary(survival_data_severe_clean$time)

# Visualise
p_survival_severe <- survival_data_severe_clean %>% 
  ggplot(aes(x = time, fill = factor(status))) +
  geom_histogram(bins = 14, alpha = 0.8, color = "white", position = "stack") +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Normal Recovery", "Severe Intolerance"),
    name = "Status"
  ) +
  scale_x_continuous(breaks = seq(1, 14, by = 1)) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "Time to Severe Training Load Intolerance Post-Match",
    subtitle = paste0("N = ", nrow(survival_data_severe_clean), " matches"),
    x = "Days Post-Match",
    y = "Number of Matches",
    caption = paste0("Events: ", sum(survival_data_severe_clean$status), 
                     " | Censored: ", sum(survival_data_severe_clean$status == 0))
  )

print(p_survival_severe)

# Export
write_csv(survival_data_severe_clean, "outputs/survival_dataset_severe.csv")
cat("\n✓ Severe intolerance survival dataset saved\n")

# Show examples
cat("\n========== EXAMPLE MATCHES ==========\n")
survival_data_severe_clean %>% 
  arrange(desc(status), time) %>% 
  select(date, opposition_full, distance, distance_over_24, 
         time, status, n_followup_days) %>% 
  head(15) %>% 
  print()


















## PHASE 3: SURVIVAL ANALYSIS ====
## Purpose: Cox models to identify GPS predictors of severe intolerance
#i want to make sure i am using all gps preicotrs here - like not just a selection of a few - i want to try all variations possisible

library(survival)
library(survminer)

cat("\n========== PHASE 3: SURVIVAL ANALYSIS ==========\n")

# Load survival dataset
surv_data <- survival_data_severe_clean

# A. KAPLAN-MEIER CURVES ====
cat("\n========== A. KAPLAN-MEIER SURVIVAL CURVES ==========\n")

# Overall survival curve (no stratification)
km_overall <- survfit(Surv(time, status) ~ 1, data = surv_data)

# Print summary
print(summary(km_overall))

# Plot with ggsurvplot (taught in class)
p_km_overall <- ggsurvplot(
  km_overall,
  data = surv_data,
  conf.int = TRUE,
  risk.table = TRUE,
  title = "Overall Survival: Time to Severe Intolerance Post-Match",
  xlab = "Days Post-Match",
  ylab = "Probability of Normal Recovery",
  legend = "none"
)
print(p_km_overall)

# Stratify by GPS intensity categories
# Create categorical variables for key GPS metrics
surv_data <- surv_data %>% 
  mutate(
    distance_cat = cut(distance, 
                       breaks = quantile(distance, c(0, 0.33, 0.67, 1)),
                       labels = c("Low", "Medium", "High"),
                       include.lowest = TRUE),
    hsr_cat = cut(distance_over_24,
                  breaks = quantile(distance_over_24, c(0, 0.33, 0.67, 1)),
                  labels = c("Low HSR", "Medium HSR", "High HSR"),
                  include.lowest = TRUE),
    accel_cat = cut(accel_decel_over_3_5,
                    breaks = quantile(accel_decel_over_3_5, c(0, 0.33, 0.67, 1)),
                    labels = c("Low Accel", "Medium Accel", "High Accel"),
                    include.lowest = TRUE)
  )

# KM curves by total distance
km_distance <- survfit(Surv(time, status) ~ distance_cat, data = surv_data)

p_km_distance <- ggsurvplot(
  km_distance,
  data = surv_data,
  conf.int = TRUE,
  pval = TRUE,  # Show log-rank test p-value
  risk.table = TRUE,
  title = "Survival by Total Distance Category",
  xlab = "Days Post-Match",
  ylab = "Probability of Normal Recovery",
  legend.title = "Distance",
  legend.labs = c("Low", "Medium", "High")
)
print(p_km_distance)

# KM curves by HSR
km_hsr <- survfit(Surv(time, status) ~ hsr_cat, data = surv_data)

p_km_hsr <- ggsurvplot(
  km_hsr,
  data = surv_data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  title = "Survival by High-Speed Running Category",
  xlab = "Days Post-Match",
  ylab = "Probability of Normal Recovery",
  legend.title = "HSR Distance",
  legend.labs = c("Low HSR", "Medium HSR", "High HSR")
)
print(p_km_hsr)

# Log-rank test for distance categories
cat("\n========== LOG-RANK TEST: Distance Categories ==========\n")
survdiff_distance <- survdiff(Surv(time, status) ~ distance_cat, data = surv_data)
print(survdiff_distance)

cat("\n========== LOG-RANK TEST: HSR Categories ==========\n")
survdiff_hsr <- survdiff(Surv(time, status) ~ hsr_cat, data = surv_data)
print(survdiff_hsr)

# B. COX PROPORTIONAL HAZARDS MODELS ====
cat("\n========== B. COX PROPORTIONAL HAZARDS MODELS ==========\n")

# Model 1: Total distance only
cox_model1 <- coxph(Surv(time, status) ~ distance, data = surv_data)
cat("\n--- Model 1: Total Distance ---\n")
summary(cox_model1)

# Model 2: HSR metrics
cox_model2 <- coxph(Surv(time, status) ~ distance_over_21 + distance_over_24 + 
                      distance_over_27, data = surv_data)
cat("\n--- Model 2: HSR Metrics ---\n")
summary(cox_model2)

# Model 3: Acceleration/deceleration
cox_model3 <- coxph(Surv(time, status) ~ accel_decel_over_2_5 + 
                      accel_decel_over_3_5 + accel_decel_over_4_5, 
                    data = surv_data)
cat("\n--- Model 3: Acceleration Metrics ---\n")
summary(cox_model3)

# Model 4: Combined key metrics (final model)
# Scale variables to avoid convergence issues
surv_data_scaled <- surv_data %>% 
  mutate(
    distance_scaled = scale(distance)[,1],
    hsr_scaled = scale(distance_over_24)[,1],
    accel_scaled = scale(accel_decel_over_3_5)[,1]
  )

cox_final <- coxph(Surv(time, status) ~ distance_scaled + hsr_scaled + accel_scaled, 
                   data = surv_data_scaled)
cat("\n--- Final Model: Combined GPS Metrics (Scaled) ---\n")
summary(cox_final)

# Compare models with AIC
cat("\n========== MODEL COMPARISON (AIC) ==========\n")
aic_comparison <- tibble(
  Model = c("Distance only", "HSR metrics", "Accel metrics", "Combined"),
  AIC = c(AIC(cox_model1), AIC(cox_model2), AIC(cox_model3), AIC(cox_final))
) %>% 
  arrange(AIC)
print(aic_comparison)

# Visualize hazard ratios (forest plot)
p_forest <- ggforest(cox_final, data = surv_data_scaled)
print(p_forest)

# C. MODEL DIAGNOSTICS ====
cat("\n========== C. PROPORTIONAL HAZARDS ASSUMPTION TEST ==========\n")

# Test PH assumption using cox.zph (Schoenfeld residuals)
ph_test_final <- cox.zph(cox_final)
print(ph_test_final)

# Plot Schoenfeld residuals
par(mfrow = c(2, 2))
plot(ph_test_final)
par(mfrow = c(1, 1))

# If PH assumption violated, consider stratification
# Example: Stratify by a problematic variable
if(any(ph_test_final$table[,"p"] < 0.05)) {
  cat("\n⚠ Proportional hazards assumption violated for some variables\n")
  cat("Consider stratified model if needed\n\n")
  
  # Example stratified model (if needed)
  # cox_stratified <- coxph(Surv(time, status) ~ distance_scaled + hsr_scaled + 
  #                          strata(accel_cat), data = surv_data_scaled)
} else {
  cat("\n✓ Proportional hazards assumption satisfied\n\n")
}

# D. PREDICTED SURVIVAL CURVES ====
cat("\n========== D. ADJUSTED SURVIVAL CURVES ==========\n")

# Create prediction data for different GPS intensities
newdata_predict <- tibble(
  distance_scaled = c(-1, 0, 1),  # Low, Medium, High
  hsr_scaled = c(-1, 0, 1),
  accel_scaled = c(-1, 0, 1),
  intensity = c("Low Intensity", "Medium Intensity", "High Intensity")
)

# Predict survival curves
fit_predict <- survfit(cox_final, newdata = newdata_predict)

# Plot adjusted curves
p_adjusted <- ggsurvplot(
  fit_predict,
  data = surv_data_scaled,
  conf.int = TRUE,
  title = "Predicted Survival by Match Intensity",
  xlab = "Days Post-Match",
  ylab = "Probability of Normal Recovery",
  legend.title = "Match Intensity",
  legend.labs = newdata_predict$intensity
)
print(p_adjusted)

# E. SUMMARY TABLE ====
cat("\n========== FINAL RESULTS SUMMARY ==========\n")

# Extract hazard ratios from final model
hr_summary <- broom::tidy(cox_final, exponentiate = TRUE, conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high, p.value) %>% 
  mutate(
    term = case_when(
      term == "distance_scaled" ~ "Total Distance (per SD)",
      term == "hsr_scaled" ~ "High-Speed Running (per SD)",
      term == "accel_scaled" ~ "Accelerations (per SD)",
      TRUE ~ term
    ),
    HR = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
    p_value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>% 
  select(term, HR, p_value)

print(hr_summary)
