# Add this RIGHT AFTER your existing Phase 2 (after you've defined match_days)

## EXPLORE Physical Capability ====
cat("\n========== PHYSICAL CAPABILITY EXPLORATION ==========\n")

# Load and check structure
glimpse(phys_cap)

# Clean dates
phys_cap <- phys_cap %>%
  mutate(
    date = dmy(testDate),
    movement = tolower(movement)
  ) %>%
  filter(!is.na(date), !is.na(benchmarkPct))

# What movements do we have?
phys_cap %>% 
  count(movement, sort = TRUE) %>%
  print()

# Distribution by movement
phys_cap %>%
  group_by(movement) %>%
  summarise(
    n = n(),
    mean = mean(benchmarkPct, na.rm = TRUE),
    median = median(benchmarkPct, na.rm = TRUE),
    min = min(benchmarkPct, na.rm = TRUE),
    max = max(benchmarkPct, na.rm = TRUE)
  ) %>%
  print()



## Match Capability to GPS Dates ====
cat("\n========== MATCHING CAPABILITIES TO MATCHES ==========\n")

# For each match, find the CLOSEST capability measurement
match_with_cap <- match_days %>%
  select(date, distance, distance_over_24, distance_over_27, accel_decel_over_3_5) %>%
  # Cross join to find closest dates
  left_join(
    phys_cap %>%
      filter(movement %in% c("jump", "sprint", "agility")) %>%
      group_by(date, movement) %>%
      summarise(cap_score = mean(benchmarkPct), .groups = "drop"),
    by = character(),
    relationship = "many-to-many"
  ) %>%
  # Keep only caps within 3 days of match
  mutate(days_diff = as.numeric(abs(date.x - date.y))) %>%
  filter(days_diff <= 3) %>%
  # Keep closest
  group_by(date.x, movement) %>%
  slice_min(days_diff, n = 1) %>%
  ungroup() %>%
  # Reshape wide
  select(match_date = date.x, movement, cap_score, distance, distance_over_24) %>%
  pivot_wider(names_from = movement, values_from = cap_score, names_prefix = "cap_")

# Check results
cat("Matches with capability data:", nrow(match_with_cap), "\n")
print(head(match_with_cap, 10))

# Summary
summary(match_with_cap %>% select(starts_with("cap_")))






## Add Capabilities to Survival Data ====
cat("\n========== ADDING CAPABILITIES TO SURVIVAL DATASET ==========\n")

# Join to your existing survival_data_severe_clean
survival_with_cap <- survival_data_severe_clean %>%
  left_join(
    match_with_cap %>% select(match_date, cap_jump, cap_sprint, cap_agility),
    by = c("date" = "match_date")
  )

# Check how many matches have capability data
cat("Matches in survival data:", nrow(survival_data_severe_clean), "\n")
cat("Matches with capabilities:", sum(!is.na(survival_with_cap$cap_jump)), "\n\n")

# Categorize capabilities (tertiles: Low/Moderate/High)
survival_with_cap <- survival_with_cap %>%
  mutate(
    jump_cat = cut(cap_jump, 
                   breaks = quantile(cap_jump, c(0, 0.33, 0.67, 1), na.rm = TRUE),
                   labels = c("Low", "Moderate", "High"),
                   include.lowest = TRUE),
    
    # Composite capability score
    cap_composite = (cap_jump + cap_sprint + cap_agility) / 3,
    
    cap_composite_cat = cut(cap_composite,
                            breaks = quantile(cap_composite, c(0, 0.33, 0.67, 1), na.rm = TRUE),
                            labels = c("Low", "Moderate", "High"),
                            include.lowest = TRUE)
  )

# Summary
cat("Capability summary:\n")
summary(survival_with_cap %>% select(cap_jump, cap_sprint, cap_agility, cap_composite))

cat("\nCapability categories:\n")
table(survival_with_cap$cap_composite_cat, useNA = "ifany")




## Test Capability as Cox Predictor ====
cat("\n========== COX MODEL WITH CAPABILITIES ==========\n")

# Model 1: GPS only (your current best)
cox_gps_only <- coxph(Surv(time, status) ~ distance_over_27, 
                      data = survival_with_cap)

# Model 2: Capability only
cox_cap_only <- coxph(Surv(time, status) ~ cap_composite,
                      data = survival_with_cap)

# Model 3: GPS + Capability
cox_combined <- coxph(Surv(time, status) ~ distance_over_27 + cap_composite,
                      data = survival_with_cap)

# Model 4: GPS * Capability INTERACTION
cox_interaction <- coxph(Surv(time, status) ~ distance_over_27 * cap_composite,
                         data = survival_with_cap)

# Compare
cat("\nModel Comparison:\n")
tibble(
  Model = c("GPS only", "Capability only", "GPS + Capability", "GPS * Capability"),
  AIC = c(AIC(cox_gps_only), AIC(cox_cap_only), AIC(cox_combined), AIC(cox_interaction))
) %>%
  arrange(AIC) %>%
  print()

# Show best model
cat("\nBest model summary:\n")
summary(cox_interaction)





## Visualize Capability Effect ====
cat("\n========== KAPLAN-MEIER BY CAPABILITY ==========\n")

# KM curve by capability category
km_cap <- survfit(Surv(time, status) ~ cap_composite_cat, 
                  data = survival_with_cap)

p_km_cap <- ggsurvplot(
  km_cap,
  data = survival_with_cap,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.height = 0.25,
  title = "Intolerance Survival by Physical Capability",
  xlab = "Days Post-Match",
  ylab = "Probability of Normal Recovery",
  legend.title = "Capability",
  legend.labs = c("Low", "Moderate", "High"),
  palette = c("#FC4E07", "#E7B800", "#00AFBB")
)

print(p_km_cap)

# Show final model with capability
cat("\n========== FINAL MODEL: GPS + CAPABILITY ==========\n")
summary(cox_combined)




## Forecast Capability Recovery ====
cat("\n========== FORECASTING CAPABILITY TRAJECTORIES ==========\n")

# Create time series of capability scores
cap_ts <- phys_cap %>%
  filter(movement == "jump") %>%  # Use jump as primary metric
  group_by(date) %>%
  summarise(cap_value = mean(benchmarkPct)) %>%
  as_tsibble(index = date)

# Fill gaps
cap_ts <- cap_ts %>%
  fill_gaps() %>%
  fill(cap_value, .direction = "down")

# Train/test split
n_train <- floor(nrow(cap_ts) * 0.8)
train <- cap_ts %>% slice(1:n_train)
test <- cap_ts %>% slice((n_train+1):nrow(cap_ts))

# Fit models
models <- train %>%
  model(
    naive = NAIVE(cap_value),
    arima = ARIMA(cap_value),
    ets = ETS(cap_value)
  )

# Forecast
fc <- models %>% forecast(h = nrow(test))

# Accuracy
acc <- fc %>% accuracy(test)
print(acc %>% select(.model, RMSE, MAE))

# Visualize
fc %>% autoplot(cap_ts, level = 95)



#summarise 
(phys_cap$date)


