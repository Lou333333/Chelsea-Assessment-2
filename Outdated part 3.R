#OLD PART 3 




# Part 3 ====
##Survival Analysis ==== 
##### am i getting rid of days before and after by simplfying its 1 and 0s? # i am - oi need to have minus and + days etc



##########################################
## this one needs work - gets lost in the prior and post days i believe # 

############################################
intolerance_events <- read_csv("outputs/intolerance_events.csv", show_col_types = FALSE)


# Create survival dataset directly ###### ERRROR HERE # 
# Create survival dataset directly
survival_data <- match_days %>%
  rowwise() %>%
  mutate(
    events_in_window = list(
      intolerance_events %>%
        filter(sessionDate >= date - 10,   # include up to 14 days BEFORE
               sessionDate <= date + 10,   # include up to 14 days AFTER
               intolerance_event == 1)
    ),
    time = if_else(
      nrow(events_in_window) > 0,
      as.numeric(min(events_in_window$sessionDate - date)),  # can be negative or positive
      NA_real_   # no event -> missing here, handle later
    ),
    status = if_else(nrow(events_in_window) > 0, 1, 0)
  ) %>%
  ungroup() %>%
  select(-events_in_window) %>%
  mutate(
    # if no event, censor at ±14 (whichever side you want to define)
    time = if_else(is.na(time), 10, time)
  )



cat("Total matches:", nrow(survival_data), "\n")
cat("Events (intolerance within 14 days):", sum(survival_data$status), "\n")
cat("Censored (no intolerance):", sum(survival_data$status == 0), "\n\n")


#add gps
survival_data <- survival_data %>% 
  mutate(
    # Create intensity categories using tertiles
    intensity_category = case_when(
      distance <= quantile(distance, 0.33, na.rm = TRUE) ~ "Low",
      distance <= quantile(distance, 0.67, na.rm = TRUE) ~ "Moderate",
      TRUE ~ "High"
    ),
    intensity_category = factor(intensity_category, 
                                levels = c("Low", "Moderate", "High"))
  )

cat("Intensity categories created\n\n")

# Check distribution
survival_data %>% 
  count(intensity_category, status) %>% 
  pivot_wider(names_from = status, values_from = n, 
              names_prefix = "status_") %>% 
  print()

#Surivival summary 
summary(survival_data$time)

cat("\nGPS predictors (matches only):\n")
survival_data %>% 
  select(distance, distance_over_21, distance_over_27, accel_decel_over_3_5) %>% 
  summary() %>% 
  print()

# Event rate by intensity
cat("\nEvent rate by intensity category:\n")
survival_data %>% 
  group_by(intensity_category) %>% 
  summarise(
    n_matches = n(),
    n_events = sum(status),
    event_rate = round(mean(status) * 100, 1)
  ) %>% 
  print()


#Data qua;ity checks

# Check for matches without follow-up data
cat("Matches with missing GPS data:", sum(is.na(survival_data$distance)), "\n")

# Check time range
cat("Time to event range:", min(survival_data$time), "to", 
    max(survival_data$time), "days\n")

# Check for ties in survival times
cat("Ties in event times:", 
    sum(duplicated(survival_data$time[survival_data$status == 1])), "\n\n")

#export survival dataswert

# Clean dataset for analysis. 

# i feel like I need all fo them in here not just a few
survival_export <- survival_data %>% 
  select(
    date,
    season,
    opposition_full,
    time,
    status,
    distance,
    distance_over_21,
    distance_over_27,
    accel_decel_over_3_5,
    peak_speed,
    intensity_category
  )

write_csv(survival_export, "outputs/survival_dataset.csv")

cat("Saved: outputs/survival_dataset.csv\n\n")

#visualisation # this needs to be just post match rather than pre match i think ?


p_survival <- survival_data %>% 
  ggplot(aes(x = distance, y = time, color = factor(status))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Censored", "Intolerance"),
    name = "Outcome"
  ) +
  theme_minimal() +
  labs(
    title = "Match Distance vs Time to Intolerance",
    subtitle = paste0("N = ", nrow(survival_data), " matches | ", 
                      sum(survival_data$status), " events"),
    x = "Match Distance (m)",
    y = "Days to Intolerance (or censoring)"
  )

print(p_survival)

#more plots 

p_survival2 <- survival_data %>% 
  ggplot(aes(x = peak_speed, y = time, color = factor(status))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Censored", "Intolerance"),
    name = "Outcome"
  ) +
  theme_minimal() +
  labs(
    title = "Match Peak Speed vs Time to Intolerance",
    subtitle = paste0("N = ", nrow(survival_data), " matches | ", 
                      sum(survival_data$status), " events"),
    x = "Match Peak Speed (m/s)",
    y = "Days to Intolerance (or censoring)"
  )

print(p_survival2)

# 5 more plots 

p_survival3 <- survival_data %>% 
  ggplot(aes(x = accel_decel_over_3_5, y = time, color = factor(status))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Censored", "Intolerance"),
    name = "Outcome"
  ) +
  theme_minimal() +
  labs(
    title = "Match Accel/Decel >3.5 m/s² vs Time to Intolerance",
    subtitle = paste0("N = ", nrow(survival_data), " matches | ", 
                      sum(survival_data$status), " events"),
    x = "Accel/Decel >3.5 m/s² (count)",
    y = "Days to Intolerance (or censoring)"
  )

print(p_survival3)

#more plots
p_survival4 <- survival_data %>% 
  ggplot(aes(x = distance_over_21, y = time, color = factor(status))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Censored", "Intolerance"),
    name = "Outcome"
  ) +
  theme_minimal() +
  labs(
    title = "Match Distance >21 km/h vs Time to Intolerance",
    subtitle = paste0("N = ", nrow(survival_data), " matches | ", 
                      sum(survival_data$status), " events"),
    x = "Distance >21 km/h (m)",
    y = "Days to Intolerance (or censoring)"
  )
print(p_survival4)









#Part 4 ====
##survival CPH analysis ====
survival_data <- read_csv("outputs/survival_dataset.csv", show_col_types = FALSE)


cat("Dataset:", nrow(survival_data), "matches\n")
cat("Events:", sum(survival_data$status), "\n")
cat("Censored:", sum(survival_data$status == 0), "\n\n")



##KAplan Meir ====

# Fit KM model stratified by intensity
km_fit <- survfit(Surv(time, status) ~ intensity_category, 
                  data = survival_data)

print(summary(km_fit))

# Log-rank test
log_rank <- survdiff(Surv(time, status) ~ intensity_category, 
                     data = survival_data)

cat("\nLog-rank test:\n")
print(log_rank)

# Create KM plot
p_km <- ggsurvplot(
  km_fit,
  data = survival_data,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.height = 0.3,
  legend.title = "Match Intensity",
  legend.labs = c("Low", "Moderate", "High"),
  palette = c("steelblue", "orange", "coral"),
  xlab = "Days Post-Match",
  ylab = "Probability of No Intolerance",
  title = "Kaplan-Meier Survival Curves",
  subtitle = "Time to training load intolerance by match intensity"
)

print(p_km)

####high vs mdoeate is coonfusing here - look into why these are the results need all markers in it ?
