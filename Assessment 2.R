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

print(emboss_status)
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



#Part 2 ====

## Defining Intolernace ====
# Purpose: Define what constitutes a "training load intolerance" event
# Outcome: Binary event definition for survival analysis

##should it be a 25 quan tile or should it be more like a 1std or 2 std o 3 std etc?


#threshold cacluaiton - 25 quantile
threshold_25 <- quantile(valid_emboss$value_num, 0.25)
cat("25th percentile threshold:", round(threshold_25, 4), "\n") #=. -0.0582 # the cutoof for intolerance poor perfromance etc

#intolerance indicator 
emboss_events <- valid_emboss %>% 
  mutate(
    intolerance_event = ifelse(value_num <= threshold_25, 1, 0)
  )

# Count events
n_events <- sum(emboss_events$intolerance_event)
event_rate <- mean(emboss_events$intolerance_event) * 100

cat("Total intolerance events:", n_events, "\n")
cat("Event rate:", round(event_rate, 1), "%\n\n")

#Summarise 

emboss_events %>% 
  group_by(intolerance_event) %>% 
  summarise(
    n = n(),
    mean_score = round(mean(value_num), 4),
    sd_score = round(sd(value_num), 4),
    min_score = round(min(value_num), 4),
    max_score = round(max(value_num), 4)
  ) %>% 
  print()


##visualisaton of intolerance ====


p_intolerance <- ggplot(emboss_events, aes(x = value_num, fill = factor(intolerance_event))) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white") +
  geom_vline(xintercept = threshold_25, linetype = "dashed", 
             color = "orange", size = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "red", size = 0.8) +
  scale_fill_manual(
    values = c("0" = "steelblue", "1" = "coral"),
    labels = c("Tolerant", "Intolerant"),
    name = "Status"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "Definition of Training Load Intolerance Event",
    subtitle = paste0("N = ", nrow(emboss_events), " | Events = ", 
                      n_events, " (", round(event_rate, 1), "%)"),
    x = "EMBOSS Recovery Score",
    y = "Frequency",
    caption = "Orange line = intolerance threshold (25th percentile)"
  )

print(p_intolerance)


#export as intolerance days csv 

intolerance_export <- emboss_events %>% 
  select(sessionDate, value_num, intolerance_event) %>% 
  rename(emboss_score = value_num)


dir.create("outputs", showWarnings = FALSE)
write_csv(intolerance_export, "outputs/intolerance_events.csv")




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
