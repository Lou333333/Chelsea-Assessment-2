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

#