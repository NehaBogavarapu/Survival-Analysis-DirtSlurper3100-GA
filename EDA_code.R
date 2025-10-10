# Load packages 
library(survival) 
library(survminer) 
library(VIM)
library(tidyverse)
library(lubridate)
# Set path to CSV data file 

file_path <- "D:\\TUE Study Material\\Q1\\Survival Analysis for Data Scientists\\GA_17\\Survival-Analysis-DirtSlurper3100-GA\\DirtSlurper3100.csv"


# Read data
og_data <- read.table(file_path,
                      header = TRUE,
                      sep = ",",
                      skip = 12,   
                      stringsAsFactors = FALSE)

# ---- Basic inspection ----
str(og_data)
summary(og_data)
View(og_data)


# ---- Data cleaning / variable formatting ----


# Identify invalid/unclear records as candidates of censoring (Sent.to.repair=0)

# No failure date but damaged component:
damage_but_no_failure_date_idx  <- which(og_data$Sent.for.repair == "YES" & og_data$Failure.date == "---")

# All parts okay but there is a failure date (failure due to unobserved component):
ok_parts_but_failure_idx <- which(
  og_data$Battery.status == "OK" &
    og_data$Impact.status == "OK" &
    og_data$IR.status == "OK" &
    og_data$Failure.date != "---"
)

# Convert the indices to a single vector of rows to drop
#rows_to_drop <- union(ok_parts_but_failure_idx, damage_but_no_failure_date_idx)

# POSIX standard English locale 
Sys.setlocale("LC_TIME", "C")  
# (had to do this because dates with "Sep" were not being identified)

data_eda <- og_data %>%
  slice(-rows_to_drop) %>% # Drop the rows_to_drop
  mutate(
    # Format dates 
    Registration.date = as.Date(Registration.date, format = "%d%b%Y"),
    Failure.date = as.Date(Failure.date, format = "%d%b%Y"),
    
    # Replace empty date fields with the last date of the study
    Failure.date = if_else(is.na(Failure.date), as.Date("2019-12-31"),
                           Failure.date),
    
    # Repair event indicator (1 = repair, 0 = no/censored)
    Sent.for.repair = as.numeric(`Sent.for.repair` == "YES"),
    
    # Pets indicator (1 = pets, 0 = none)
    Pets = as.numeric(Pets == "YES"),
    
    # Formatting numerical values
    Carpet.score = as.numeric(Carpet.score),
    Total.usage.time = as.numeric(Total.usage.time),
    
    # Parts statuses (0 = OK or 1 = Damage)
    Battery.status = as.numeric(Battery.status %in% c("Damage")),
    IR.status = as.numeric(IR.status %in% c("Damage")),
    Impact.status = as.numeric(Impact.status %in% c("Damage")),
    
    # New variable: Possession time in hours
    Possession.time = as.numeric(difftime(Failure.date, Registration.date, units = "hours"))

    ) %>%
  mutate(
    Pets = factor(Pets, levels = c(0, 1), labels = c("No", "Yes")),
    Sent.for.repair = factor(Sent.for.repair, levels = c(0, 1), labels = c("No", "Yes")),
    Battery.status = factor(Battery.status, levels = c(0, 1), labels = c("OK", "Damage")),
    IR.status = factor(IR.status, levels = c(0, 1), labels = c("OK", "Damage")),
    Impact.status = factor(Impact.status, levels = c(0, 1), labels = c("OK", "Damage")),
    Carpet.score = factor(Carpet.score, levels = sort(unique(Carpet.score))),
    reg_year = factor(lubridate::year(Registration.date))
  )

View(data_eda)

# ---- Visualizations ----

colors <- c("cadetblue", "coral")

# Histogram of Total Usage Time
ggplot(data_eda, aes(x = Total.usage.time, fill = Sent.for.repair)) +
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Total Usage Time",
       x = "Total Usage Time (hours)",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Bar chart for Pets indicator
ggplot(data_eda, aes(x = factor(Pets), fill = Sent.for.repair)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Pets",
       x = "Pets indicator (0 = No, 1 = Yes)",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Bar chart for Carpet Scores
ggplot(data_eda, aes(x = factor(Carpet.score), fill = Sent.for.repair)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Carpet Scores",
       x = "Carpet Score (1-9)",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Bar chart for Sent.for.repair
ggplot(data_eda, aes(x = Sent.for.repair, fill = Sent.for.repair)) +
  geom_bar(color = "black") +
  labs(title = "Repair Event Counts",
       x = "Sent for Repair (0 = No, 1 = Yes)",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Bar chart for Battery Status
ggplot(data_eda, aes(x = Battery.status, fill = Sent.for.repair)) +
  geom_bar(color = "black") +
  labs(title = "Battery Status Counts",
       x = "Battery Status",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Bar chart for Impact Status
ggplot(data_eda, aes(x = Impact.status, fill = Sent.for.repair)) +
  geom_bar(color = "black") +
  labs(title = "Impact Sensor Status Counts",
       x = "Impact Sensor Status",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Bar chart for IR Status
ggplot(data_eda, aes(x = IR.status, fill = Sent.for.repair)) +
  geom_bar(color = "black") +
  labs(title = "IR Sensor Status Counts",
       x = "IR Sensor Status",
       y = "Count") +
  scale_fill_manual(values = colors) +
  theme_minimal()

# Scatter plot: Usage vs Possession Time
ggplot(data_eda, aes(x = Possession.time, y = Total.usage.time, color = Sent.for.repair)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(linetype = as.factor(Sent.for.repair)), method = "lm", se = TRUE, color = "black") +
  scale_linetype_manual(values = c("No" = "dotted", "Yes" = "solid")) +
  labs(title = "Usage vs. Possession Time by Repair Status",
       x = "Possession Time (hours)",
       y = "Total Usage Time (hours)",
       linetype = "Sent for Repair") +
  scale_color_manual(values = colors) +
  theme_minimal()

# ---- Gauri plots ----------------------------------------------------

# Plot a scatter plot Registration date vs Total usage time 
reg <- data_eda$Registration.date
totaltime <- data_eda$Total.usage.time
plot(reg, totaltime,
     main = "Observation of Total Time wrt to Registration Date",
     xlab = "Registration Time", ylab = "Total Time")
# Seemingly right as newer the product it would have worked less but of course there are exceptions as well

# Relation between Usage Time and Presence of PETS
# Mean usage time vs pets
pets <- data_eda$Pets
pets_mean <- tapply(totaltime, pets, mean, na.rm = TRUE)
barplot(pets_mean,
        names.arg = names(pets_mean),
        col = c("cadetblue"),
        main = "Vacuum Cleaner Usage based on Presence of Pets",
        xlab = "Pets",
        ylab = "Average Usage Time (hours)")
# Yes the vacuum works more if there are pets 

# Relation between Carpet Score and Total usage time
carpetscore <- data_eda$Carpet.score
carpet_mean <- tapply(data_eda$Total.usage.time, data_eda$Carpet.score, mean, na.rm = TRUE)
barplot(carpet_mean,
        names.arg = names(carpet_mean),
        col = c("cadetblue"),
        main = "Vacuum Cleaner Usage based on Carpet Area",
        xlab = "Carpet Score",
        ylab = "Average Usage Time (hours)")
# This strangely implies that carpet area is not influencing the working time of the vacuum cleaner

# Relation between Vacuums sent back for repair vs their Total usage time
sent_repair <- data_eda$Sent.for.repair
mean_value <- tapply(totaltime, sent_repair, mean, na.rm = TRUE)
barplot(mean_value,
        names.arg = names(mean_value),
        col = c("cadetblue", "coral"),
        main = "Average usage of Vacuum sent for repairs vs not",
        xlab = "Sent for Repair",
        ylab = "Average Usage Time (hours)")
# This implies that a vacuum that is used intensively requires frequent repairs

# Count the number of Ok/failures in Battery, Impact Sensor, Infrared Sensor (to see which is most vulnerable)
batterystatus <- data_eda$Battery.status
impactsensor_status <- data_eda$Impact.status
infraredsensor_status <- data_eda$IR.status

battery_ok <- sum(batterystatus == "OK", na.rm = TRUE)
impactsensor_ok <- sum(impactsensor_status == "OK", na.rm = TRUE)
infraredsensor_ok <- sum(infraredsensor_status == "OK", na.rm = TRUE)

battery_fail <- sum(batterystatus == "Damage", na.rm = TRUE)
impactsensor_fail <- sum(impactsensor_status == "Damage", na.rm = TRUE)
infraredsensor_fail <- sum(infraredsensor_status == "Damage", na.rm = TRUE)

ok <- c(battery_ok, impactsensor_ok, infraredsensor_ok)
fail <- c(battery_fail, impactsensor_fail, infraredsensor_fail)

barplot(rbind(ok, fail),
        beside = TRUE,
        col = c("cadetblue", "coral"),
        names.arg = c("Battery", "Impact Sensor", "Infrared Sensor"),
        main = "Status of Components",
        xlab = "Components of Vacuum",
        ylab = "Count")
legend("topright",
       legend = c("OK", "Damage"),
       fill = c("cadetblue", "coral"),
       inset = c(-0.07, -0.07), bty = "n")  
# This implies that Battery is the most sensitive followed by Infrared and Impact Sensor

# Relation between Average usage time and Failure of Battery, Impact Sensor, Infrared Sensor
battery_mean <- tapply(totaltime, batterystatus, mean, na.rm = TRUE)
impactsensor_mean <- tapply(totaltime, impactsensor_status, mean, na.rm = TRUE)
infraredsensor_mean <- tapply(totaltime, infraredsensor_status, mean, na.rm = TRUE)

battery_ok <- battery_mean["OK"]
battery_damage <- battery_mean["Damage"]
impact_ok <- impactsensor_mean["OK"]
impact_damage <- impactsensor_mean["Damage"]
ir_ok <- infraredsensor_mean["OK"]
ir_damage <- infraredsensor_mean["Damage"]

ok <- c(battery_ok, impactsensor_ok, infraredsensor_ok)
damage <- c(battery_damage, impact_damage, ir_damage)

barplot(rbind(ok, damage),
        beside = TRUE,
        col = c("cadetblue", "coral"),
        names.arg = c("Battery", "Impact Sensor", "Infrared Sensor"),
        main = "Status of Components",
        xlab = "Components of Vacuum",
        ylab = "Average Usage Time (hours)")
legend("topleft",
       legend = c("OK", "Damage"),
       fill = c("cadetblue", "coral"),
       inset = c(-0.02, -0.02),
       bty = "n")
# Infrared Sensor fails when vacuum used less as opposed to battery and impact where more use corresponds to more failure

# Relation between Pets and Failure of Battery, Impact Status, Infrared Sensor
battery_fail <- tapply(batterystatus == "Damage", pets, sum, na.rm = TRUE)
impactsensor_fail <- tapply(impactsensor_status == "Damage", pets, sum, na.rm = TRUE)
infraredsensor_fail <- tapply(infraredsensor_status == "Damage", pets, sum, na.rm = TRUE)

fail_matrix <- rbind(Battery = battery_fail,
                     Impact  = impactsensor_fail,
                     IR      = infraredsensor_fail)

barplot(fail_matrix,
        beside = TRUE,
        col = c("tomato", "steelblue", "orange"),
        main = "Component Failures by Pets",
        xlab = "Pets (NO / YES)",
        ylab = "Failure Count")
legend("topleft", legend = c("Battery", "Impact Sensor", "Infrared Sensor"),
       fill = c("tomato", "steelblue", "orange"), bty = "n")
# When no pets IR fails; when pets Battery fails

# Relation between Carpet Score and Failure of Battery, Impact Status, Infrared Sensor
battery_fail <- tapply(batterystatus == "Damage" , carpetscore, sum, na.rm = TRUE)
impactsensor_fail <- tapply(impactsensor_status == "Damage", carpetscore, sum, na.rm = TRUE)
infraredsensor_fail <- tapply(infraredsensor_status == "Damage", carpetscore, sum, na.rm = TRUE)

fail_matrix <- rbind(Battery = battery_fail,
                     Impact  = impactsensor_fail,
                     IR      = infraredsensor_fail)

barplot(fail_matrix,
        beside = TRUE,
        col = c("tomato", "steelblue", "orange"),
        main = "Component Failures by Extensive Carpet Area",
        xlab = "Carpet Score",
        ylab = "Failure Count")
legend("topright", legend = c("Battery", "Impact Sensor", "Infrared Sensor"),
       fill = c("tomato", "steelblue", "orange"), bty = "n")
# Failure rate highest when carpet score is 3; Battery followed by IR Sensor, followed by Impact Sensor.
# So carpet area does not determine failure of the vacuum reaffirmed

# ---- Cansu plots ----------------------------------------------------

# # VIM missing data pattern plot COMMENTED OUT BECAUSE PREPROCESSING STEPS NEED TO BE REVERTED FOR THIS (MISSING VALUES FILLED)
# VIM::aggr(data_eda, col = c("navyblue", "red"),
#           numbers = TRUE, sortVars = TRUE,
#           main = "Missing Data Pattern")

# # Heatmap of missing data
# data_eda %>%
#   is.na() %>%
#   as.data.frame() %>%
#   mutate(row = row_number()) %>%
#   pivot_longer(-row, names_to = "variable", values_to = "is_missing") %>%
#   ggplot(aes(variable, row, fill = is_missing)) +
#   geom_tile() +
#   scale_fill_manual(values = c("FALSE" = "lightblue", "TRUE" = "red"),
#                     name = "Missing") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Missing Data Heatmap",
#        x = "Variables", y = "Observations")

# Log-transformed Usage Time 
ggplot(data_eda, aes(x = log(Total.usage.time + 1))) +
  geom_histogram(bins = 50, fill = "cadetblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Log(Usage Time + 1)",
       x = "Log(Total Usage Time + 1)", y = "Frequency") +
  theme_minimal()

# Boxplot: Usage Time by Pet Ownership 
ggplot(data_eda, aes(x = factor(Pets), y = Total.usage.time, fill = factor(Pets))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Usage Time by Pet Ownership",
       x = "Has Pets", y = "Total Usage Time (hours)") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "cadetblue", "Yes" = "coral"))

# Boxplot: Usage Time by Carpet Score 
ggplot(data_eda, aes(x = factor(Carpet.score), y = Total.usage.time)) +
  geom_boxplot(fill = "cadetblue", alpha = 0.7) +
  labs(title = "Usage Time by Carpet Score",
       x = "Carpet Score", y = "Total Usage Time (hours)") +
  theme_minimal()

# Component Failures by Usage Quartile 
# Create usage time quartiles
data_eda$usage_quartile <- cut(data_eda$Total.usage.time,
                           breaks = quantile(data_eda$Total.usage.time, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                           labels = c("Q1", "Q2", "Q3", "Q4"),
                           include.lowest = TRUE)

# Failure rates by usage quartile (summary table)
failure_by_usage <- data_eda %>%
  filter(!is.na(usage_quartile)) %>%
  group_by(usage_quartile) %>%
  summarise(
    n = n(),
    battery_failures = sum(Battery.status == 1, na.rm = TRUE),
    impact_failures = sum(Impact.status == 1, na.rm = TRUE),
    ir_failures = sum(IR.status == 1, na.rm = TRUE),
    battery_rate = round(battery_failures / sum(!is.na(Battery.status)) * 100, 1),
    impact_rate = round(impact_failures / sum(!is.na(Impact.status)) * 100, 1),
    ir_rate = round(ir_failures / sum(!is.na(IR.status)) * 100, 1)
  )

print(failure_by_usage)


ggplot(data_eda, aes(x = factor(reg_year))) +
  geom_bar(fill = "cadetblue", color = "black", alpha = 0.7) +
  labs(title = "Registrations by Year",
       x = "Registration Year", y = "Count") +
  theme_minimal()

# Follow-up Time Distribution 
ggplot(data_eda, aes(x = Possession.time / 24)) +
  geom_histogram(bins = 50, fill = "cadetblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Follow-up Times",
       x = "Follow-up Time (days)", y = "Frequency") +
  theme_minimal()

# Follow-up Time by Registration Year 
ggplot(data_eda, aes(x = factor(reg_year), y = Possession.time / 24)) +
  geom_boxplot(fill = "cadetblue", alpha = 0.7) +
  labs(title = "Follow-up Time by Registration Year",
       x = "Registration Year", y = "Follow-up Time (days)") +
  theme_minimal()

# Summary table with key statistics
summary_table <- data %>%
  summarise(
    n_units = n(),
    usage_mean = round(mean(Total.usage.time, na.rm = TRUE), 1),
    usage_median = round(median(Total.usage.time, na.rm = TRUE), 1),
    usage_sd = round(sd(Total.usage.time, na.rm = TRUE), 1),
    carpet_mean = round(mean(Carpet.score, na.rm = TRUE), 1),
    pets_pct = round(mean(Pets == 1, na.rm = TRUE) * 100, 1),
    repair_pct = round(mean(Sent.for.repair == 1, na.rm = TRUE) * 100, 1),
    followup_mean = round(mean(Possession.time / 24, na.rm = TRUE), 1),  # in days
    followup_median = round(median(Possession.time / 24, na.rm = TRUE), 1)  # in days
  )

cat("\n=== DATASET SUMMARY ===\n")
print(summary_table)

# T-test: Does pet ownership affect usage time?
cat("\n=== T-TEST: Usage Time by Pet Ownership ===\n")
pet_usage_test <- t.test(Total.usage.time ~ Pets, data = data_eda)
print(pet_usage_test)
