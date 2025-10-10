library(tidyverse)
library(lubridate)

# Set path to CSV data file 
file_path <- "D:\\TUE Study Material\\Q1\\Survival Analysis for Data Scientists\\GA_17\\Survival-Analysis-DirtSlurper3100-GA\\DirtSlurper3100.csv"

# Load packages 

# Read data
og_data <- read.table(file_path,
                      header = TRUE,
                      sep = ",",
                      skip = 12,   
                      stringsAsFactors = FALSE)

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
rows_to_drop <- union(ok_parts_but_failure_idx, damage_but_no_failure_date_idx)

# POSIX standard English locale 
Sys.setlocale("LC_TIME", "C")  
# (had to do this because dates with "Sep" were not being identified)

#Using lubridate for registration.date

data <- og_data %>%
  slice(-rows_to_drop) %>% # Drop the rows_to_drop
  mutate(
    # Replace "---" or empty strings with NA
    Registration.date = na_if(Registration.date, "---"),
    Failure.date = na_if(Failure.date, "---"),
    
    # Format dates 
    Registration.date = dmy(Registration.date), 
    Failure.date = dmy(Failure.date),
    
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
  filter(Total.usage.time > 0)  # Remove invalid survival times (these were devices registered on the last day of the study)

View(data)

# # To save a csv:
# write.csv(data, file = "C:/Users/20221564/Data_Science_and_Artificial_Intelligence/Survival-Analysis-DirtSlurper3100-GA/data_preprocessed.csv", row.names = FALSE)