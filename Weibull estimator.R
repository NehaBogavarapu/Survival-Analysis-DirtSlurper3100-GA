# Load packages 
library(survival) 
library(survminer) 


# Set path to CSV data file 
file_path <- "C:/Users/20221564/Data_Science_and_Artificial_Intelligence/Survival-Analysis-DirtSlurper3100-GA/data_preprocessed.csv"

# Read data
data <- read.table(file_path,
                      header = TRUE,
                      sep = ",",
                      stringsAsFactors = FALSE)

# View(data)


# Weibull ----------------------------------------------------------------------

# Battery model (using status as event indicator: 1 means failure (Damage), 0 means censored (OK))

weibull_battery <- survreg(Surv(Total.usage.time, Battery.status) ~ Pets + Carpet.score,
                           data = data, dist = "weibull")
summary(weibull_battery)
# Pets =             0.15543 ---> having pets increases log survival time, so longer survival time (less damage/failure) for battery when pets present.
# Carpet.score =     -0.01869 ---> higher carpet score reduces survival time (battery fails sooner).
# Scale = exp(-1.58) ≈ 0.205 ---> shape parameter < 1 means hazard rate decreases over time (failures happen early, then decrease).

# Impact sensor model
weibull_impact <- survreg(Surv(Total.usage.time, Impact.status) ~ Pets + Carpet.score,
                          data = data, dist = "weibull")
summary(weibull_impact)
# Model did not converge properly so NA values 
# Looks wrong - log likelihood values are crazy (negative and huge)
# Maybe too few failures?

# IR sensor model
weibull_ir <- survreg(Surv(Total.usage.time, IR.status) ~ Pets + Carpet.score,
                      data = data, dist = "weibull")
summary(weibull_ir)


# ----------------------------------

