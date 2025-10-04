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
# Maybe too few failures? unstable and unreliable

# IR sensor model
weibull_ir <- survreg(Surv(Total.usage.time, IR.status) ~ Pets + Carpet.score,
                      data = data, dist = "weibull")
summary(weibull_ir)
# converged properly 
# Pets significantly increase log survival, while carpet score has negligible effect. 
# Scale ≈ 1 indicates roughly constant hazard over time. 
# The AIC is comparable to the exponential model.


# Exponential ----------------------------------

# Battery model
exp_battery <- survreg(Surv(Total.usage.time, Battery.status) ~ Pets + Carpet.score,
                       data = data, dist = "exponential")
summary(exp_battery)
# Coefficients indicate that pets reduce failure risk (negative hazard effect) and higher carpet score slightly increases failure rate. 
# Scale is fixed at 1 (constant hazard).

# Impact sensor model
exp_impact <- survreg(Surv(Total.usage.time, Impact.status) ~ Pets + Carpet.score,
                      data = data, dist = "exponential")
summary(exp_impact)
# Exponential model converged - gave interpretable coefficients, unlike the Weibull model. 
# Pets reduce failures, carpet score increases failures. 
# This model is usable for the impact sensor since Weibull failed.

# IR sensor model
exp_ir <- survreg(Surv(Total.usage.time, IR.status) ~ Pets + Carpet.score,
                  data = data, dist = "exponential")
summary(exp_ir)
# Similar to the Weibull model; coefficients indicate pets increase survival, carpet score negligible effect. 
# Scale fixed at 1.


# Compare the two models 
# (Got this from chat since I saw it in one of the coming lectures in week 7) --------------

AIC(exp_battery, weibull_battery)
AIC(exp_ir, weibull_ir)
# Battery: Weibull fits better than exponential (AIC = 12771.94 < 13487.43).
# IR sensor: Exponential slightly better in terms of AIC (6229.1 < 6230.7).
# Impact sensor: Weibull is unstable; exponential should be used.


# Key Takeaways
# Non-convergence in Weibull for impact sensor suggests insufficient failure events (models require enough events for stable estimation)
# Pets consistently affect survival across models; Carpet score effect is small and sometimes negligible.
# Right censoring handled properly: devices without failure dates were treated as censored, which allows correct likelihood calculation.