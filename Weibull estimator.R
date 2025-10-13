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

# The models use status as event indicator: 1 means failure (Damage), 0 means censored (OK)

# Weibull ----------------------------------------------------------------------
# Models
weibull_battery <- survreg(Surv(Possession.time, Battery.status) ~ Pets + Carpet.score,
                           data = data, dist = "weibull")
weibull_impact <- survreg(Surv(Possession.time, Impact.status) ~ Pets + Carpet.score,
                          data = data, dist = "weibull")
weibull_ir <- survreg(Surv(Possession.time, IR.status) ~ Pets + Carpet.score,
                      data = data, dist = "weibull")

# Model summaries
summary(weibull_battery)
summary(weibull_impact)
summary(weibull_ir)

# NOTES (possession times):

# Battery model
  # Negative coefficients for Pets and Carpet.score indicate these factors shorten battery survival time.
  # Scale parameter (≈0.418) implies shape > 1, meaning hazard (failure rate) increases with time (aging effect).
# Impact sensor
  # Model did not converge properly 
  # EDA shows its the least occuring failure instance 
# IR sensor
  # Roughly constant hazard over time (shape approx = 1)
  # Estimates not statistically significant




# OLD NOTES (total usage time):
# Battery model
  # Pets =             0.15543 ---> having pets increases log survival time, so longer survival time (less damage/failure) for battery when pets present.
  # Carpet.score =     -0.01869 ---> higher carpet score reduces survival time (battery fails sooner).
  # Scale = exp(-1.58) ≈ 0.205 ---> shape parameter (1/scale) < 1 means hazard rate decreases over time (failures happen early, then decrease).
# Impact sensor model
  # Model did not converge properly so NA values 
  # Looks wrong - log likelihood values are crazy (negative and huge)
  # Maybe too few failures? unstable and unreliable
# IR sensor model
  # converged properly 
  # Pets significantly increase log survival, while carpet score has negligible effect. 
  # Scale ≈ 1 indicates roughly constant hazard over time. 
  # The AIC is comparable to the exponential model.


# Exponential ------------------------------------------------------------------
# Models
exp_battery <- survreg(Surv(Possession.time, Battery.status) ~ Pets + Carpet.score,
                       data = data, dist = "exponential")
exp_impact <- survreg(Surv(Possession.time, Impact.status) ~ Pets + Carpet.score ,
                      data = data, dist = "exponential")
exp_ir <- survreg(Surv(Possession.time, IR.status) ~ Pets + Carpet.score,
                  data = data, dist = "exponential")

# Model summaries
summary(exp_battery)
summary(exp_impact)
summary(exp_ir)

# NOTES:
  # All models assume a constant hazard rate over time (scale fixed at 1).
  # Negative coefficient for Pets and/or Carpet.score means those factors shorten survival time (accelerate failure) for components.
  # Positive coefficient means the factor is associated with longer survival time (delays failure).
  # For exp_battery and exp_impact, Pets significantly reduce survival time; Carpet.score mostly shortens time except in impact sensor (where it's positive).
  # For exp_ir, neither Pets nor Carpet.score are significant predictors of survival; covariates have no explanatory power (Chisq ~ 0, p ~ 1 indicates null effect).
  # Use exponential model for impact sensor when Weibull does not converge.

# OLD NOTES:
# Battery Model:
  # Coefficients indicate that pets reduce failure risk (negative hazard effect) and higher carpet score slightly increases failure rate. 
  # Scale is fixed at 1 (constant hazard).
# Impact sensor model
  # Exponential model converged - gave interpretable coefficients, unlike the Weibull model. 
  # Pets reduce failures, carpet score increases failures. 
  # This model is usable for the impact sensor since Weibull failed.
# IR sensor model
  # Similar to the Weibull model; coefficients indicate pets increase survival, carpet score negligible effect. 
  # Scale fixed at 1.


# Model comparison with AIC (Akaike Information Criterion) ---------------------
# Lower AIC indicates a better balance of model fit and complexity
aic_comparison_battery <- AIC(weibull_battery, exp_battery)
aic_comparison_ir <- AIC(weibull_ir, exp_ir)
aic_comparison_impact <- AIC(weibull_impact, exp_impact)

print(aic_comparison_battery)
print(aic_comparison_ir)
print(aic_comparison_impact)

# NOTES:
  # For Battery: Weibull model (AIC=16430.83) fits better than Exponential (AIC=17020.59).
  # For Infrared Sensor: Exponential model (AIC=7879.21) fits slightly better than Weibull (AIC=7880.78).
  # For Impact Sensor: Exponential model (AIC=2611.92) vastly outperforms Weibull (AIC=149732.12), confirming Weibull convergence issues.
  # Large AIC difference indicates strong evidence favoring the model with lowest AIC.
  # AIC is especially useful for comparing non-nested parametric survival models on the same dataset.
  # Note: AIC can be sensitive to high censoring; interpret with caution if censoring is large.


# OLD NOTES:

  # Battery: Weibull fits better than exponential (AIC = 12771.94 < 13487.43).
  # IR sensor: Exponential slightly better in terms of AIC (6229.1 < 6230.7).
  # Impact sensor: Weibull is unstable; exponential should be used.
  
  # Other Takeaways:
    # Non-convergence in Weibull for impact sensor suggests insufficient failure events (models require enough events for stable estimation)
    # Pets consistently affect survival across models; Carpet score effect is small and sometimes negligible.
    # Right censoring handled properly: devices without failure dates were treated as censored, which allows correct likelihood calculation.

