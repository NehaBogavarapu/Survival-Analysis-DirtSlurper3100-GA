# Load packages 
library(survival) 
library(survminer) 


# Set path to CSV data file 
#file_path <- "C:/Users/20221564/Data_Science_and_Artificial_Intelligence/Survival-Analysis-DirtSlurper3100-GA/data_preprocessed.csv"
file_path <- "data_preprocessed.csv"


# Read data
data <- read.table(file_path,
                   header = TRUE,
                   sep = ",",
                   stringsAsFactors = FALSE)

# View(data).

# The models use status as event indicator: 1 means failure (Damage), 0 means censored (OK)

# Weibull ----------------------------------------------------------------------
# Models
weibull_battery <- survreg(Surv(Possession.time, Battery.status) ~ Total.usage.time + Pets + Carpet.score,
                           data = data, dist = "weibull")
weibull_impact <- survreg(Surv(Possession.time, Impact.status) ~ Total.usage.time + Pets + Carpet.score,
                          data = data, dist = "weibull")
weibull_ir <- survreg(Surv(Possession.time, IR.status) ~ Total.usage.time + Pets + Carpet.score,
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
exp_battery <- survreg(Surv(Possession.time, Battery.status) ~ Total.usage.time + Pets + Carpet.score,
                       data = data, dist = "exponential")
exp_impact <- survreg(Surv(Possession.time, Impact.status) ~ Total.usage.time + Pets + Carpet.score ,
                      data = data, dist = "exponential")
exp_ir <- survreg(Surv(Possession.time, IR.status) ~ Total.usage.time + Pets + Carpet.score,
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

# Key Takeaways
# Non-convergence in Weibull for impact sensor suggests insufficient failure events (models require enough events for stable estimation)
# Pets consistently affect survival across models; Carpet score effect is small and sometimes negligible.
# Right censoring handled properly: devices without failure dates were treated as censored, which allows correct likelihood calculation.

# ===========================================================================
# 1. LOG-RANK TEST - EXTREME USAGE EFFECT
# ===========================================================================

cat("\n\n=== LOG-RANK TEST: EXTREME USAGE (>=2400 hours) ===\n")

# Create extreme usage indicator based on operational hours
data <- data %>%
  mutate(extreme_use = ifelse(Total.usage.time >= 2400, 
                              "Extreme (>=2400h)", 
                              "Normal (<2400h)"))

# Battery - using POSSESSION TIME as time scale
cat("\n--- BATTERY ---\n")
surv_battery <- Surv(data$Possession.time, data$Battery.status)  # Convert to days
logrank_battery <- survdiff(surv_battery ~ extreme_use, data = data)
print(logrank_battery)
cat("p-value:", format.pval(1 - pchisq(logrank_battery$chisq, 1)), "\n")

# Kaplan-Meier plot
fit_battery <- survfit(surv_battery ~ extreme_use, data = data)
ggsurvplot(fit_battery, data = data,
           pval = TRUE, conf.int = TRUE, risk.table = TRUE,
           xlab = "Time (days)", ylab = "Survival Probability",
           title = "Battery Survival by Usage Intensity",
           palette = c("cadetblue", "coral"))

# IR Sensor
cat("\n--- IR SENSOR ---\n")
surv_ir <- Surv(data$Possession.time, data$IR.status)
logrank_ir <- survdiff(surv_ir ~ extreme_use, data = data)
print(logrank_ir)
cat("p-value:", format.pval(1 - pchisq(logrank_ir$chisq, 1)), "\n")

# Impact Sensor
cat("\n--- IMPACT SENSOR ---\n")
surv_impact <- Surv(data$Possession.time, data$Impact.status)
logrank_impact <- survdiff(surv_impact ~ extreme_use, data = data)
print(logrank_impact)
cat("p-value:", format.pval(1 - pchisq(logrank_impact$chisq, 1)), "\n")

# ============================================================================
# 2. ACCELERATED FAILURE TIME MODELS
# Covariates: Total.usage.time (operational), Pets, Carpet.score
# ============================================================================

cat("\n\n=== ACCELERATED FAILURE TIME MODELS ===\n")
cat("Time scale: Calendar time (Possession.time)\n")
cat("Covariates: Usage intensity (Total.usage.time), Pets, Carpet.score\n\n")

# Battery - Weibull
# Battery - Weibull.

cat("--- BATTERY (Weibull) ---\n")
weibull_battery <- survreg(Surv(Possession.time, Battery.status) ~ 
                             Total.usage.time + Pets + Carpet.score,
                           data = data, dist = "weibull")

if (weibull_battery$iter >= 30) {
  cat("WARNING: Model reached iteration limit.\n\n")
}

summary(weibull_battery)

# Extract parameters
lambda_battery <- exp(coef(weibull_battery)[1])
k_battery <- 1 / weibull_battery$scale

cat("\n=== INTERPRETATION ===\n")
cat("Shape (k):", round(k_battery, 3), "\n")
if (k_battery > 1) {
  cat("  → Increasing hazard (wear-out)\n")
} else if (k_battery < 1) {
  cat("  → Decreasing hazard (infant mortality)\n")
} else {
  cat("  → Constant hazard\n")
}

cat("\nCovariate Effects (AFT parameterization):\n")
coef_summary <- summary(weibull_battery)$table
for (i in 2:(nrow(coef_summary)-1)) {
  var <- rownames(coef_summary)[i]
  coef <- coef_summary[i, "Value"]
  pval <- coef_summary[i, "p"]
  
  cat(sprintf("  %s: β=%.4f, p=%s\n", var, coef, format.pval(pval)))
  
  if (var == "Total.usage.time") {
    cat(sprintf("    → Each additional hour of usage changes survival time by %.2f%%\n",
                (exp(coef) - 1) * 100))
  } else if (coef > 0) {
    cat(sprintf("    → Increases survival time by %.1f%%\n", (exp(coef) - 1) * 100))
  } else {
    cat(sprintf("    → Decreases survival time by %.1f%%\n", (1 - exp(coef)) * 100))
  }
}

# IR Sensor - Weibull
cat("\n--- IR SENSOR (Weibull) ---\n")
weibull_ir <- survreg(Surv(Possession.time , IR.status) ~ 
                        Total.usage.time + Pets + Carpet.score,
                      data = data, dist = "weibull")
summary(weibull_ir)

lambda_ir <- exp(coef(weibull_ir)[1])
k_ir <- 1 / weibull_ir$scale
cat("\nShape (k):", round(k_ir, 3), "\n")

# Impact Sensor - Try Weibull first
cat("\n--- IMPACT SENSOR ---\n")
weibull_impact <- tryCatch({
  survreg(Surv(Possession.time , Impact.status) ~ 
            Total.usage.time + Pets + Carpet.score,
          data = data, dist = "weibull")
}, error = function(e) NULL)

if (is.null(weibull_impact) || any(is.na(coef(weibull_impact)))) {
  cat("Weibull failed. Using Exponential.\n")
  exp_impact <- survreg(Surv(Possession.time , Impact.status) ~ 
                          Total.usage.time + Pets + Carpet.score,
                        data = data, dist = "exponential")
  summary(exp_impact)
  impact_model <- exp_impact
} else {
  summary(weibull_impact)
  impact_model <- weibull_impact
}

# ============================================================================
# 3. LIKELIHOOD RATIO TESTS
# ============================================================================

cat("\n\n=== LIKELIHOOD RATIO TESTS ===\n")

# Test 1: Battery - Are covariates significant?
cat("\n--- Test 1: Battery Covariates ---\n")

weibull_battery_null <- survreg(Surv(Possession.time , Battery.status) ~ 1,
                                data = data, dist = "weibull")

if (weibull_battery$loglik[2] > weibull_battery_null$loglik[2]) {
  cat("WARNING: Convergence issue detected\n")
  LRT1 <- 2 * abs(weibull_battery$loglik[2] - weibull_battery_null$loglik[2])
} else {
  LRT1 <- 2 * (weibull_battery$loglik[2] - weibull_battery_null$loglik[2])
}

p_val1 <- 1 - pchisq(abs(LRT1), df = 3)  # 3 covariates now

cat("LRT statistic:", abs(LRT1), "\n")
cat("df: 3\n")
cat("p-value:", format.pval(p_val1), "\n")
cat("Result:", ifelse(p_val1 < 0.05, "Covariates SIGNIFICANT", "Not significant"), "\n")

# Test 2: Battery - Weibull vs Exponential
cat("\n--- Test 2: Battery Distribution ---\n")
exp_battery <- survreg(Surv(Possession.time , Battery.status) ~ 
                         Total.usage.time + Pets + Carpet.score,
                       data = data, dist = "exponential")
LRT2 <- 2 * abs(weibull_battery$loglik[2] - exp_battery$loglik[2])
p_val2 <- 1 - pchisq(LRT2, df = 1)

cat("LRT statistic:", LRT2, "\n")
cat("p-value:", format.pval(p_val2), "\n")
cat("Result:", ifelse(p_val2 < 0.05, "Weibull PREFERRED", "Exponential adequate"), "\n")

# Model comparison
cat("\nAIC Comparison:\n")
print(AIC(exp_battery, weibull_battery))

cat("\n\n=== LIKELIHOOD RATIO TESTS ===\n")

# Test 3: Impact - Are covariates significant?
cat("\n--- Test 3: Impact Covariates ---\n")

weibull_impact_null <- survreg(Surv(Possession.time , Impact.status) ~ 1,
                                data = data, dist = "weibull")

if (weibull_impact$loglik[2] > weibull_impact_null$loglik[2]) {
  cat("WARNING: Convergence issue detected\n")
  LRT1 <- 2 * abs(weibull_impact$loglik[2] - weibull_impact_null$loglik[2])
} else {
  LRT1 <- 2 * (weibull_impact$loglik[2] - weibull_impact_null$loglik[2])
}

p_val1 <- 1 - pchisq(abs(LRT1), df = 3)  # 3 covariates now

cat("LRT statistic:", abs(LRT1), "\n")
cat("df: 3\n")
cat("p-value:", format.pval(p_val1), "\n")
cat("Result:", ifelse(p_val1 < 0.05, "Covariates SIGNIFICANT", "Not significant"), "\n")


# Test 4: Impact - Weibull vs Exponential
cat("\n--- Test 4: Impact Distribution ---\n")

exp_impact <- survreg(Surv(Possession.time , Impact.status) ~ 
                         Total.usage.time + Pets + Carpet.score,
                       data = data, dist = "exponential")
LRT2 <- 2 * abs(weibull_impact$loglik[2] - exp_impact$loglik[2])
p_val2 <- 1 - pchisq(LRT2, df = 1)

cat("LRT statistic:", LRT2, "\n")
cat("p-value:", format.pval(p_val2), "\n")
cat("Result:", ifelse(p_val2 < 0.05, "Weibull PREFERRED", "Exponential adequate"), "\n")

# Model comparison
cat("\nAIC Comparison:\n")
print(AIC(exp_impact, weibull_impact))

cat("\n\n=== LIKELIHOOD RATIO TESTS ===\n")

# Test 5: IR - Are covariates significant?
cat("\n--- Test 5: IR Covariates ---\n")

weibull_ir_null <- survreg(Surv(Possession.time , IR.status) ~ 1,
                                data = data, dist = "weibull")

if (weibull_ir$loglik[2] > weibull_ir_null$loglik[2]) {
  cat("WARNING: Convergence issue detected\n")
  LRT1 <- 2 * abs(weibull_ir$loglik[2] - weibull_ir_null$loglik[2])
} else {
  LRT1 <- 2 * (weibull_ir$loglik[2] - weibull_ir_null$loglik[2])
}

p_val1 <- 1 - pchisq(abs(LRT1), df = 3)  # 3 covariates now

cat("LRT statistic:", abs(LRT1), "\n")
cat("df: 3\n")
cat("p-value:", format.pval(p_val1), "\n")
cat("Result:", ifelse(p_val1 < 0.05, "Covariates SIGNIFICANT", "Not significant"), "\n")

# Test 6: IR - Weibull vs Exponential
cat("\n--- Test 6: Battery Distribution ---\n")
exp_ir <- survreg(Surv(Possession.time , IR.status) ~ 
                         Total.usage.time + Pets + Carpet.score,
                       data = data, dist = "exponential")

LRT2 <- 2 * abs(weibull_ir$loglik[2] - exp_ir$loglik[2])
p_val2 <- 1 - pchisq(LRT2, df = 1)

cat("LRT statistic:", LRT2, "\n")
cat("p-value:", format.pval(p_val2), "\n")
cat("Result:", ifelse(p_val2 < 0.05, "Weibull PREFERRED", "Exponential adequate"), "\n")

# Model comparison
cat("\nAIC Comparison:\n")
print(AIC(exp_ir, weibull_ir))

# ============================================================================
# 4. RESIDUAL ANALYSIS - DETAILED EXPLANATION
# 4. RESIDUAL ANALYSIS 
# ============================================================================

cat("\n\n=== RESIDUAL ANALYSIS - DETAILED ===\n")
cat("\n\n=== RESIDUAL ANALYSIS - DETAILED EXPLANATION ===\n")

analyze_residuals_detailed <- function(model, data, event_col, time_col, comp_name) {
  cat("\n" ," ═══════════════════════════════════════════════\n")
  cat("  ", comp_name, "RESIDUAL DIAGNOSTICS\n")
  cat("  ═══════════════════════════════════════════════\n\n")
  
  # Get model parameters
  lp <- predict(model, type = "lp")  # Linear predictor: X'β
  scale_param <- model$scale         # Scale parameter
  shape_param <- 1 / scale_param     # Shape = 1/scale for survreg
  event <- data[[event_col]]

  time_days <- data[[time_col]] 

  
  cat("Model type:", model$dist, "\n")
  cat("Shape parameter (k):", round(shape_param, 3), "\n")
  cat("Number of events:", sum(event), "/", length(event), "\n\n")
  
  # -------------------------------------------------------------------------
  # 1. COX-SNELL RESIDUALS
  # -------------------------------------------------------------------------
  cat("─────────────────────────────────────────────────\n")
  cat("1. COX-SNELL RESIDUALS\n")
  cat("─────────────────────────────────────────────────\n")
  cat("Definition: r_CS = H(t) = cumulative hazard at observed time\n")
  cat("Formula: r_CS = (t / exp(X'β))^k for Weibull\n")
  cat("Property: If model correct, r_CS ~ Exponential(1)\n")
  cat("Check: Plot cumulative hazard vs r_CS, should be 45° line\n\n")
  
  # Calculate Cox-Snell residuals
  cox_snell <- (time_days / exp(lp))^shape_param
  
  # Create survival object for residuals
  surv_cs <- survfit(Surv(cox_snell, event) ~ 1)
  
  cat("Range: [", round(min(cox_snell[is.finite(cox_snell)], na.rm = TRUE), 3),
      ",", round(max(cox_snell[is.finite(cox_snell)], na.rm = TRUE), 3), "]\n")
  cat("Mean:", round(mean(cox_snell[is.finite(cox_snell)], na.rm = TRUE), 3),
      "(should be approx. 1 if Exp(1))\n\n")
  
  # -------------------------------------------------------------------------
  # 2. MARTINGALE-LIKE RESIDUALS
  # -------------------------------------------------------------------------
  cat("─────────────────────────────────────────────────\n")
  cat("2. MARTINGALE-LIKE RESIDUALS\n")
  cat("─────────────────────────────────────────────────\n")
  cat("Definition: r_M = δ - H(t)\n")
  cat("  where δ = event indicator (1=failed, 0=censored)\n")
  cat("        H(t) = cumulative hazard (Cox-Snell residual)\n")
  cat("Range: (-∞, 1]\n")
  cat("  → Failed observations: approach 1\n")
  cat("  → Censored observations: negative values\n")
  cat("Property: Should scatter randomly around 0\n")
  cat("Use: Detect functional form misspecification\n\n")
  
  mart_resid <- event - cox_snell
  
  cat("Range: [", round(min(mart_resid, na.rm = TRUE), 3),
      ",", round(max(mart_resid, na.rm = TRUE), 3), "]\n")
  cat("Mean:", round(mean(mart_resid, na.rm = TRUE), 3), "(should be ≈0)\n")
  cat("Events (δ=1): mean =", round(mean(mart_resid[event == 1], na.rm = TRUE), 3), "\n")
  cat("Censored (δ=0): mean =", round(mean(mart_resid[event == 0], na.rm = TRUE), 3), "\n\n")
  
  # -------------------------------------------------------------------------
  # 3. DEVIANCE RESIDUALS
  # -------------------------------------------------------------------------
  cat("─────────────────────────────────────────────────\n")
  cat("3. DEVIANCE RESIDUALS\n")
  cat("─────────────────────────────────────────────────\n")
  cat("Definition: Signed square root of contribution to -2*log-likelihood\n")
  cat("Property: More symmetric than martingale residuals\n")
  cat("Range: Approximately (-3, 3) for well-fitted models\n")
  cat("Interpretation:\n")
  cat("  |r_D| > 2: Potential outlier\n")
  cat("  |r_D| > 3: Strong outlier\n")
  cat("Use: Identify poorly fitted observations\n\n")
  
  dev_resid <- residuals(model, type = "deviance")
  
  cat("Range: [", round(min(dev_resid, na.rm = TRUE), 3),
      ",", round(max(dev_resid, na.rm = TRUE), 3), "]\n")
  cat("SD:", round(sd(dev_resid, na.rm = TRUE), 3), "(should be approx. 1)\n")
  
  outliers_2 <- sum(abs(dev_resid) > 2, na.rm = TRUE)
  outliers_3 <- sum(abs(dev_resid) > 3, na.rm = TRUE)
  
  cat("Outliers |r| > 2:", outliers_2, "/", length(dev_resid),
      sprintf("(%.1f%%)\n", 100 * outliers_2 / length(dev_resid)))
  cat("Outliers |r| > 3:", outliers_3, "/", length(dev_resid),
      sprintf("(%.1f%%)\n", 100 * outliers_3 / length(dev_resid)))
  
  # Shapiro-Wilk test (sample if too large)
  sample_size <- min(5000, length(dev_resid))
  shapiro_result <- shapiro.test(sample(dev_resid, sample_size))
  cat("\nShapiro-Wilk normality test: p =", format.pval(shapiro_result$p.value))
  if (shapiro_result$p.value < 0.05) {
    cat(" (Reject normality)\n")
  } else {
    cat(" (Cannot reject normality)\n")
  }
  
  # -------------------------------------------------------------------------
  # DIAGNOSTIC PLOTS
  # -------------------------------------------------------------------------
  cat("\n─────────────────────────────────────────────────\n")
  cat("CREATING DIAGNOSTIC PLOTS...\n")
  cat("─────────────────────────────────────────────────\n\n")
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  # Plot 1: Cox-Snell Check
  plot(surv_cs, fun = "cumhaz",
       xlab = "Cox-Snell Residuals",
       ylab = "Cumulative Hazard H(r)",
       main = paste(comp_name, "- Cox-Snell Check"),
       xlim = c(0, quantile(cox_snell[is.finite(cox_snell)], 0.95, na.rm = TRUE)))
  abline(0, 1, col = "red", lty = 2, lwd = 2)
  legend("topleft", legend = c("Observed", "Expected (45° line)"),
         col = c("black", "red"), lty = c(1, 2), bty = "n", cex = 0.8)
  text(0.1, par("usr")[4] * 0.9, 
       "Good fit: points follow red line", 
       adj = 0, cex = 0.7, col = "blue")
  
  # Plot 2: Martingale vs Linear Predictor
  plot(lp, mart_resid,
       xlab = "Linear Predictor (X'β)",
       ylab = "Martingale Residuals",
       main = paste(comp_name, "- Martingale"),
       pch = 16, cex = 0.5, col = rgb(0, 0, 0, 0.3))
  abline(h = 0, col = "red", lty = 2, lwd = 2)
  lines(lowess(lp, mart_resid), col = "blue", lwd = 2)
  legend("topright", 
         legend = c("Zero line", "Lowess smooth"),
         col = c("red", "blue"), lty = c(2, 1), lwd = 2, 
         bty = "n", cex = 0.7)
  text(par("usr")[1], par("usr")[4] * 0.9,
       "Good fit: blue line flat around 0",
       adj = 0, cex = 0.7, col = "blue")
  
  # Plot 3: Deviance vs Linear Predictor
  plot(lp, dev_resid,
       xlab = "Linear Predictor (X'β)",
       ylab = "Deviance Residuals",
       main = paste(comp_name, "- Deviance"),
       pch = 16, cex = 0.5, col = rgb(0, 0, 0, 0.3))
  abline(h = 0, col = "red", lty = 2, lwd = 2)
  abline(h = c(-2, 2), col = "orange", lty = 2, lwd = 1.5)
  abline(h = c(-3, 3), col = "darkred", lty = 3, lwd = 1.5)
  legend("topright",
         legend = c("Zero", "±2 (outliers)", "±3 (strong)"),
         col = c("red", "orange", "darkred"),
         lty = c(2, 2, 3), lwd = c(2, 1.5, 1.5),
         bty = "n", cex = 0.7)
  
  # Plot 4: QQ Plot
  qqnorm(dev_resid, main = paste(comp_name, "- QQ Plot"),
         pch = 16, cex = 0.5)
  qqline(dev_resid, col = "red", lwd = 2)
  text(par("usr")[1], par("usr")[4] * 0.9,
       "Good fit: points follow red line",
       adj = 0, cex = 0.7, col = "blue")
  
  par(mfrow = c(1, 1))
  
  cat("\n")
  cat("═══════════════════════════════════════════════\n")
  cat("PLOT INTERPRETATION GUIDE:\n")
  cat("═══════════════════════════════════════════════\n")
  cat("1. Cox-Snell: Points on 45° line = good fit\n")
  cat("   - Deviation above line: underestimate hazard\n")
  cat("   - Deviation below line: overestimate hazard\n\n")
  
  cat("2. Martingale: Blue lowess line flat around 0\n")
  cat("   - U-shape: missing quadratic term\n")
  cat("   - Trend: functional form misspecified\n\n")
  
  cat("3. Deviance: Random scatter, most within ±2\n")
  cat("   - Points outside ±2: investigate these cases\n")
  cat("   - Pattern: model inadequate\n\n")
  
  cat("4. QQ Plot: Points on diagonal = normality\n")
  cat("   - Tails deviate: outliers present\n")
  cat("   - S-shape: skewed distribution\n\n")
  
  invisible(list(
    cox_snell = cox_snell,
    martingale = mart_resid,
    deviance = dev_resid
  ))
}

# Analyzing each component
resid_battery <- analyze_residuals_detailed(weibull_battery, data, 
                                            "Battery.status", "Possession.time",
                                            "BATTERY")

resid_ir <- analyze_residuals_detailed(weibull_ir, data,
                                       "IR.status", "Possession.time",
                                       "IR SENSOR")

resid_impact <- analyze_residuals_detailed(impact_model, data,
                                           "Impact.status", "Possession.time",
                                           "IMPACT SENSOR")
# ============================================================================
# 5. ADDITIONAL ANALYSES FOR SPECIFICATION CHECKS
# Testing individual covariate effects for warranty considerations
# ============================================================================

cat("\n\n═══════════════════════════════════════════════\n")
cat("ADDITIONAL ANALYSES: PETS & CARPET EFFECTS\n")
cat("═══════════════════════════════════════════════\n\n")

# ============================================================================
# 5.1 LOG-RANK TESTS FOR PETS EFFECT (All Components)
# ============================================================================

cat("─────────────────────────────────────────────────\n")
cat("LOG-RANK TEST: PETS EFFECT ON SURVIVAL\n")
cat("─────────────────────────────────────────────────\n")
cat("Research question: Does pet ownership affect component survival?\n")
cat("Implication: Should warranties differentiate by pet ownership?\n\n")

# Create factor for pets
data <- data %>%
  mutate(Pets_factor = factor(Pets, levels = c(0, 1), labels = c("No Pets", "Has Pets")))

# Battery - Pets effect
cat("BATTERY - Pets Effect\n")
surv_battery_pets <- Surv(data$Possession.time , data$Battery.status)
logrank_battery_pets <- survdiff(surv_battery_pets ~ Pets_factor, data = data)
print(logrank_battery_pets)

p_battery_pets <- 1 - pchisq(logrank_battery_pets$chisq, 1)
cat("\np-value:", format.pval(p_battery_pets), "\n")
cat("Interpretation:", 
    ifelse(p_battery_pets < 0.05, 
           "SIGNIFICANT - Pet ownership affects battery survival", 
           "Not significant"), "\n")

# Direction of effect
obs_pets <- logrank_battery_pets$obs[logrank_battery_pets$n > 0]
exp_pets <- logrank_battery_pets$exp[logrank_battery_pets$n > 0]
if (length(obs_pets) == 2) {
  if (obs_pets[2] < exp_pets[2]) {
    cat("Direction: Pets are PROTECTIVE (fewer failures than expected)\n")
  } else {
    cat("Direction: Pets are HARMFUL (more failures than expected)\n")
  }
}
cat("\n")

# IR Sensor - Pets effect
cat("IR SENSOR - Pets Effect\n")
surv_ir_pets <- Surv(data$Possession.time , data$IR.status)
logrank_ir_pets <- survdiff(surv_ir_pets ~ Pets_factor, data = data)
print(logrank_ir_pets)

p_ir_pets <- 1 - pchisq(logrank_ir_pets$chisq, 1)
cat("\np-value:", format.pval(p_ir_pets), "\n")
cat("Interpretation:", 
    ifelse(p_ir_pets < 0.05, 
           "SIGNIFICANT - Pet ownership affects IR sensor survival", 
           "Not significant"), "\n\n")

# Impact Sensor - Pets effect
cat("IMPACT SENSOR - Pets Effect\n")
surv_impact_pets <- Surv(data$Possession.time , data$Impact.status)
logrank_impact_pets <- survdiff(surv_impact_pets ~ Pets_factor, data = data)
print(logrank_impact_pets)

p_impact_pets <- 1 - pchisq(logrank_impact_pets$chisq, 1)
cat("\np-value:", format.pval(p_impact_pets), "\n")
cat("Interpretation:", 
    ifelse(p_impact_pets < 0.05, 
           "SIGNIFICANT - Pet ownership affects impact sensor survival", 
           "Not significant"), "\n")

# Direction of effect for Impact
obs_pets_impact <- logrank_impact_pets$obs[logrank_impact_pets$n > 0]
exp_pets_impact <- logrank_impact_pets$exp[logrank_impact_pets$n > 0]
if (length(obs_pets_impact) == 2) {
  if (obs_pets_impact[2] < exp_pets_impact[2]) {
    cat("Direction: Pets are PROTECTIVE (fewer failures than expected)\n")
  } else {
    cat("Direction: Pets are HARMFUL (more failures than expected)\n")
  }
}
cat("\n")

# ============================================================================
# 5.2 LOG-RANK TESTS FOR CARPET EFFECT (All Components)
# ============================================================================

cat("─────────────────────────────────────────────────\n")
cat("LOG-RANK TEST: CARPET SCORE EFFECT ON SURVIVAL\n")
cat("─────────────────────────────────────────────────\n")
cat("Research question: Does carpet coverage affect component survival?\n")
cat("Note: Grouping carpet scores into Low/Medium/High for testing\n\n")

# Create carpet groups (Low: 1-3, Medium: 4-6, High: 7-9)
data <- data %>%
  mutate(Carpet_group = cut(Carpet.score, 
                            breaks = c(0, 3, 6, 9),
                            labels = c("Low (1-3)", "Medium (4-6)", "High (7-9)"),
                            include.lowest = TRUE))

# Battery - Carpet effect
cat("BATTERY - Carpet Effect\n")
logrank_battery_carpet <- survdiff(surv_battery_pets ~ Carpet_group, data = data)
print(logrank_battery_carpet)

p_battery_carpet <- 1 - pchisq(logrank_battery_carpet$chisq, df = 2)
cat("\np-value:", format.pval(p_battery_carpet), "\n")
cat("Interpretation:", 
    ifelse(p_battery_carpet < 0.05, 
           "SIGNIFICANT - Carpet coverage affects battery survival", 
           "Not significant"), "\n\n")

# IR Sensor - Carpet effect
cat("IR SENSOR - Carpet Effect\n")
logrank_ir_carpet <- survdiff(surv_ir_pets ~ Carpet_group, data = data)
print(logrank_ir_carpet)

p_ir_carpet <- 1 - pchisq(logrank_ir_carpet$chisq, df = 2)
cat("\np-value:", format.pval(p_ir_carpet), "\n")
cat("Interpretation:", 
    ifelse(p_ir_carpet < 0.05, 
           "SIGNIFICANT - Carpet coverage affects IR sensor survival", 
           "Not significant"), "\n\n")

# Impact Sensor - Carpet effect
cat("IMPACT SENSOR - Carpet Effect\n")
logrank_impact_carpet <- survdiff(surv_impact_pets ~ Carpet_group, data = data)
print(logrank_impact_carpet)

p_impact_carpet <- 1 - pchisq(logrank_impact_carpet$chisq, df = 2)
cat("\np-value:", format.pval(p_impact_carpet), "\n")
cat("Interpretation:", 
    ifelse(p_impact_carpet < 0.05, 
           "SIGNIFICANT - Carpet coverage affects impact sensor survival", 
           "Not significant"), "\n\n")

# ============================================================================
# 5.3 MANUFACTURER SPECIFICATIONS CHECK
# ============================================================================

cat("═══════════════════════════════════════════════\n")
cat("MANUFACTURER SPECIFICATIONS VERIFICATION\n")
cat("═══════════════════════════════════════════════\n\n")

# Define median usage for predictions
median_usage <- median(data$Total.usage.time, na.rm = TRUE)
cat("Reference conditions:\n")
cat("  Usage intensity:", round(median_usage, 1), "hours (median)\n")
cat("  Pets: No\n")
cat("  Carpet score: 5 (medium)\n\n")

# ---- IR SENSOR ----
cat("─────────────────────────────────────────────────\n")
cat("IR SENSOR: L10 >= 2,000 days\n")
cat("─────────────────────────────────────────────────\n")

L10_ir <- predict(weibull_ir,
                  newdata = data.frame(
                    Total.usage.time = median_usage,
                    Pets = 0,
                    Carpet.score = 5
                  ),
                  type = "quantile", p = 0.1)

L50_ir <- predict(weibull_ir,
                  newdata = data.frame(
                    Total.usage.time = median_usage,
                    Pets = 0,
                    Carpet.score = 5
                  ),
                  type = "quantile", p = 0.5)

cat("Estimated L10:", round(L10_ir, 1), "days\n")
cat("Estimated L50 (median):", round(L50_ir, 1), "days\n")
cat("Manufacturer guarantee: >=2,000 days\n\n")

if (L10_ir >= 2000) {
  cat("SPECIFICATION MET\n")
  cat("   Exceeds requirement by", round(L10_ir - 2000, 1), "days\n\n")
} else {
  cat("SPECIFICATION VIOLATED\n")
  cat("   Falls short by", round(2000 - L10_ir, 1), "days\n")
  cat("   Performance ratio:", round(L10_ir / 2000 * 100, 1), "% of guarantee\n\n")
}


# ---- BATTERY ----
cat("─────────────────────────────────────────────────\n")
cat("BATTERY: L10 >= 1,000 days (excluding extreme use >= 2,400h)\n")
cat("─────────────────────────────────────────────────\n")

# Filter to normal use only
#data_normal <- data %>% filter(Total.usage.time < 2400)
#data_normal <- data %>% filter(Total.usage.time < 2400)
#median_usage_normal <- median(data_normal$Total.usage.time, na.rm = TRUE)
data_normal <- data[data$Total.usage.time < 2400, ]
median_usage_normal <- median(data_normal$Total.usage.time, na.rm = TRUE)

cat("Normal use dataset: n =", nrow(data_normal), "\n")
cat("Median usage (normal):", round(median_usage_normal, 1), "hours\n\n")

# Fit model on normal use data
weibull_battery_normal <- tryCatch({
  survreg(Surv(Possession.time , Battery.status) ~
            Total.usage.time + Pets + Carpet.score,
          data = data_normal, dist = "weibull")
}, warning = function(w) {
  cat("Warning:", conditionMessage(w), "\n")
  survreg(Surv(Possession.time , Battery.status) ~
            Total.usage.time + Pets + Carpet.score,
          data = data_normal, dist = "weibull")
})

L10_battery <- predict(weibull_battery_normal,
                       newdata = data.frame(
                         Total.usage.time = median_usage_normal,
                         Pets = 0,
                         Carpet.score = 5
                       ),
                       type = "quantile", p = 0.1)

L50_battery <- predict(weibull_battery_normal,
                       newdata = data.frame(
                         Total.usage.time = median_usage_normal,
                         Pets = 0,
                         Carpet.score = 5
                       ),
                       type = "quantile", p = 0.5)

cat("Estimated L10:", round(L10_battery, 1), "days\n")
cat("Estimated L50 (median):", round(L50_battery, 1), "days\n")
cat("Manufacturer guarantee: >= 1,000 days\n\n")

if (L10_battery >= 1000) {
  cat("SPECIFICATION MET\n")
  cat("   Exceeds requirement by", round(L10_battery - 1000, 1), "days\n\n")
} else {
  cat("SPECIFICATION VIOLATED\n")
  cat("   Falls short by", round(1000 - L10_battery, 1), "days\n")
  cat("   Performance ratio:", round(L10_battery / 1000 * 100, 1), "% of guarantee\n\n")
}

# ---- IMPACT SENSOR ----
cat("─────────────────────────────────────────────────\n")
cat("IMPACT SENSOR: Recommended Specifications\n")
cat("─────────────────────────────────────────────────\n")
cat("(IButler in-house component - setting warranty terms)\n\n")

L10_impact <- predict(impact_model,
                      newdata = data.frame(
                        Total.usage.time = median_usage,
                        Pets = 0,
                        Carpet.score = 5
                      ),
                      type = "quantile", p = 0.1)

L50_impact <- predict(impact_model,
                      newdata = data.frame(
                        Total.usage.time = median_usage,
                        Pets = 0,
                        Carpet.score = 5
                      ),
                      type = "quantile", p = 0.5)

L90_impact <- predict(impact_model,
                      newdata = data.frame(
                        Total.usage.time = median_usage,
                        Pets = 0,
                        Carpet.score = 5
                      ),
                      type = "quantile", p = 0.9)

cat("Current Performance (reference conditions):\n")
cat("   L10:", round(L10_impact, 0), "days\n")
cat("   L50:", round(L50_impact, 0), "days\n")
cat("   L90:", round(L90_impact, 0), "days\n\n")

# Test if pets/carpet should affect warranty
cat("IMPACT SENSOR WARRANTY CONSIDERATIONS:\n\n")

cat("1. Should warranties differ by pet ownership?\n")
cat("   Log-rank test p-value:", format.pval(p_impact_pets), "\n")
if (p_impact_pets < 0.05) {
  cat("YES: Pets significantly affect impact sensor survival\n")
  cat("Consider differentiated warranty terms\n\n")
  
  # Calculate L10 for pets vs no pets
  L10_impact_no_pets <- predict(impact_model,
                                newdata = data.frame(
                                  Total.usage.time = median_usage,
                                  Pets = 0,
                                  Carpet.score = 5
                                ),
                                type = "quantile", p = 0.1)
  
  L10_impact_with_pets <- predict(impact_model,
                                  newdata = data.frame(
                                    Total.usage.time = median_usage,
                                    Pets = 1,
                                    Carpet.score = 5
                                  ),
                                  type = "quantile", p = 0.1)
  
  cat("   L10 without pets:", round(L10_impact_no_pets, 0), "days\n")
  cat("   L10 with pets:", round(L10_impact_with_pets, 0), "days\n")
  cat("   Difference:", round(L10_impact_no_pets - L10_impact_with_pets, 0), "days\n\n")
} else {
  cat("NO: Pets do not significantly affect survival\n")
  cat("Single warranty term sufficient\n\n")
}

cat("2. Should warranties differ by carpet coverage?\n")
cat("Log-rank test p-value:", format.pval(p_impact_carpet), "\n")
if (p_impact_carpet < 0.05) {
  cat("YES: Carpet coverage significantly affects survival\n")
  cat("Consider differentiated warranty terms\n\n")
  
  # Calculate L10 for different carpet levels
  L10_impact_low_carpet <- predict(impact_model,
                                   newdata = data.frame(
                                     Total.usage.time = median_usage,
                                     Pets = 0,
                                     Carpet.score = 2
                                   ),
                                   type = "quantile", p = 0.1)
  
  L10_impact_high_carpet <- predict(impact_model,
                                    newdata = data.frame(
                                      Total.usage.time = median_usage,
                                      Pets = 0,
                                      Carpet.score = 8
                                    ),
                                    type = "quantile", p = 0.1)
  
  cat("L10 low carpet (score=2):", round(L10_impact_low_carpet, 0), "days\n")
  cat("L10 high carpet (score=8):", round(L10_impact_high_carpet, 0), "days\n")
  cat("Difference:", round(L10_impact_low_carpet - L10_impact_high_carpet, 0), "days\n\n")
} else {
  cat("NO: Carpet coverage does not significantly affect survival\n")
  cat("Single warranty term sufficient\n\n")
}

cat("RECOMMENDED SPECIFICATIONS for Impact Sensor:\n")
cat("   Conservative guarantee: L10 >=", round(L10_impact * 0.9, 0), "days\n")
cat("   Standard guarantee: L50 >=", round(L50_impact * 0.8, 0), "days\n\n")

cat("Note: Only", sum(data$Impact.status), "failures in", nrow(data), 
    "devices (", round(100 * mean(data$Impact.status), 2), "%)\n")
cat("      Impact sensor shows excellent reliability\n\n")

# ============================================================================
# 5.4 SUMMARY: WARRANTY DIFFERENTIATION RECOMMENDATIONS
# ============================================================================

cat("═══════════════════════════════════════════════\n")
cat("WARRANTY DIFFERENTIATION RECOMMENDATIONS\n")
cat("═══════════════════════════════════════════════\n\n")

# Create summary table
warranty_summary <- data.frame(
  Component = c("Battery", "IR Sensor", "Impact Sensor"),
  Pets_pvalue = c(
    format.pval(p_battery_pets),
    format.pval(p_ir_pets),
    format.pval(p_impact_pets)
  ),
  Pets_Effect = c(
    ifelse(p_battery_pets < 0.05, "Significant", "Not significant"),
    ifelse(p_ir_pets < 0.05, "Significant", "Not significant"),
    ifelse(p_impact_pets < 0.05, "Significant", "Not significant")
  ),
  Carpet_pvalue = c(
    format.pval(p_battery_carpet),
    format.pval(p_ir_carpet),
    format.pval(p_impact_carpet)
  ),
  Carpet_Effect = c(
    ifelse(p_battery_carpet < 0.05, "Significant", "Not significant"),
    ifelse(p_ir_carpet < 0.05, "Significant", "Not significant"),
    ifelse(p_impact_carpet < 0.05, "Significant", "Not significant")
  ),
  stringsAsFactors = FALSE
)

print(warranty_summary)

cat("\n")
cat("INTERPRETATION:\n")
cat("───────────────────────────────────────────────\n\n")

cat("PETS EFFECT:\n")
if (p_battery_pets < 0.05) {
  cat("  • Battery: Pets significantly affect survival\n")
}
if (p_ir_pets < 0.05) {
  cat("  • IR Sensor: Pets significantly affect survival\n")
}
if (p_impact_pets < 0.05) {
  cat("  • Impact Sensor: Pets significantly affect survival\n")
}

cat("\nCARPET EFFECT:\n")
if (p_battery_carpet < 0.05) {
  cat("  • Battery: Carpet coverage significantly affects survival\n")
}
if (p_ir_carpet < 0.05) {
  cat("  • IR Sensor: Carpet coverage significantly affects survival\n")
  cat("  • IR Sensor: Carpet coverage significantly affFects survival\n")
}
if (p_impact_carpet < 0.05) {
  cat("  • Impact Sensor: Carpet coverage significantly affects survival\n")
}

cat("\nRECOMMENDATIONS:\n")
cat("───────────────────────────────────────────────\n")

# Battery recommendations
cat("\n1. BATTERY:\n")
if (p_battery_pets < 0.05 || p_battery_carpet < 0.05) {
  cat("   Consider differentiated warranties based on:\n")
  if (p_battery_pets < 0.05) cat("     - Pet ownership\n")
  if (p_battery_carpet < 0.05) cat("     - Carpet coverage\n")
} else {
  cat("   Standard warranty terms sufficient\n")
}

# IR Sensor recommendations
cat("\n2. IR SENSOR:\n")
if (p_ir_pets < 0.05 || p_ir_carpet < 0.05) {
  cat("   Consider differentiated warranties based on:\n")
  if (p_ir_pets < 0.05) cat("     - Pet ownership\n")
  if (p_ir_carpet < 0.05) cat("     - Carpet coverage\n")
} else {
  cat("   Standard warranty terms sufficient\n")
}

# Impact Sensor recommendations
cat("\n3. IMPACT SENSOR:\n")
if (p_impact_pets < 0.05 || p_impact_carpet < 0.05) {
  cat("   Consider differentiated warranties based on:\n")
  if (p_impact_pets < 0.05) cat("     - Pet ownership\n")
  if (p_impact_carpet < 0.05) cat("     - Carpet coverage\n")
} else {
  cat("   Standard warranty terms sufficient\n")
}