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
weibull_battery <- survreg(Surv(Possession.time, Battery.status) ~ Pets + Carpet.score + Total.usage.time,
                           data = data, dist = "weibull")
weibull_impact <- survreg(Surv(Possession.time, Impact.status) ~ Pets + Carpet.score + Total.usage.time,
                          data = data, dist = "weibull")
weibull_ir <- survreg(Surv(Possession.time, IR.status) ~ Pets + Carpet.score + Total.usage.time,
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
exp_battery <- survreg(Surv(Possession.time, Battery.status) ~ Pets + Carpet.score + Total.usage.time,
                       data = data, dist = "exponential")
exp_impact <- survreg(Surv(Possession.time, Impact.status) ~ Pets + Carpet.score + Total.usage.time ,
                      data = data, dist = "exponential")
exp_ir <- survreg(Surv(Possession.time, IR.status) ~ Pets + Carpet.score + Total.usage.time,
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

<<<<<<< HEAD
=======
# Key Takeaways
# Non-convergence in Weibull for impact sensor suggests insufficient failure events (models require enough events for stable estimation)
# Pets consistently affect survival across models; Carpet score effect is small and sometimes negligible.
# Right censoring handled properly: devices without failure dates were treated as censored, which allows correct likelihood calculation.

# ============================================================================
# 1. LOG-RANK TEST - EXTREME USAGE EFFECT
# ============================================================================

cat("\n\n=== LOG-RANK TEST: EXTREME USAGE (>=2400 hours) ===\n")

# Create extreme usage indicator based on operational hours
data <- data %>%
  mutate(extreme_use = ifelse(Total.usage.time >= 2400, 
                              "Extreme (>=2400h)", 
                              "Normal (<2400h)"))

# Battery - using POSSESSION TIME as time scale
cat("\n--- BATTERY ---\n")
surv_battery <- Surv(data$Possession.time / 24, data$Battery.status)  # Convert to days
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
surv_ir <- Surv(data$Possession.time / 24, data$IR.status)
logrank_ir <- survdiff(surv_ir ~ extreme_use, data = data)
print(logrank_ir)
cat("p-value:", format.pval(1 - pchisq(logrank_ir$chisq, 1)), "\n")

# Impact Sensor
cat("\n--- IMPACT SENSOR ---\n")
surv_impact <- Surv(data$Possession.time / 24, data$Impact.status)
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
cat("--- BATTERY (Weibull) ---\n")
weibull_battery <- survreg(Surv(Possession.time / 24, Battery.status) ~ 
                             Total.usage.time + Pets + Carpet.score,
                           data = data, dist = "weibull")

# Batteries inference comparision to KM
predict(
  weibull_battery,
  newdata = data.frame(Pets = 1, Carpet.score = mean(data$Carpet.score, na.rm = TRUE)),
  type = "quantile",
  p = 0.1
)

# If this differs from KM it means:
  # Data might not be weibull shaped
  # KM estimates survival empirically only up to the largest observed event time.
  # If you have heavy right-censoring, the KM curve gets less precise at long times.
  # The Weibull model, using parametric assumptions, can extrapolate beyond where the data are sparse — often producing higher (or lower) L₁₀ estimates.
  # Covariate adjustment (?) - weibull is conditional on the covariates but km is marginal



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
weibull_ir <- survreg(Surv(Possession.time / 24, IR.status) ~ 
                        Total.usage.time + Pets + Carpet.score,
                      data = data, dist = "weibull")
summary(weibull_ir)

lambda_ir <- exp(coef(weibull_ir)[1])
k_ir <- 1 / weibull_ir$scale
cat("\nShape (k):", round(k_ir, 3), "\n")

# Impact Sensor - Try Weibull first
cat("\n--- IMPACT SENSOR ---\n")
weibull_impact <- tryCatch({
  survreg(Surv(Possession.time / 24, Impact.status) ~ 
            Total.usage.time + Pets + Carpet.score,
          data = data, dist = "weibull")
}, error = function(e) NULL)

if (is.null(weibull_impact) || any(is.na(coef(weibull_impact)))) {
  cat("Weibull failed. Using Exponential.\n")
  exp_impact <- survreg(Surv(Possession.time / 24, Impact.status) ~ 
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
weibull_battery_null <- survreg(Surv(Possession.time / 24, Battery.status) ~ 1,
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
exp_battery <- survreg(Surv(Possession.time / 24, Battery.status) ~ 
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

# ============================================================================
# 4. RESIDUAL ANALYSIS - DETAILED EXPLANATION
# ============================================================================

cat("\n\n=== RESIDUAL ANALYSIS - DETAILED ===\n")

analyze_residuals_detailed <- function(model, data, event_col, time_col, comp_name) {
  cat("\n" ," ═══════════════════════════════════════════════\n")
  cat("  ", comp_name, "RESIDUAL DIAGNOSTICS\n")
  cat("  ═══════════════════════════════════════════════\n\n")
  
  # Get model parameters
  lp <- predict(model, type = "lp")  # Linear predictor: X'β
  scale_param <- model$scale         # Scale parameter
  shape_param <- 1 / scale_param     # Shape = 1/scale for survreg
  event <- data[[event_col]]
  time_days <- data[[time_col]] / 24
  
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
      "(should be ≈1 if Exp(1))\n\n")
  
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
# >>>>>>> f752491f6ac575ce788b6054f7f464e9290a2fd0
