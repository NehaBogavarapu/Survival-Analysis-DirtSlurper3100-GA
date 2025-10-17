library(survival)
library(survminer)
library(tidyverse)
library(boot)
library(ggplot2)
library(patchwork)

<<<<<<< HEAD
# ===========================
# LOAD PRE-PROCESSED DATA
# ===========================
file_path <- "C:\\Users\\Gourisha Verma\\OneDrive\\Documents\\GitHub\\Survival-Analysis-DirtSlurper3100-GA\\data_preprocessed.csv"
file_path <- "D:\\TUE Study Material\\Q1\\Survival Analysis for Data Scientists\\GA_17\\Survival-Analysis-DirtSlurper3100-GA\\data_preprocessed.csv"
=======
# Loading the pre-processed data
file_path <- "data_preprocessed.csv"
>>>>>>> 6817dd7b3380470e84f0aced5009d2705efb855a
data_eda <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Make sure columns are in numeric form ,Possession.time in days, Total.usage.time in hours, Sent.for.repair and indicators are 0/1
# Pets is also 0/1
str(data_eda)

# Full Cox Model
# event = Sent.for.repair (0/1)
<<<<<<< HEAD
# predictors = Pets, Carpet.score, Total.usage.time
# ===========================

=======
# predictors are Pets, Carpet.score, Total.usage.time
>>>>>>> 6817dd7b3380470e84f0aced5009d2705efb855a
surv_obj_sent <- Surv(time = data_eda$Possession.time, event = data_eda$Sent.for.repair)

cox_model_sent <- coxph(surv_obj_sent ~ Pets + Carpet.score + Total.usage.time, data = data_eda)
summary(cox_model_sent)

# Martingale residuals to make our cox snell computation easier 
mart_sent <- residuals(cox_model_sent, type = "martingale")

# Cox-Snell residuals
cox_snell_sent <- -mart_sent + data_eda$Sent.for.repair

<<<<<<< HEAD
# 2. Survival curves for covariate profiles — Pets = 0 vs Pets = 1
newdata_profiles <- data.frame(
  Pets = c(0, 1),
  Pets = c(0, 1), #pets
  Carpet.score = c(5, 5),
  Total.usage.time = c(100, 100)
=======
# Fit cumulative hazard for Cox-Snell residuals
fit_resid_sent <- survfit(Surv(cox_snell_sent, data_eda$Sent.for.repair) ~ 1)

resid_df_sent <- data.frame(
  time = fit_resid_sent$time,
  cumhaz = -log(fit_resid_sent$surv)
>>>>>>> 6817dd7b3380470e84f0aced5009d2705efb855a
)

# Plot Cox-Snell residuals
p_sent_coxsnell <- ggplot(resid_df_sent, aes(x = time, y = cumhaz)) +
  geom_step() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Cox-Snell Residuals - Sent for Repair (Full Cox Model)",
    x = "Cox-Snell residual",
    y = "Estimated cumulative hazard"
  ) +
  theme_minimal()

print(p_sent_coxsnell)

<<<<<<< HEAD
# Survival curves for Total.usage.time — low vs high (e.g., 50 vs 300)
newdata_usage <- data.frame(
  Pets = c(0, 0),                  # pets fixed
  Carpet.score = c(5, 5),          # carpet fixed
  Total.usage.time = c(50, 300)    # vary usage
  Total.usage.time = c(50, 2400)    # vary usage
)

fit_usage <- survfit(cox_model_sent, newdata = newdata_usage)
p_usage <- ggsurvplot(
  fit_usage,
  data = newdata_usage,
  conf.int = TRUE,
  ggtheme = theme_minimal(),
  title = "Survival Curves by Total Usage Time",
  legend.title = "Usage Time",
  legend.labs = c("Low (50h)", "High (300h)")
)
print(p_usage)

fit_profiles <- survfit(cox_model_sent, newdata = newdata_profiles)
p_profiles <- ggsurvplot(
  fit_profiles,
  data = newdata_profiles,
  conf.int = TRUE,
  ggtheme = theme_minimal(),
  title = "Survival Curves by Pet Ownership",
  legend.title = "Pets",
  legend.labs = c("No Pets", "Has Pets")
)
print(p_profiles)

# 3. Forest plot for hazard ratios
=======
# Forest plot for hazard ratios
>>>>>>> 6817dd7b3380470e84f0aced5009d2705efb855a
p_forest <- ggforest(
  model = cox_model_sent,
  data = data_eda,
  main = "Hazard Ratios - Full Cox Model (Sent for Repair)",
  cpositions = c(0.02, 0.22, 0.4),
  fontsize = 1
)
print(p_forest)

# Cox Snell Residuals for indicator events:
# Battery.status, IR.status, Impact.status
indicator_vars <- c("Battery.status", "IR.status", "Impact.status")
plot_list <- list()

for (v in indicator_vars) {
  event_indicator <- data_eda[[v]]  # already 0/1
  
  # Fit Cox model with same covariates, different event
  dtmp <- data_eda %>% mutate(.event = event_indicator)
  cox_evt <- coxph(Surv(Possession.time, .event) ~ Pets + Carpet.score + Total.usage.time, data = dtmp)
  print(summary(cox_evt))
  
  # Cox-Snell residuals
  mart <- residuals(cox_evt, type = "martingale")
  cox_snell <- -mart + event_indicator
  
  fit_resid <- survfit(Surv(cox_snell, event_indicator) ~ 1)
  resid_df <- data.frame(
    time = fit_resid$time,
    cumhaz = -log(fit_resid$surv)
  )
  
  p <- ggplot(resid_df, aes(x = time, y = cumhaz)) +
    geom_step() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      title = paste("Cox-Snell Residuals (event =", v, ")"),
      x = "Cox-Snell residual",
      y = "Estimated cumulative hazard"
    ) +
    theme_minimal()
  
  plot_list[[v]] <- p
}

combined_plot <- wrap_plots(plot_list, ncol = 2)
print(combined_plot)

# Variable Independence Checks
cor_test <- cor(data_eda$Total.usage.time, data_eda$Possession.time, use = "complete.obs")
cat("\nCorrelation (Total usage vs Possession time):", cor_test, "\n")

chisq_result <- chisq.test(table(data_eda$Pets, data_eda$Battery.status))
print(chisq_result)

# Bootstrap Confidence Approach for Sent.for.repair COX model
cox_coef_sent <- function(data, indices){
  d <- data[indices, ]
  surv_obj <- Surv(d$Possession.time, d$Sent.for.repair)
  fit <- coxph(surv_obj ~ Pets + Carpet.score + Total.usage.time, data = d)
  coef(fit)
}

set.seed(123)
boot_results <- boot(data = data_eda, statistic = cox_coef_sent, R = 1000)

ncoefs <- length(coef(cox_model_sent))
for (i in 1:ncoefs){
  cat("\n95% Bootstrap CI for coefficient", i, ":\n")
  print(boot.ci(boot_results, type = "perc", index = i))
}
