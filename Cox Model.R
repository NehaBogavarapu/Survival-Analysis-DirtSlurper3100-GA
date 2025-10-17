library(survival)
library(survminer)
library(tidyverse)
library(boot)
library(ggplot2)
library(patchwork)

# Loading the pre-processed data
file_path <- "data_preprocessed.csv"
data_eda <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

# Make sure columns are in numeric form ,Possession.time in days, Total.usage.time in hours, Sent.for.repair and indicators are 0/1
# Pets is also 0/1
str(data_eda)

# Full Cox Model
# event = Sent.for.repair (0/1)
# predictors are Pets, Carpet.score, Total.usage.time
surv_obj_sent <- Surv(time = data_eda$Possession.time, event = data_eda$Sent.for.repair)

cox_model_sent <- coxph(surv_obj_sent ~ Pets + Carpet.score + Total.usage.time, data = data_eda)
summary(cox_model_sent)

# Martingale residuals to make our cox snell computation easier 
mart_sent <- residuals(cox_model_sent, type = "martingale")

# Cox-Snell residuals
cox_snell_sent <- -mart_sent + data_eda$Sent.for.repair

# Fit cumulative hazard for Cox-Snell residuals
fit_resid_sent <- survfit(Surv(cox_snell_sent, data_eda$Sent.for.repair) ~ 1)

resid_df_sent <- data.frame(
  time = fit_resid_sent$time,
  cumhaz = -log(fit_resid_sent$surv)
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

# Forest plot for hazard ratios
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
