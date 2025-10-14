library(survival)
library(survminer)
library(tidyverse)
library(boot)
library(ggplot2)

file_path <- "C:\\Users\\Gourisha Verma\\OneDrive\\Documents\\GitHub\\Survival-Analysis-DirtSlurper3100-GA\\data_preprocessed.csv"
raw <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)

og_data <- raw[-1, ]
colnames(og_data) <- make.names(trimws(colnames(og_data)), unique = TRUE)
og_data[] <- lapply(og_data, trimws)

Sys.setlocale("LC_TIME", "C") 
data_eda <- og_data %>%
  mutate(
    Registration.date = as.Date(Registration.date, format = "%Y-%m-%d"),
    Failure.date = as.Date(Failure.date, format = "%Y-%m-%d"),
    
    Possession.time = as.numeric(difftime(Failure.date, Registration.date, units = "hours")),
    
    Sent.for.repair = as.numeric(Sent.for.repair == 1),
    Pets = as.numeric(Pets == 1),
    Battery.status = as.numeric(Battery.status == 1),
    IR.status = as.numeric(IR.status == 1),
    Impact.status = as.numeric(Impact.status == 1),
    Total.usage.time = as.numeric(Total.usage.time),
    Carpet.score = as.numeric(Carpet.score)
  ) %>%
  filter(!is.na(Possession.time)) %>%
  na.omit()

# Converting the categorical variables to factors
data_eda <- data_eda %>%
  mutate(
    Pets = factor(Pets, levels = c(0, 1), labels = c("No", "Yes")),
    Sent.for.repair = factor(Sent.for.repair, levels = c(0, 1), labels = c("No", "Yes")),
    Battery.status = factor(Battery.status, levels = c(0, 1), labels = c("OK", "Damage")),
    IR.status = factor(IR.status, levels = c(0, 1), labels = c("OK", "Damage")),
    Impact.status = factor(Impact.status, levels = c(0, 1), labels = c("OK", "Damage"))
  )

# Eda plot

p_hist <- ggplot(data_eda, aes(x = Total.usage.time, fill = Sent.for.repair)) +
  geom_histogram(bins = 30, color = "black") +
  labs(title = "Distribution of Total Usage Time", x = "Total Usage Time (hours)", y = "Count") +
  scale_fill_manual(values = c("cadetblue", "coral")) +
  theme_minimal()

print(p_hist)

# Cox Proportional Hazards Models
event_num <- as.numeric(data_eda$Sent.for.repair == "Yes")
surv_obj <- Surv(time = data_eda$Possession.time, event = event_num)

# Full Cox model
cox_model <- coxph(surv_obj ~ Battery.status + IR.status + Impact.status + Pets + Carpet.score, data = data_eda)
summary(cox_model)

# Single-variable cox models
vars <- c("Battery.status", "IR.status", "Impact.status", "Pets", "Carpet.score")
for (v in vars) {
  cat("\n--- Cox Model:", v, "---\n")
  formula <- as.formula(paste("Surv(Possession.time, Sent.for.repair == 'Yes') ~", v))
  print(summary(coxph(formula, data = data_eda)))
}

library(patchwork)
plot_list <- list()

# Looping through variables and creating residual plots
for (v in vars) {
  
  formula <- as.formula(paste("Surv(Possession.time, Sent.for.repair == 'Yes') ~", v))
  cox_model_single <- coxph(formula, data = data_eda)
  
  # Cox-Snell residuals
  cox_snell <- -residuals(cox_model_single, type = "martingale") + event_num
  fit_resid <- survfit(Surv(cox_snell, event_num) ~ 1)
  
  resid_df <- data.frame(
    time = fit_resid$time,
    cumhaz = -log(fit_resid$surv)
  )
  
  p <- ggplot(resid_df, aes(x = time, y = cumhaz)) +
    geom_step(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Cox-Snell Residuals:", v),
      x = "Cox-Snell Residuals",
      y = "Cumulative Hazard"
    ) +
    theme_minimal()
  
  plot_list[[v]] <- p
}

# Combining all plots in a grid
combined_plot <- wrap_plots(plot_list, ncol = 2)
print(combined_plot)

# Cox–Snell Residuals Plots
for (v in vars) {
  cat("\n--- Cox-Snell Residuals for", v, "---\n")
  
  formula <- as.formula(paste("Surv(Possession.time, Sent.for.repair == 'Yes') ~", v))
  cox_model_single <- coxph(formula, data = data_eda)
  
  # Cox-Snell residuals
  cox_snell <- -residuals(cox_model_single, type = "martingale") + event_num
  fit_resid <- survfit(Surv(cox_snell, event_num) ~ 1)
  
  resid_df <- data.frame(
    time = fit_resid$time,
    cumhaz = -log(fit_resid$surv)
  )
  
  p <- ggplot(resid_df, aes(x = time, y = cumhaz)) +
    geom_step(color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Cox-Snell Residuals Plot:", v),
      x = "Cox-Snell Residuals",
      y = "Cumulative Hazard"
    ) +
    theme_minimal()
  
  print(p)
}

# Variable Independence Checks
# Continuous vs Continuous
cor_test <- cor(data_eda$Total.usage.time, data_eda$Possession.time, use = "complete.obs")
cat("\nCorrelation (Total usage vs Possession time):", cor_test, "\n")

# Categorical vs Categorical
chisq_result <- chisq.test(table(data_eda$Pets, data_eda$Battery.status))
print(chisq_result)

# Bootstrap Confidence Intervals for Cox Coefficients
cox_coef <- function(data, indices){
  d <- data[indices, ]
  surv_obj <- Surv(d$Possession.time, d$Sent.for.repair == "Yes")
  fit <- coxph(surv_obj ~ Battery.status + IR.status + Impact.status + Pets + Carpet.score, data = d)
  coef(fit)
}

set.seed(123)
boot_results <- boot(data = data_eda, statistic = cox_coef, R = 1000)

# 95% CI for all coeffs
for (i in 1:5){
  cat("\n95% Bootstrap CI for coefficient", i, ":\n")
  print(boot.ci(boot_results, type = "perc", index = i))
}