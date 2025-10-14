library(survival)
library(survminer)
library(tidyverse)
library(boot)

file_path <- "C:\\Users\\Gourisha Verma\\OneDrive\\Documents\\GitHub\\Survival-Analysis-DirtSlurper3100-GA\\DirtSlurper3100.csv"

raw <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, skip = 12)

colnames(raw) <- raw[1, ]
og_data <- raw[-1, ]
colnames(og_data) <- make.names(trimws(colnames(og_data)), unique = TRUE)
og_data[] <- lapply(og_data, trimws)

Sys.setlocale("LC_TIME", "C")  

data_eda <- og_data %>%
  mutate(
    Registration.date = as.Date(Registration.date, format = "%d-%b-%y"),
    Failure.date = if_else(Failure.date == "---", NA, Failure.date),
    Failure.date = as.Date(Failure.date, format = "%d-%b-%y"),
    Failure.date = if_else(is.na(Failure.date), as.Date("2019-12-31"), Failure.date),

    Sent.for.repair = as.numeric(Sent.for.repair == "YES"),
    Pets = as.numeric(Pets == "YES"),

    Battery.status = if_else(Battery.status == "---", "OK", Battery.status),
    IR.status = if_else(IR.status == "---", "OK", IR.status),
    Impact.status = if_else(Impact.status == "---", "OK", Impact.status),
    Battery.status = as.numeric(Battery.status == "Damage"),
    IR.status = as.numeric(IR.status == "Damage"),
    Impact.status = as.numeric(Impact.status == "Damage"),

    Total.usage.time = as.numeric(Total.usage.time),
    Carpet.score = as.numeric(Carpet.score),

    Possession.time = as.numeric(difftime(Failure.date, Registration.date, units = "hours"))
  ) %>%
  filter(!is.na(Possession.time))

# Drop remaining NAs
data_eda <- na.omit(data_eda)

# Convert to factors for categorical variables
data_eda <- data_eda %>%
  mutate(
    Pets = factor(Pets, levels = c(0, 1), labels = c("No", "Yes")),
    Sent.for.repair = factor(Sent.for.repair, levels = c(0, 1), labels = c("No", "Yes")),
    Battery.status = factor(Battery.status, levels = c(0, 1), labels = c("OK", "Damage")),
    IR.status = factor(IR.status, levels = c(0, 1), labels = c("OK", "Damage")),
    Impact.status = factor(Impact.status, levels = c(0, 1), labels = c("OK", "Damage"))
  )

#Histogram Plot
print(
  ggplot(data_eda, aes(x = Total.usage.time, fill = Sent.for.repair)) +
    geom_histogram(bins = 30, color = "black") +
    labs(title = "Distribution of Total Usage Time", x = "Total Usage Time (hours)", y = "Count") +
    scale_fill_manual(values = c("cadetblue", "coral")) +
    theme_minimal()
)

#Cox Proportional Hazards Model
# surv_obj <- Surv(time = data_eda$Possession.time, event = data_eda$Sent.for.repair == "Yes")
# cox_model <- coxph(surv_obj ~ Battery.status + IR.status + Impact.status + Pets + Carpet.score, data = data_eda)
# summary(cox_model)

# data_eda$Sent.for.repair_num <- ifelse(data_eda$Sent.for.repair == "Yes", 1, 0)

# #Cox-Snell Residuals
# cox_snell <- residuals(cox_model, type = "martingale") + data_eda$Sent.for.repair_num
# fit_resid <- survfit(Surv(cox_snell, data_eda$Sent.for.repair_num) ~ 1)

# resid_df <- data.frame(
#   time = fit_resid$time,
#   cumhaz = -log(fit_resid$surv)
# )

# # Plot Cox-Snell residuals
# print(
#   ggplot(resid_df, aes(x = time, y = cumhaz)) +
#     geom_step(color = "blue") +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
#     labs(title = "Cox-Snell Residuals Plot", x = "Cox-Snell Residuals", y = "Cumulative Hazard") +
#     theme_minimal()
# )

library(survival)
library(ggplot2)

# Make sure the event is numeric
event_num <- as.numeric(data_eda$Sent.for.repair == "Yes")

# Survival object
surv_obj <- Surv(time = data_eda$Possession.time, event = event_num)

# Fit Cox model
cox_model <- coxph(
  surv_obj ~ Pets + Carpet.score,
  data = data_eda
)

# Cox-Snell residuals (numeric!)
cox_snell <- -residuals(cox_model, type = "martingale") + event_num

# Fit survival curve on residuals
fit_resid <- survfit(Surv(cox_snell, event_num) ~ 1)

# Prepare data for ggplot
resid_df <- data.frame(
  time = fit_resid$time,
  cumhaz = -log(fit_resid$surv)
)

p <- ggplot(resid_df, aes(x = time, y = cumhaz)) +
  geom_step(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Cox-Snell Residuals Plot",
    x = "Cox-Snell Residuals",
    y = "Cumulative Hazard"
  ) +
  theme_minimal()

print(p)


# Var Independence Checks
# Continuous vs Continuous
cor_test <- cor(data_eda$Total.usage.time, data_eda$Possession.time, use = "complete.obs")
print(paste("Correlation (Total usage vs Possession time):", cor_test))

# Categorical vs Categorical
table_pets_battery <- table(data_eda$Pets, data_eda$Battery.status)
chisq_result <- chisq.test(table_pets_battery)
print(chisq_result)

#Bootstrap Approach for Cox Model Coeffs
cox_coef <- function(data, indices) {
  d <- data[indices, ]
  surv_obj <- Surv(d$Possession.time, d$Sent.for.repair == "Yes")
  fit <- coxph(surv_obj ~ Battery.status + IR.status + Impact.status + Pets + Carpet.score, data = d)
  return(coef(fit))
}

set.seed(123)
boot_results <- boot(data = data_eda, statistic = cox_coef, R = 1000)

# 95% CI for first coeff
boot.ci(boot_results, type = "perc", index = 1)