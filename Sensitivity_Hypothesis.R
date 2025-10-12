
#forthe dmy function
library(lubridate)
#for the analysing survival
library(survival)
#for pretty plotting of Survival Functions
library(survminer)
library (dplyr)

file_path="D:\\TUE Study Material\\Q1\\Survival Analysis for Data Scientists\\GA_17\\Survival-Analysis-DirtSlurper3100-GA\\data_preprocessed.csv"
# Read data
data <- read.table(file_path,
                   header = TRUE,
                   sep = ",",
                   stringsAsFactors = FALSE)

#--------------------------------------Same as in KM estimator---------------------------------#
#31 dec 2019
study_end <- ("2019-12-31")
failuredate <- as.Date(data$Failure.date,format="%Y-%m-%d")
reg <-as.Date(data$Registration.date,format="%Y-%m-%d")
##Changing the convention i.e 1 for failure and 0 for censored
#failuredate-reg=working time, if failure not given means vacuum working
#so then study_end -reg=working time(handles ok values)
# setting up parameters for Battery 
time_battery <- ifelse(!is.na(failuredate) & data$Battery.status == 1,
                       as.numeric(difftime(failuredate, reg, units="days")),
                       as.numeric(difftime(study_end, reg, units="days")))
event_battery <- ifelse(data$Battery.status == 1, 1, 0)


# setting up parameters for Impact Sensor
time_impactsensor <- ifelse(!is.na(failuredate) & data$Impact.status == 1,
                            as.numeric(difftime(failuredate, reg, units="days")),
                            as.numeric(difftime(study_end, reg, units="days")))
event_impactsensor <- ifelse(data$Impact.status == 1, 1, 0)

# setting up parameters for INFRARED Sensor
time_infraredsensor <- ifelse(!is.na(failuredate) & data$IR.status == 1,
                              as.numeric(difftime(failuredate, reg, units="days")),
                              as.numeric(difftime(study_end, reg, units="days")))
event_infraredsensor <- ifelse(data$IR.status == 1, 1, 0)

# setting up parameters for analysing survival of Whole Vacuum (fails if ANY component fails) 
event_vacuum <- ifelse((data$Battery.status == 1 |
                          data$Impact.status == 1 |
                          data$IR.status == 1), 1, 0)

time_vacuum <- ifelse(event_vacuum == 1 & !is.na(failuredate),
                      as.numeric(difftime(failuredate, reg, units="days")),
                      as.numeric(difftime(study_end, reg, units="days")))

#-------------------------------------------------------------------------------#
#Sensitivity of Components wrt to Extreme usage
#Because extreme usage has an overall impact

#I set 2400 hrs as threshold
threshold=2400

data_sens <- data %>%
  mutate(extreme_use = ifelse(Total.usage.time >= threshold, 1, 0))

# Using data_sens here,
km_full <- survfit(Surv(time_vacuum, event_vacuum) ~ 1, data = data_sens)
km_full
km_no_extreme <- survfit(Surv(time_vacuum, event_vacuum) ~ 1, data = filter(data_sens, extreme_use == 0))
km_no_extreme
library(ggpubr)
p1 <- ggsurvplot(km_full, conf.int = TRUE, ggtheme = theme_minimal(), title = "Full Data")
p2 <- ggsurvplot(km_no_extreme, conf.int = TRUE, ggtheme = theme_minimal(), title = "Without Extreme Use")

# ✅ Merge plots into a grid
ggarrange(p1$plot, p2$plot, ncol = 2, nrow = 1)

# can be answer for inference
#Extreme users are NOT driving most failures.The failure events are spread 
#across general users, not dominated by heavy usage.This suggests that the component 
#(e.g., IR sensor, battery, etc.) is failing under normal operating conditions, not only in harsh environments.
#Therefore, management concern about general underperformance is justified, 
#because we cannot blame just extreme users.

#but to be sure another test needs to be done like log rank


#-------------Goal is Environment Specific Warranty for components required?------#
#Sensitivity of Components wrt to Pets


#Infrared Sensor only as doesn't make sense for Battery and Impact Sensor be analysed on the same
km_pets <- survfit(Surv(time_infraredsensor, event_infraredsensor) ~ data$Pets, data = data)
km_pets
ggsurvplot(km_pets, conf.int = TRUE,
           xlab = "Days", ylab = "Survival Probability",
           legend.labs = c("No Pets", "Pets"),
           ggtheme = theme_minimal(), data=data, title="Analysing Senstivity of Infrared Sensor wrt to Pets")
# NA values means very less failures and means Pets have nor impact so no Animal Specific Warranty needed for IR sensor

km_pets_b <- survfit(Surv(time_battery, event_battery) ~ data$Pets, data = data)
km_pets_b
ggsurvplot(km_pets_b, conf.int = TRUE,
           xlab = "Days", ylab = "Survival Probability ",
           legend.labs = c("No Pets", "Pets"),
           ggtheme = theme_minimal(), data=data, title="Analysing Senstivity of Battery wrt to Pets")
# NA values means very less failures and means Pets have nor impact so no Animal Specific Warranty needed for Battery

km_pets_impact <- survfit(Surv(time_impactsensor, event_impactsensor) ~ data$Pets, data = data)
km_pets_impact
ggsurvplot(km_pets_impact, conf.int = TRUE,
           xlab = "Days", ylab = "Survival Probability ",
           legend.labs = c("No Pets", "Pets"),
           ggtheme = theme_minimal(), data=data, title="Analysing Senstivity of Impact Sensor wrt to Pets")


#Infrared has no relation
data <- data %>%
  mutate(carpet_group = case_when(
    Carpet.score <= 3 ~ "Low",
    Carpet.score <= 6 ~ "Medium",
    TRUE ~ "High"
  ))

km_carpet <- survfit(Surv(time_infraredsensor, event_infraredsensor) ~ carpet_group, data = data)

ggsurvplot(km_carpet, conf.int = TRUE,
           xlab = "Days", ylab = "Survival Probability",
           legend.title = "Carpet Level",
           ggtheme = theme_minimal(), data=data)
#Infrared has no relation w.r.t to carpet
data <- data %>%
  mutate(carpet_group = case_when(
    Carpet.score <= 3 ~ "Low",
    Carpet.score <= 6 ~ "Medium",
    TRUE ~ "High"
  ))
#Impact sensor has no relation
km_carpet <- survfit(Surv(time_impactsensor, event_impactsensor) ~ carpet_group, data = data)
km_carpet
ggsurvplot(km_carpet, conf.int = TRUE,
           xlab = "Days", ylab = "Survival Probability",
           legend.title = "Carpet Level",
           ggtheme = theme_minimal(), data=data)
#battery
#Carpet impacts,  has impact on battery
km_carpet <- survfit(Surv(time_battery, event_battery) ~ carpet_group, data = data)

ggsurvplot(km_carpet, conf.int = TRUE,
           xlab = "Days", ylab = "Survival Probability",
           legend.title = "Carpet Level",
           ggtheme = theme_minimal(), data=data)

#We can draw that no environment specific warranty required the intensity of usage 
#impacts battery component specifically, but usage not a reason for failure this implies battery 
#fails due to a different reason. Batteries are not failing randomly ("out of the blue"), but fail 
#due to environmental stress (like high carpet resistance) rather than due to purely high usage volume.
#Total usage hours alone does not capture mechanical loading — operating in high friction environments (high carpet score)
#increases current draw → accelerates battery degradation even at moderate usage.
#This maybe a reason that device is failing more than expected. 


#-------------------------Verifying vacuum performance wrt to carpet score------------#
# 
data <- data %>%
  mutate(carpet_group = case_when(
    Carpet.score <= 3 ~ "Low",
    Carpet.score <= 6 ~ "Medium",
    TRUE ~ "High"
  ))

# Survival Object for Battery
surv_batt <- Surv(time_vacuum, event_vacuum)
km_batt_carpet <- survfit(surv_batt ~ carpet_group, data = data)
ggsurvplot(
  km_batt_carpet,
  data = data,
  conf.int = TRUE,
  pval = TRUE,                     # Shows p-value from log-rank test
  risk.table = TRUE,              # Displays number at risk table
  xlab = "Time (Days)",
  ylab = "Survival Probability",
  legend.title = "Carpet Environment",
  legend.labs = c("Low", "Medium", "High"),
  ggtheme = theme_minimal()
)
#----------------Hypothesis---------------------------#
#For infrared sensor
#One-sided test on the KM survival at time = 2000 using Greenwood variance.
#ho=Claim of manufacturer true(L10>=2000 days)ie. S(2000>=0.90)
#h1=Claim of Manufcaturer false (L10<2000 days)S(2000<0.90)
km_ir <- survfit(Surv(time_infraredsensor, event_infraredsensor) ~ 1, data = data)

# helper to get S_hat and SE at time t
get_surv_at <- function(km, t) {
  s <- summary(km, times = t, extend = TRUE)
  # summary returns a list; s$surv and s$std.err if available
  S_hat <- ifelse(length(s$surv)==0, NA, s$surv)
  SE <- ifelse(length(s$std.err)==0, NA, s$std.err)
  list(S = S_hat, SE = SE)
}

t0 <- 2000
res <- get_surv_at(km_ir, t0)
S_hat <- res$S
SE <- res$SE

# Z-test (one-sided: H0: S >= 0.9 vs H1: S < 0.9)
z <- (S_hat - 0.90) / SE
p_value_one_sided <- pnorm(z)  # lower-tail p-value

list(t = t0, S_hat = S_hat, SE = SE, z = z, p_one_sided = p_value_one_sided)
#Claim of manufacturer holds for IR as p value>=0.50 so null hypothesis correct


#----------Hypothesis For Battery based inference
#For Battery
#Ho:Battery that is not being used intensively i.e more than 2400 hrs lasts atleast 1000 days
#H1:Battery that is not being used intensively i.e more than 2400 hrs does not last 1000 days

time_battery2400 <- ifelse(!is.na(failuredate) & data$Total.usage.time<=2400 & data$Battery.status == 1 ,
                           as.numeric(difftime(failuredate, reg, units="days")),
                           as.numeric(difftime(study_end, reg, units="days")))
event_battery2400 <- ifelse(data$Battery.status == 1, 1, 0)

km_battery2400 <- survfit(Surv(time_battery2400, event_battery2400) ~ 1, data = data)

# helper to get S_hat and SE at time t
get_surv_at <- function(km, t) {
  s <- summary(km, times = t, extend = TRUE)
  # summary returns a list; s$surv and s$std.err if available
  S_hat <- ifelse(length(s$surv)==0, NA, s$surv)
  SE <- ifelse(length(s$std.err)==0, NA, s$std.err)
  list(S = S_hat, SE = SE)
}

t0 <- 1000
res <- get_surv_at(km_battery2400, t0)
S_hat <- res$S
SE <- res$SE

# Z-test (one-sided: H0: S >= 0.9 vs H1: S < 0.9)
z <- (S_hat - 0.90) / SE
p_value_one_sided <- pnorm(z)  # lower-tail p-value

list(t = t0, S_hat = S_hat, SE = SE, z = z, p_one_sided = p_value_one_sided)
#Claim of manufacturer holds for Batteries as p value>=0.50 so null hypothesis correct


#--------------------Claim of Impact Sensor-------------------#
#No claim just check
km_impactsensor <- survfit(Surv(time_impactsensor, event_impactsensor) ~ 1, data = data)

# helper to get S_hat and SE at time t
get_surv_at <- function(km, t) {
  s <- summary(km, times = t, extend = TRUE)
  # summary returns a list; s$surv and s$std.err if available
  S_hat <- ifelse(length(s$surv)==0, NA, s$surv)
  SE <- ifelse(length(s$std.err)==0, NA, s$std.err)
  list(S = S_hat, SE = SE)
}

t0 <- 2000
res <- get_surv_at(km_impactsensor, t0)
S_hat <- res$S
SE <- res$SE

# Z-test (one-sided: H0: S >= 0.9 vs H1: S < 0.9)
z <- (S_hat - 0.90) / SE
p_value_one_sided <- pnorm(z)  # lower-tail p-value

list(t = t0, S_hat = S_hat, SE = SE, z = z, p_one_sided = p_value_one_sided)
#Claim made none holds but for Impact Sensor our analysis holds  as p value>=0.50 

