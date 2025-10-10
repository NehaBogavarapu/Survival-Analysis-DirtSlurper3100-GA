
#forthe dmy function
library(lubridate)
#for the analysing survival
library(survival)
#for pretty plotting of Survival Functions
library(survminer)


file_path="D:\\TUE Study Material\\Q1\\Survival Analysis for Data Scientists\\GA_17\\Survival-Analysis-DirtSlurper3100-GA\\data_preprocessed.csv"
# Read data
data <- read.table(file_path,
                   header = TRUE,
                   sep = ",",
                   stringsAsFactors = FALSE)
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

# Kaplan Mier Estimation
km_battery <- survfit(Surv(time_battery, event_battery) ~ 1, data = data)
km_impactsensor  <- survfit(Surv(time_impactsensor, event_impactsensor) ~ 1, data = data)
km_infraredsensor   <- survfit(Surv(time_infraredsensor, event_infraredsensor) ~ 1, data = data)
km_vacuum  <- survfit(Surv(time_vacuum, event_vacuum) ~ 1)


# KM Plots for all the components as well as the whole vacuum
ggsurvplot(km_battery, data = data, conf.int=TRUE, title="Battery Survival", xlab="Days", ylab="Survival Probability")
ggsurvplot(km_impactsensor, data = data, conf.int=TRUE, title="Impact Sensor Survival", xlab="Days", ylab="Survival Probability")
ggsurvplot(km_infraredsensor, data = data, conf.int=TRUE, title="Infrared Sensor Survival", xlab="Days", ylab="Survival Probability")
ggsurvplot(km_vacuum, data = data, conf.int=TRUE, title="Overall Vacuum Survival", xlab="Days", ylab="Survival Probability")



# ---  Plot all three survival curves together ---
plot(km_vacuum, col = "black", lwd = 2, conf.int = FALSE,
     xlab = "Days", ylab = "Survival Probability",
     main = "Kaplan–Meier Survival Estimates",xlim = c(0, max(time_vacuum)),ylim = c(0, 1)) 
lines(km_battery, col = "blue", lwd = 2)
lines(km_impactsensor, col = "red", lwd = 2)
lines(km_infraredsensor, col = "green", lwd = 2)
legend("bottomright", legend = c("Overall Device", "Battery", "Impact Sensor","Infrared Sensor"),
       col = c("black", "blue", "red","green"), lwd = 2, bty = "n")


# Survival probabilities, Error and Confidence Intervals  at 500, 1000, 1500 days
#Max time is close to somewhere 1800 something 
summary(km_battery, times=c(500,1000,1500))
summary(km_impactsensor, times=c(500,1000,1500))
summary(km_infraredsensor, times=c(500,1000,1500))
summary(km_vacuum, times=c(500,1000,1500))


#----------------------------------------------------------------------------#


#Analysing Inference two(a)
# setting up parameters for Battery that runs at most 2400 hours
time_battery2400 <- ifelse(!is.na(failuredate) & data$Total.usage.time<=2400 & data$Battery.status == 1 ,
                           as.numeric(difftime(failuredate, reg, units="days")),
                           as.numeric(difftime(study_end, reg, units="days")))
event_battery2400 <- ifelse(data$Battery.status == 1, 1, 0)

# Kaplan Mier Estimation
km_battery2400 <- survfit(Surv(time_battery2400, event_battery2400) ~ 1, data = data)

# KM Plot for battery that runs less than 2400 hours
ggsurvplot(km_battery2400, data = data, conf.int=TRUE, title="Survival of Batteries that run <=2400 hours", xlab="Days", ylab="Survival Probability")

# Survival probabilities, Error and Confidence Intervals  at 500, 1000, 1500 days
summary(km_battery2400, times=c(500,1000,1500))

# Extract survival probabilities and corresponding times
km_summary <- summary(km_battery2400)
surv_probs <- km_summary$surv
surv_times <- km_summary$time

# Using this exFind the time where survival drops below or equal to 0.10 (i.e., 90% quantile)
#quantile_90 <- min(surv_times[surv_probs <= 0.10])

# Find the earliest time when survival drops to or below 0.90
quantile_10 <- min(surv_times[surv_probs <= 0.90])
# Print the result
cat("Estimated 10% quantile for battery that works less than 2400 hrs :", quantile_10, "\n")
#Ans is 1001 days so yes it works well
#--------------------------------------------------------------------------------------#
#Analysing Inference two(b)
#Battery that works for greater than 2400 hours sent for repair or not
#Hypothesis that every time it fails sent for repair, and when OK it isnot sent for repair
g2400batterysent <- ifelse(data$Total.usage.time>=2400 ,data$Sent.for.repair,NA)
g2400battery_ok=sum(g2400batterysent==0,na.rm=TRUE)
g2400battery_fail=sum(g2400batterysent==1,na.rm=TRUE)

ok_notsent <- c(g2400battery_ok)
fail_sent <- c(g2400battery_fail)

bp<-barplot(rbind(ok_notsent, fail_sent),
            beside = TRUE,
            col = c("blue", "orange"),
            names.arg = c("Battery"),
            main = "Repair status of Components",
            xlab = "Components of Vacuum",
            ylab = "Count")
legend("topleft",
       legend = c("Not Sent", "Sent"),
       fill = c("blue", "orange"),
       inset = c(-0.07, -0.07),bty = "n")  
# Adding text on top of bars
text(x = bp, 
     y = rbind(ok_notsent, fail_sent), 
     labels = rbind(ok_notsent, fail_sent), 
     pos = 1)  
#pos# Determines position of text to the top of the bar
#So there are 228 such batteries with charge less than 80%
#Analysis Inference Two can be done with the fist ig
#---------------------------------------------------------------------------#

# Extract survival probabilities and corresponding times
km_summary <- summary(km_infraredsensor)
surv_probs <- km_summary$surv
surv_times <- km_summary$time

# Using this exFind the time where survival drops below or equal to 0.10 (i.e., 90% quantile)
#quantile_90 <- min(surv_times[surv_probs <= 0.10])

# Find the earliest time when survival drops to or below 0.90
quantile_10 <- min(surv_times[surv_probs <= 0.90])
# Print the result
cat("Estimated 10% quantile fro infrared Sensor :", quantile_10, "\n") #ans is infinity so yes
#This means less than 10% of the IR sensors actually failed within the follow-up window.
#Hence, we cannot estimate L10 from KM, because there aren't enough failures to reach that 10% failure threshold.
#for 0.95 there are 1059 days
#-------------------------------------------------------------#
# Overall Extract survival probabilities and corresponding times of battery overall
km_summary <- summary(km_battery)
surv_probs <- km_summary$surv
surv_times <- km_summary$time
# Find the earliest time when survival drops to or below 0.90
quantile_10 <- min(surv_times[surv_probs <= 0.90])
# Print the result
cat("Estimated 10% quantile :", quantile_10, "\n")
# Our estimate that parts survive 936 days (general not asked anywhere)
#--------------------------------------------------------------#
  # Overall Extract survival probabilities and corresponding times of impact sensor
  #inf at 90
km_summary <- summary(km_impactsensor)
surv_probs <- km_summary$surv
surv_times <- km_summary$time
# Find the earliest time when survival drops to or below 0.90
quantile_5 <- min(surv_times[surv_probs <= 0.95])
# Print the result
cat("Estimated 5% quantile :", quantile_5, "\n")
#Works very well and can last 1615 days L95 i.e ~1600 days.