library(survival)
dat <- read.table("heroin.csv", header = TRUE, sep = ",")

# Plot Kaplan-Meier survival function of days from discharge from clinic to relapse:

addict.km <- survfit(Surv(time, status) ~ 1, conf.type = "none", type = "kaplan-meier", data = dat)
plot(addict.km, xlab = "Days to relapse", ylab = "S(t)")

# Kaplan-Meier survival function with confidence intervals:

addict.km <- survfit(Surv(time, status) ~ 1, type = "kaplan-meier", data = dat)
plot(addict.km, xlab = "Days to relapse", ylab = "S(t)", conf.int = TRUE)
 
# Kaplan-Meier survival function of days to relapse, stratifying by clinic:
  addict.km <- survfit(Surv(time, status) ~ clinic, type = "kaplan-meier", data = dat)
plot(addict.km, xlab = "Days to relapse", ylab = "S(t)", lty = c(1,2),
     legend.text = c("Clinic 1","Clinic 2"), legend.pos = 0, legend.bty = "n")

# Kaplan-Meier survival function of days to relapse, stratifying by methadone dose:

dat$newdose[dat$dose < 60] <- 0
dat$newdose[dat$dose >= 60] <- 1
addict.km <- survfit(Surv(time, status) ~ newdose, type = "kaplan-meier", data = dat)

# plot

plot(addict.km, xlab = "Days to relapse", ylab = "S(t)", lty = c(1,2),
     legend.text = c("Low dose methadone","High dose methadone"), legend.pos = 0,
     legend.bty = "n")

