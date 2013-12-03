#***************************************************************
#***************************************************************
#
#  Designs to improve numbers of Treated Liver Transplant Patients
#
#***************************************************************
#***************************************************************



#***************************************************************
#
#  Read in the data
#
#***************************************************************


r11donor <- read.table("R11donor.csv", sep = ",", header = T)

r11xplant <- read.table("R11xplant.csv", sep = ",", header = T)

uva <- read.table("UVAxplant.csv", sep = ",", header = T)

duke <- read.table("Dukexplant.csv", sep = ",", header = T)

mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

unc <- read.table("UNCxplant.csv", sep = ",", header = T)

#**********************************************

source("Transplant.plots.R")
source("TSbootfunctions.R")
library(boot)
library(forecast)


#**********************************************


#*********************************************
#  
#		Plotting Liver Transplants
#
#*********************************************

# Region plot

region.plot(cbind(r11xplant$Liver[-26], r11donor$Liver[-26], uva$Liver[-26], unc$Liver[-26], mcv$Liver[-26], duke$Liver[-26]),Year = seq(1988,2012), title = "Liver Transplants")

# Center plot
center.plot(cbind(uva$Liver[-26], unc$Liver[-26], mcv$Liver[-26], duke$Liver[-26]), Year = seq(1988,2012), title = "Liver Transplants")

##What do you observe in the center plot?  How is UVA compared to other centers in Liver transplants?

plot(seq(1988,2012), uva$Liver[-26], type = "l", col = "orange", ylim = c(min(mcv$Liver),max(uva$Liver)), xlab = "Years", ylab = "Transplants")
lines(seq(1988,2012), mcv$Liver[-26], col = "black")
lines(seq(1988,2012), duke$Liver[-26], col = "blue3")

legend(1990,85, legend = c("UVA", "MCV", "Duke"), col = c("orange", "black", "blue3"), lwd = 2)

#*********************************************
#  
#			Transplant Center
#		Classical Tests for Effectiveness of Center
#
#*********************************************

# In 2005, UVA opened a new tranplant center in Roanoke
# Test of center contribution - 1988-2004 vs 2005-2013

t.test(uva$Liver[1:17], uva$Liver[18:26])

wilcox.test(uva$Liver[1:17], uva$Liver[18:26])

##Interpert the results of the t-test.  Is there a difference before and after the Roanoke center?

#*********************************************
#  
#		Time Series for Liver Transplants
#
#*********************************************

par(mfrow = c(1,2))
acf(uva$Liver)
pacf(uva$Liver)
par(mfrow = c(1,1))

uva.arma <- auto.arima(uva$Liver)

uva.arma 

par(mfrow = c(1,2))
acf(uva.arma$resid)
pacf(uva.arma$resid)
par(mfrow = c(1,1))

plot(1988:2013,uva.arma$resid, type = "l", xlab = "Years", ylab = "Residuals", main = "UVA ARMA Model Residuals")

# Classical tests again

t.test(uva.arma$resid[1:17], uva.arma$resid[18:26])

wilcox.test(uva.arma$resid[1:17], uva.arma$resid[18:26])

# But this does not control for Region 11 or for the center.

#*******************************************
#
#  	Time Series and Linear Models
#
#*******************************************


# Linear Model of Liver transplants at UVA

uva.Liv <- lm(uva$Liver[-26]~r11donor$Liver[-26])

summary(uva.Liv)

# diagnostics
par(mfrow = c(2,2))
plot(uva.Liv)
par(mfrow = c(1,1))

## Diagnostic plot issues?

# Evaluating correlation
par(mfcol = c(1,2))
acf(uva.Liv$residuals)
pacf(uva.Liv$residuals)
par(mfcol = c(1,1))

## Is there any serial correlation?

library(MASS)
boxcox(uva.Liv)

#*********************************************
#  
#		Center Model
#
#*********************************************

Roan <- c(rep(0, 17), rep(1, 8))

uva.liv2 <- lm(uva$Liver[-26]~Roan + r11donor$Liver[-26])

summary(uva.liv2)

par(mfrow = c(2,2))
plot(uva.liv2)
par(mfrow = c(1,1))


par(mfrow = c(1,2))
acf(uva.liv2$resid, main = "ACF for Resid from RS1")
pacf(uva.liv2$resid, main = "PACF for Resid from RS1")
par(mfrow = c(1,1))

#******************************************************
#  
#	Linear Model with Time Series for Liver Transplants
#
#*****************************************************

# time series model for the correlated residuals

uvaliv.ar <- ar(uva.liv2$resid)
uvaliv.ar
plot(uvaliv.ar$aic, type = "h", ylab = "AIC")
uvaliv.ar$aic
# Model with AR(2)

uva.liv3 <- lm(uva$Liver[2:25]~Roan[2:25] + r11donor$Liver[2:25]+uva.liv2$resid[1:24])

summary(uva.liv3)

par(mfrow = c(2,2))
plot(uva.liv3)
par(mfrow = c(1,1))

par(mfrow = c(1,2))
acf(uva.liv3$resid, main = "ACF for Resid from RS2")
pacf(uva.liv3$resid, main = "PACF for Resid from RS2")
par(mfrow = c(1,1))


#    Get the fitted values from the regression model
uva.fit <- fitted(uva.liv3)
#    Get the residuals from the regression model
uva.le <- residuals(uva.liv3)
#    Get the regression model
uva.mod <- model.matrix(uva.liv3)
#     Use the RTSB function to obtain the bootstrap
uva.liv.boot <- RTSB(uva$Liver[2:25], Roan[2:25], uva.fit, uva.le, uva.mod,2000)
#     The estimates
uva.liv.boot
#    Plot the results for the coeffiecient for the center
plot(uva.liv.boot, index = 2)

boot.ci(uva.liv.boot, index = 2)

## What can we we conclude about the Roanoke Center?

#*********************************************
#  
#		Poisson Model for Liver Transplants
#
#*********************************************

hist(uva$Liver, breaks = 20)

#rug draws ticks for each value
rug(uva$Liver)

#Without time series
ldf = data.frame(UVAL = uva$Liver[1:25], R11D = r11donor$Liver[1:25])


uva.liv.glm <- glm(UVAL~., data = ldf, family = poisson)

summary(uva.liv.glm)

# Model utility test

uva.liv.null <- glm(UVAL~1, data = ldf, family = poisson)

anova(uva.liv.null, uva.liv.glm, test= "Chi")

# check dispersion

sum(resid(uva.liv.glm, type = "pearson")^2/uva.liv.glm$df.residual)


# Quasi-poisson model

uva.liv.glm2 <- glm(uva$Liver~r11donor$Liver, family = quasipoisson)

summary(uva.liv.glm2)

par(mfrow = c(2,2))
plot(uva.liv.glm2)
par(mfrow = c(1,1))

# Look at the time series

uva.liv.res <- residuals(uva.liv.glm2, type = "pearson")

par(mfrow = c(1,2))
acf(uva.liv.res)
pacf(uva.liv.res)
par(mfrow = c(1,1))

# Model with time series component

ldf = data.frame(UVAL = uva$Liver[2:25], R11D = r11donor$Liver[2:25], LUVAL = uva.liv.res[1:24])

uva.liv.glm3 <- glm(UVAL~., data = ldf, family = quasipoisson)

# Model utility test

uva.liv.null <- glm(UVAL~1, data = ldf, family = quasipoisson)

anova(uva.liv.null, uva.liv.glm3, test= "Chi")

summary(uva.liv.glm3)

par(mfrow = c(2,2))
plot(uva.liv.glm3)
par(mfrow = c(1,1))



# Does the new center affect the results?

Roan <- c(rep(0, 17), rep(1, 8))

ldf = data.frame(UVAL = uva$Liver[2:25], R11D = r11donor$Liver[2:25], LUVAL = uva.liv.res[1:24], Roan = Roan[2:25])

uva.liv.glm4 <- glm(UVAL~., data = ldf, family = quasipoisson)

summary(uva.liv.glm4)

# Model utility test

uva.liv.null <- glm(UVAL~1, data = ldf, family = quasipoisson)

anova(uva.liv.null, uva.liv.glm4, test= "Chi")

# Diagnostics

par(mfrow = c(2,2))
plot(uva.liv.glm4)
par(mfrow = c(1,1))

# Look for serial correlation

uva.liv.res <- residuals(uva.liv.glm4, type = "pearson")

par(mfrow = c(1,2))
acf(uva.liv.res)
pacf(uva.liv.res)
par(mfrow = c(1,1))

# Test of Roanoke center

uva.liv.nRoan <- glm(UVAL~., data = ldf[,-4], family = quasipoisson)

anova(uva.liv.nRoan, uva.liv.glm4, test= "Chi")




