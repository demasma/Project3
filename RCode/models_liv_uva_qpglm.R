uva <- read.table("~/Documents/UVaGrad/Fall2013/SYS6021/Project3/TransplantData/UVAxplant.csv", sep = ",", header = T)
r11donor <- read.table("~/Documents/UVaGrad/Fall2013/SYS6021/Project3/TransplantData/R11donor.csv", sep = ",", header = T)
#uva <- uva.xplant
#*********************************************
#  
#  		Transplant Center
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

png("./acf-pacf_uva-liv.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva$Liver)
pacf(uva$Liver)
par(mfrow = c(1,1))
dev.off()

uva.arma <- auto.arima(uva$Liver)

uva.arma 

png("./acf-pacf_uva-liv_arma_resid.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.arma$resid)
pacf(uva.arma$resid)
par(mfrow = c(1,1))
dev.off()

png("./uva-liv_arma_resid.png", width= 900, height=900)
plot(1988:2013,uva.arma$resid, type = "l", xlab = "Years", ylab = "Residuals", main = "UVA ARMA Model Residuals")
abline(0,0, lty=3)
dev.off()
# Classical tests again

t.test(uva.arma$resid[1:17], uva.arma$resid[18:26])
summary(uva.arma)
uva.arma
wilcox.test(uva.arma$resid[1:17], uva.arma$resid[18:26])

# But this does not control for Region 11 or for the center.

#*******************************************
#
#  	Time Series and Linear Models
#
#*******************************************


# Linear Model of Liver transplants at UVA

uva.Liv <- lm(uva$Liver[-26]~r11.donor$Liver[-26])

summary(uva.Liv)

# diagnostics

png("./diag_lm_uva-liv--r11donor.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.Liv)
par(mfrow = c(1,1))
dev.off()
## Diagnostic plot issues?

# Evaluating correlation
png("./acf_lm_uva-liv--r11donor.png", width= 900, height=900)
par(mfcol = c(1,2))
acf(uva.Liv$residuals)
pacf(uva.Liv$residuals)
par(mfcol = c(1,1), ps=20)
dev.off()
## Is there any serial correlation?

library(MASS)
png("./box-cox_lm_uva-liv--r11donor.png", width= 900, height=900)
boxcox(uva.Liv, main="Box-Cox Plot for UVa Liver-R11 donor LM")
dev.off()
#*********************************************
#  
#		Center Model
#
#*********************************************

Roan <- c(rep(0, 17), rep(1, 8))

uva.liv2 <- lm(uva$Liver[-26]~Roan + r11.donor$Liver[-26])

summary(uva.liv2)


png("./diag_lm_uva-liv--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.liv2)
par(mfrow = c(1,1))
dev.off()

png("./acf_lm_uva-liv--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.liv2$resid, main = "ACF for Resid from RS1")
pacf(uva.liv2$resid, main = "PACF for Resid from RS1")
par(mfrow = c(1,1))
dev.off()

(uva.liv2.ar <- ar(uva.liv2$resid))

png("./aic_lm_uva-liv--r11donor--roan.png", width= 900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.liv2.ar$aic, type = "h")
dev.off()

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

# Model with AR(1)

uva.liv3 <- lm(uva$Liver[2:25]~Roan[2:25] + r11.donor$Liver[2:25]+uva.liv2$resid[1:24])

summary(uva.liv3)

png("./diag_ar1-lm_uva-liv--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.liv3)
par(mfrow = c(1,1))
dev.off()

png("./acf_ar1-lm_uva-liv--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(1,2),ps=20)
acf(uva.liv3$resid, main = "ACF for Resid from RS2")
pacf(uva.liv3$resid, main = "PACF for Resid from RS2")
par(mfrow = c(1,1))
dev.off()

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
png("./boot_ar1-lm_uva-liv--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(1,1),ps=20)
plot(uva.liv.boot, index = 2)
dev.off()

boot.ci(uva.liv.boot, index = 2)

## What can we we conclude about the Roanoke Center?

#*********************************************
#  
#		Poisson Model for Liver Transplants
#
#*********************************************


png("./hist_uva-liv.png", width= 900, height=900)
hist(uva$Liver, breaks = 20, 
     main="Histogram of UVa Liver Transplants from 1988 to 2013")

#rug draws ticks for each value
rug(uva$Liver)
dev.off()

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

png("./diag_qpglm_uva-liv.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.liv.glm2)
par(mfrow = c(1,1))
dev.off()
# Look at the time series

(uva.liv.res <- residuals(uva.liv.glm2, type = "pearson"))

png("./acf_qpglm_uva-liv.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.liv.res)
pacf(uva.liv.res)
par(mfrow = c(1,1))
dev.off()

# Model with time series component

ldf = data.frame(UVAL = uva$Liver[2:25], R11D = r11donor$Liver[2:25], LUVAL = uva.liv.res[1:24])

uva.liv.glm3 <- glm(UVAL~., data = ldf, family = quasipoisson)
summary(uva.liv.glm3)
# Model utility test

uva.liv.null <- glm(UVAL~1, data = ldf, family = quasipoisson)

anova(uva.liv.null, uva.liv.glm3, test= "Chi")

summary(uva.liv.glm3)

png("./diag_qpglm_ar1_uva-liv.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.liv.glm3)
par(mfrow = c(1,1))
dev.off()


# Does the new center affect the results?

Roan <- c(rep(0, 17), rep(1, 8))

ldf = data.frame(UVAL = uva$Liver[2:25], R11D = r11donor$Liver[2:25], LUVAL = uva.liv.res[1:24], Roan = Roan[2:25])

uva.liv.glm4 <- glm(UVAL~., data = ldf, family = quasipoisson)

summary(uva.liv.glm4)
BIC(uva.liv.glm4)
?aic
# Model utility test

uva.liv.null <- glm(UVAL~1, data = ldf, family = quasipoisson)

anova(uva.liv.null, uva.liv.glm4, test= "Chi")

# Diagnostics

png("./diag_qpglm_uva-liv_roan.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.liv.glm4)
par(mfrow = c(1,1))
dev.off()
# Look for serial correlation

uva.liv.res <- residuals(uva.liv.glm4, type = "pearson")

png("./acf_qpglm_uva-liv_roan.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.liv.res)
pacf(uva.liv.res)
par(mfrow = c(1,1))
dev.off()
# Test of Roanoke center

uva.liv.nRoan <- glm(UVAL~., data = ldf[,-4], family = quasipoisson)

anova(uva.liv.nRoan, uva.liv.glm4, test= "Chi")
