uva <- read.table("~/Documents/UVaGrad/Fall2013/SYS6021/Project3/TransplantData/UVAxplant.csv", sep = ",", header = T)
r11donor <- read.table("~/Documents/UVaGrad/Fall2013/SYS6021/Project3/TransplantData/R11donor.csv", sep = ",", header = T)


#*********************************************
#  
#    	Transplant Center
#		Classical Tests for Effectiveness of Center
#
#*********************************************

# In 2005, UVA opened a new tranplant center in Roanoke
# Test of center contribution - 1988-2004 vs 2005-2013

t.test(uva$Kidney[1:17], uva$Kidney[18:26])

wilcox.test(uva$Kidney[1:17], uva$Kidney[18:26])

##Interpert the results of the t-test.  Is there a difference before and after the Roanoke center?

#*********************************************
#  
#		Time Series for Kidney Transplants
#
#*********************************************

png("./acf-pacf_uva-kid.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva$Kidney)
pacf(uva$Kidney)
par(mfrow = c(1,1))
dev.off()

uva.arma <- auto.arima(uva$Kidney)

uva.arma 

png("./acf-pacf_uva-kid_arma_resid.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.arma$resid)
pacf(uva.arma$resid)
par(mfrow = c(1,1))
dev.off()

png("./uva-kid_arma_resid.png", width= 900, height=900)
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


# Linear Model of Kidney transplants at UVA

uva.Liv <- lm(uva$Kidney[-26]~r11.donor$Kidney[-26])

summary(uva.Liv)

# diagnostics

png("./diag_lm_uva-kid--r11donor.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.Liv)
par(mfrow = c(1,1))
dev.off()
## Diagnostic plot issues?

# Evaluating correlation
png("./acf_lm_uva-kid--r11donor.png", width= 900, height=900)
par(mfcol = c(1,2))
acf(uva.Liv$residuals)
pacf(uva.Liv$residuals)
par(mfcol = c(1,1), ps=20)
dev.off()
## Is there any serial correlation?

library(MASS)
png("./box-cox_lm_uva-kid--r11donor.png", width= 900, height=900)
boxcox(uva.Liv, main="Box-Cox Plot for UVa Kidney-R11 donor LM")
dev.off()
#*********************************************
#  
#		Center Model
#
#*********************************************

Roan <- c(rep(0, 17), rep(1, 8))

uva.kid2 <- lm(uva$Kidney[-26]~Roan + r11.donor$Kidney[-26])

summary(uva.kid2)


png("./diag_lm_uva-kid--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.kid2)
par(mfrow = c(1,1))
dev.off()

png("./acf_lm_uva-kid--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.kid2$resid, main = "ACF for Resid from RS1")
pacf(uva.kid2$resid, main = "PACF for Resid from RS1")
par(mfrow = c(1,1))
dev.off()

(uva.kid2.ar <- ar(uva.kid2$resid))

png("./aic_lm_uva-kid--r11donor--roan.png", width= 900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.kid2.ar$aic, type = "h")
dev.off()

#******************************************************
#  
#	Linear Model with Time Series for Kidney Transplants
#
#*****************************************************

# time series model for the correlated residuals

uvakid.ar <- ar(uva.kid2$resid)
uvakid.ar
plot(uvakid.ar$aic, type = "h", ylab = "AIC")
uvakid.ar$aic

# Model with AR(4)

#uva.kid3 <- lm(uva$Kidney[2:25]~Roan[2:25] + r11.donor$Kidney[2:25]+uva.kid2$resid[1:24])
uva.kid3 <- lm(uva$Kidney[5:25]~Roan[5:25] + r11.donor$Kidney[5:25] 
               + uva.kid2$resid[1:21]
               + uva.kid2$resid[2:22]
               + uva.kid2$resid[3:23]
               + uva.kid2$resid[4:24])

summary(uva.kid3)

png("./diag_ar4-lm_uva-kid--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.kid3)
par(mfrow = c(1,1))
dev.off()

png("./acf_ar4-lm_uva-kid--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(1,2),ps=20)
acf(uva.kid3$resid, main = "ACF for Resid from RS2")
pacf(uva.kid3$resid, main = "PACF for Resid from RS2")
par(mfrow = c(1,1))
dev.off()

#    Get the fitted values from the regression model
uva.fit <- fitted(uva.kid3)
#    Get the residuals from the regression model
uva.le <- residuals(uva.kid3)
#    Get the regression model
uva.mod <- model.matrix(uva.kid3)
#     Use the RTSB function to obtain the bootstrap
uva.kid.boot <- RTSB(uva$Kidney[5:25], Roan[5:25], uva.fit, uva.le, uva.mod,2000)
#     The estimates
uva.kid.boot
#    Plot the results for the coeffiecient for the center
png("./boot_ar1-lm_uva-kid--r11donor--roan.png", width= 900, height=900)
par(mfrow = c(3,1),ps=20)
plot(uva.kid.boot, index = 2)
plot(uva.kid.boot, index = 3)
plot(uva.kid.boot, index = 4)
par(mfrow = c(1,1),ps=20)
dev.off()

boot.ci(uva.kid.boot, index = 2)
boot.ci(uva.kid.boot, index = 3)
boot.ci(uva.kid.boot, index = 4)
boot.ci(uva.kid.boot, index = 5)
boot.ci(uva.kid.boot, index = 6)
boot.ci(uva.kid.boot, index = 7)

## What can we we conclude about the Roanoke Center?

#*********************************************
#  
#		Poisson Model for Kidney Transplants
#
#*********************************************


png("./hist_uva-kid.png", width= 900, height=900)
hist(uva$Kidney, breaks = 20, 
     main="Histogram of UVa Kidney Transplants from 1988 to 2013")

#rug draws ticks for each value
rug(uva$Kidney)
dev.off()

#Without time series
ldf = data.frame(UVAL = uva$Kidney[1:25], R11D = r11donor$Kidney[1:25])


uva.kid.glm <- glm(UVAL~., data = ldf, family = poisson)

summary(uva.kid.glm)

# Model utility test

uva.kid.null <- glm(UVAL~1, data = ldf, family = poisson)

anova(uva.kid.null, uva.kid.glm, test= "Chi")

# check dispersion

sum(resid(uva.kid.glm, type = "pearson")^2/uva.kid.glm$df.residual)


# Quasi-poisson model

uva.kid.glm2 <- glm(uva$Kidney~r11donor$Kidney, family = quasipoisson)

summary(uva.kid.glm2)

png("./diag_qpglm_uva-kid.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.kid.glm2)
par(mfrow = c(1,1))
dev.off()
# Look at the time series

(uva.kid.res <- residuals(uva.kid.glm2, type = "pearson"))

png("./acf_qpglm_uva-kid.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.kid.res)
pacf(uva.kid.res)
par(mfrow = c(1,1))
dev.off()

uva.kid.glm2.ar <- ar(uva.kid.glm2$resid)
uva.kid.glm2.ar
plot(uva.kid.glm2.ar$aic, type = "h", ylab = "AIC")
uva.kid.glm2.ar$aic

# Model with time series component

#ldf = data.frame(UVAL = uva$Kidney[2:25], R11D = r11donor$Kidney[2:25], LUVAL = uva.kid.res[1:24])
ldf = data.frame(UVAL = uva$Kidney[3:25], R11D = r11donor$Kidney[3:25], 
                 LUVAL1 = uva.kid.res[2:24],
                 LUVAL2 = uva.kid.res[1:23]
                 )

uva.kid.glm3 <- glm(UVAL~., data = ldf, family = quasipoisson)
summary(uva.kid.glm3)
# Model utility test

uva.kid.null <- glm(UVAL~1, data = ldf, family = quasipoisson)

anova(uva.kid.null, uva.kid.glm3, test= "Chi")

summary(uva.kid.glm3)

png("./diag_qpglm_ar2_uva-kid.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.kid.glm3)
par(mfrow = c(1,1))
dev.off()


# Does the new center affect the results?

Roan <- c(rep(0, 17), rep(1, 8))

#ldf = data.frame(UVAL = uva$Kidney[2:25], R11D = r11donor$Kidney[2:25], LUVAL = uva.kid.res[1:24], Roan = Roan[2:25])
ldf = data.frame(UVAL = uva$Kidney[3:25], R11D = r11donor$Kidney[3:25], 
                 LUVAL1 = uva.kid.res[2:24],
                 LUVAL2 = uva.kid.res[1:23],
                 Roan = Roan[3:25]
                 )

uva.kid.glm4 <- glm(UVAL~., data = ldf, family = quasipoisson)

summary(uva.kid.glm4)

# Model utility test

uva.kid.null <- glm(UVAL~1, data = ldf, family = quasipoisson)

anova(uva.kid.null, uva.kid.glm4, test= "Chi")

# Diagnostics

png("./diag_qpglm_uva-kid_roan.png", width= 900, height=900)
par(mfrow = c(2,2))
plot(uva.kid.glm4)
par(mfrow = c(1,1))
dev.off()
# Look for serial correlation

uva.kid.res <- residuals(uva.kid.glm4, type = "pearson")

png("./acf_qpglm_uva-kid_roan.png", width= 900, height=900)
par(mfrow = c(1,2))
acf(uva.kid.res)
pacf(uva.kid.res)
par(mfrow = c(1,1))
dev.off()
# Test of Roanoke center

uva.kid.nRoan <- glm(UVAL~., data = ldf[,-5], family = quasipoisson)

anova(uva.kid.nRoan, uva.kid.glm4, test= "Chi")
