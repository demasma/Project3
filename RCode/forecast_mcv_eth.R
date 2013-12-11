#------------------------------------------------------------------------------
# Things we need:
#  - model
#    - uvamcv.e.lm2
#  - data
#    - uva and mcv xplant
#  - dataframe from model
#    - uvamcv.e.dm
#  - residuals
#    - uvamcv.e.lm.e1
#------------------------------------------------------------------------------
#
library(forecast)
library(boot)

setwd("~/Projects")
setwd("./Project3/TransplantData/")

r11donor <- read.table("R11donor.csv", sep = ",", header = T)

uva <- read.table("UVAxplant.csv", sep = ",", header = T)
duke <- read.table("Dukexplant.csv", sep = ",", header = T)
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

setwd("~/Box Documents/project/figures/")

uva.e <- data.frame(uva.eth[, "Kidney.W"], Kidney.O = apply(uva.eth[,which(colnames(uva.eth) != "Kidney.W")], 1, sum))
mcv.e <- data.frame(mcv.eth[, "Kidney.W"], Kidney.O = apply(mcv.eth[,which(colnames(mcv.eth) != "Kidney.W")], 1, sum))
duke.e <- data.frame(duke.eth[, "Kidney.W"], Kidney.O = apply(duke.eth[,which(colnames(duke.eth) != "Kidney.W")], 1, sum))

# TS Difference
uvamcv.e <- uva.e$Kidney.O - mcv.e$Kidney.O 
# LM Model
uvamcv.e.lm <- lm(uvamcv.e ~ r11donor$Kidney)
# Adding the time series model
# AR(1)
uvamcv.e.lm.e1 <- uvamcv.e.lm$resid[1:24] # new var..1 AR term put resid into AR
r11k <- r11donor$Kidney[2:25] # need lag of use 1-24 to pred 2-25
uvamcv.e <- uva.k.e$Kidney.O[2:25] - mcv.k.e$Kidney.O[2:25] # lag from 1-24 to pred 2-25 diff
uvamcv.e.dm <- data.frame(uvamcv.e, r11k, uvamcv.e.lm.e1)

# Linear model with time series component
uvamcv.e.lm2<- lm(uvamcv.e ~ ., data = uvamcv.e.dm)






##########################
# Forecasting R11 
# We need to forecast the r11 donors and the model residuals.
# Time series model of r11 donors
##########################
par(mfrow = c(2,1))
acf(r11donor$Kidney)
pacf(r11donor$Kidney) # AR(1) is good
par(mfrow = c(1,1))
# Time series model of R11 donors.
r11.ar <- ar(r11donor$Kidney[-26])
# Forecast of R11 donors
r11.ar.f <- forecast(r11.ar, h = 1) 
#------------------------------------------------------------------------------
# Model Dependent
#------------------------------------------------------------------------------
#
# Creating the new data data fame
uvamcv.e.nd <- data.frame(r11k = r11.ar.f$mean,uvamcv.e.lm.e1 = uvamcv.e.lm.e1[24])
# include r11 forecast and resid term and the use predict
# Predicting the linear model
predict(uvamcv.e.lm2, newdata = uvamcv.e.nd, se.fit = T)
# Bootstrap prediction
uvamcv.e.boot <- RFB(uvamcv.e.dm, model = uvamcv.e.lm2, 
                   ndata = uvamcv.e.nd, num = 2000 )
# Bootstrap plot 
plot(uvamcv.e.boot, index = 1)
##figure diffhist.pdf
plot(uvamcv.e.boot, index = 1, main = "UVA−MCV Kidey Transplants")
# 2 Sided CI
uvamcv.e.ci <- boot.ci(uvamcv.e.boot,index =1)
uvamcv.e.ci
# Plot of predictions for each value of the pw and the confidence intervals
uvamcv.e <- uva$Kidney[-26]-mcv$Kidney[-26]
plot( uvamcv.e[-26]~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Numbers of UVA Kidney Transplants", ylim = c(-80, 50))
points(2013, uvamcv.e.boot$t0[1], pch = 19)
segments(2013, uvamcv.e.boot$t0,  2013, uvamcv.e.ci$percent[4], lty = "dashed" )
segments(2013, uvamcv.e.boot$t0,  2013, uvamcv.e.ci$percent[5], lty = "dashed" )
points(2012.5, uva$Kidney[26]-mcv$Kidney[26])
abline(h = 0)
legend(1990,-40, legend = c("Predicted", "Current"), pch = c(19, 1))
#used forecasting for R11
#***************************************************
# 
#  Simulation for Improved CI
#
#***************************************************
# Simulating the results from R11 and the UVA transplant time series
# Get mean and SD for the time series inputs
# Forecast of R11 donors gives 
# parameters for the simulation
r11.ar.f <- predict(r11.ar, se.fit = T) 
r11.ar.f
# Simulating the predictor to obtain CI
#------------------------------------------------------------------------------
# Model Input
#------------------------------------------------------------------------------
uvamcv.e.sim <- RFBSIM2(uvamcv.e.dm, model = uvamcv.e.lm2, ndata = uvamcv.e.nd, num = 2000, predse = data.frame(r11k = c(1, r11.ar.f$se)) )
plot(uvamcv.e.sim, index = 1)
##############
# 2 sided CI
uvamcv.e.sim.ci <- boot.ci(uvamcv.e.sim, index = 1)
uvamcv.e.sim.ci
quantile(uvamcv.e.sim$t, c(.025, .975))
hist(uvamcv.e.sim$t)
# Plot of difference of UVA and MCV in no. of transplants
uvamcv.e <- uva$Kidney[-26]-mcv$Kidney[-26]
png('~/Box Documents/project/figures/pred_uva-mcv_kid_eth.png', width=900, height=600)
plot( uvamcv.e~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Difference Between UVA and MCV Kidney Non-White Patient Transplants", ylim = c(uvamcv.e.sim.ci$percent[4], max(uvamcv.e[-26])))
#Bootstrapped predictions
points(2013, uvamcv.e.boot$t0[1], pch = 19)
segments(2013, uvamcv.e.boot$t0,  2013, uvamcv.e.ci$percent[4], lty = "dashed" )
segments(2013, uvamcv.e.boot$t0,  2013, uvamcv.e.ci$percent[5], lty = "dashed" )
#Simulation predictions
points(2013.5, median(uvamcv.e.sim$t), pch = 17)
segments(2013.5, median(uvamcv.e.sim$t),  2013.5, uvamcv.e.sim.ci$percent[4], lty = "dashed" )
segments(2013.5, median(uvamcv.e.sim$t),  2013.5, uvamcv.e.sim.ci$percent[5], lty = "dashed" )
abline(h = 0)
points(2012.5, uva.e$Kidney.O[26]-mcv.e$Kidney.O[26]) # Current value
legend(1990,-40, legend = c("Bootstrap", "Current", "Simulation"), pch = c(19, 1, 17))
dev.off()
# are the ranges close to 0??? including ConfInt? the 
# subdata functions??? 
median(uvamcv.e.sim$t)
uvamcv.e.sim.ci$percent[4]
uvamcv.e.sim.ci$percent[5]
uvamcv.e.sim.ci

uvamcv.e.boot$t0[1]
uvamcv.e.ci$percent[4]
uvamcv.e.ci$percent[5]
uvamcv.e.ci

uva$Kidney[26]-mcv$Kidney[26]
uva.e$Kidney.O[26]-mcv.e$Kidney.O[26]
