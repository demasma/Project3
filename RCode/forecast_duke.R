##########################
# Forecasting R11 
# We need to forecast the r11 donors and the model residuals.
# Time series model of r11 donors
##########################
library(forecast)
library(boot)

setwd("~/Projects")
setwd("./Project3/TransplantData/")

r11donor<-read.table("R11donor.csv", sep = ",", header = T)

uva <- read.table("UVAxplant.csv", sep = ",", header = T)
duke <- read.table("Dukexplant.csv", sep = ",", header = T)
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

setwd("~/Box Documents/project/figures/")

uvaduke <- uva$Kidney - duke$Kidney
uvaduke.lm <- lm(uvaduke ~ r11donor$Kidney)
#=======================================================
# UVa-Duke TS LM model
#=======================================================
# Adding the time series model
# AR(4)
uvaduke.lm.e1 <- uvaduke.lm$resid[4:24] # new var..1 AR term put resid into AR
uvaduke.lm.e2 <- uvaduke.lm$resid[3:23] # new var..1 AR term put resid into AR
uvaduke.lm.e3 <- uvaduke.lm$resid[2:22] # new var..1 AR term put resid into AR
uvaduke.lm.e4 <- uvaduke.lm$resid[1:21] # new var..1 AR term put resid into AR
r11k <- r11donor$Kidney[5:25] # need lag of use 1-24 to pred 2-25
uvaduke <- uva$Kidney[5:25] - duke$Kidney[5:25] # lag from 1-24 to pred 2-25 diff
uvaduke.dm <- data.frame(uvaduke, r11k, uvaduke.lm.e1,uvaduke.lm.e2,uvaduke.lm.e3,uvaduke.lm.e4)
# Linear model with time series component
uvaduke.lm2<- lm(uvaduke ~ ., data = uvaduke.dm)


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
#uvaduke.nd <- data.frame(r11k = r11.ar.f$mean,uvaduke.lm.e1 = uvaduke.lm.e1[24])
uvaduke.nd <- data.frame(r11k = r11.ar.f$mean,
                         uvaduke.lm.e1 = uvaduke.lm.e1[21],
                         uvaduke.lm.e2 = uvaduke.lm.e2[21],
                         uvaduke.lm.e3 = uvaduke.lm.e3[21],
                         uvaduke.lm.e4 = uvaduke.lm.e4[21]
                         )
# include r11 forecast and resid term and the use predict
# Predicting the linear model
predict(uvaduke.lm2, newdata = uvaduke.nd, se.fit = T)
# Bootstrap prediction
uvaduke.boot <- RFB(uvaduke.dm, model = uvaduke.lm2, 
                   ndata = uvaduke.nd, num = 2000 )
# Bootstrap plot 
plot(uvaduke.boot, index = 1)
##figure diffhist.pdf
plot(uvaduke.boot, index = 1, main = "UVA−MCV Kidey Transplants")
# 2 Sided CI
uvaduke.ci <- boot.ci(uvaduke.boot,index =1)
uvaduke.ci
# Plot of predictions for each value of the pw and the confidence intervals
uvaduke <- uva$Kidney[-26]-duke$Kidney[-26]
plot( uvaduke[-26]~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Numbers of UVA Kidney Transplants", ylim = c(-80, 50))
points(2013, uvaduke.boot$t0[1], pch = 19)
segments(2013, uvaduke.boot$t0,  2013, uvaduke.ci$percent[4], lty = "dashed" )
segments(2013, uvaduke.boot$t0,  2013, uvaduke.ci$percent[5], lty = "dashed" )
points(2012.5, uva$Kidney[26]-duke$Kidney[26])
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
uvaduke.sim <- RFBSIM2(uvaduke.dm, model = uvaduke.lm2, ndata = uvaduke.nd, num = 2000, predse = data.frame(r11k = c(1, r11.ar.f$se)) )
plot(uvaduke.sim, index = 1)
##############
# 2 sided CI
uvaduke.sim.ci <- boot.ci(uvaduke.sim, index = 1)
uvaduke.sim.ci
quantile(uvaduke.sim$t, c(.025, .975))
hist(uvaduke.sim$t)
# Plot of difference of UVA and MCV in no. of transplants
uvaduke <- uva$Kidney[-26]-duke$Kidney[-26]
png('~/Box Documents/project/figures/pred_uva-duke_kid.png', width=900, height=600)
plot( uvaduke~uva$Year[-26], xlim = c(1988, 2013), type = "l", 
      xlab = "Years", ylab = "No. of Kidney", 
      main = "Predicted Difference Between UVA and Duke Kidney Transplants", 
      ylim = c(min(uvaduke[-26]), max(uvaduke[-26])))
      #ylim = c(uvaduke.sim.ci$percent[4], max(uvaduke[-26])))
#Bootstrapped predictions
points(2013, uvaduke.boot$t0[1], pch = 19)
segments(2013, uvaduke.boot$t0,  2013, uvaduke.ci$percent[4], lty = "dashed" )
segments(2013, uvaduke.boot$t0,  2013, uvaduke.ci$percent[5], lty = "dashed" )
#Simulation predictions
points(2013.5, median(uvaduke.sim$t), pch = 17)
segments(2013.5, median(uvaduke.sim$t),  2013.5, uvaduke.sim.ci$percent[4], lty = "dashed" )
segments(2013.5, median(uvaduke.sim$t),  2013.5, uvaduke.sim.ci$percent[5], lty = "dashed" )
abline(h = 0)
points(2012.5, uva$Kidney[26]-duke$Kidney[26]) # Current value
legend(1990,-40, legend = c("Bootstrap", "Current", "Simulation"), pch = c(19, 1, 17))
# are the ranges close to 0??? including ConfInt? the 
# subdata functions??? 
dev.off()
median(uvaduke.sim$t)
uvaduke.sim.ci$percent[4]
uvaduke.sim.ci$percent[5]

uvaduke.boot$t0[1]
uvaduke.ci$percent[4]
uvaduke.ci$percent[5]

uva$Kidney[26]-duke$Kidney[26]
