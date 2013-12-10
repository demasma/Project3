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
uvamcv.nd <- data.frame(r11k = r11.ar.f$mean,uvamcv.lm.e1 = uvamcv.lm.e1[24])
# include r11 forecast and resid term and the use predict
# Predicting the linear model
predict(uvamcv.lm2, newdata = uvamcv.nd, se.fit = T)
# Bootstrap prediction
uvamcv.boot <- RFB(uvamcv.dm, model = uvamcv.lm2, 
                   ndata = uvamcv.nd, num = 2000 )
# Bootstrap plot 
plot(uvamcv.boot, index = 1)
##figure diffhist.pdf
plot(uvamcv.boot, index = 1, main = "UVA−MCV Kidey Transplants")
# 2 Sided CI
uvamcv.ci <- boot.ci(uvamcv.boot,index =1)
uvamcv.ci
# Plot of predictions for each value of the pw and the confidence intervals
uvamcv <- uva$Kidney[-26]-mcv$Kidney[-26]
plot( uvamcv[-26]~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Numbers of UVA Kidney Transplants", ylim = c(-80, 50))
points(2013, uvamcv.boot$t0[1], pch = 19)
segments(2013, uvamcv.boot$t0,  2013, uvamcv.ci$percent[4], lty = "dashed" )
segments(2013, uvamcv.boot$t0,  2013, uvamcv.ci$percent[5], lty = "dashed" )
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
uvamcv.sim <- RFBSIM2(uvamcv.dm, model = uvamcv.lm2, ndata = uvamcv.nd, num = 2000, predse = data.frame(r11k = c(1, r11.ar.f$se)) )
plot(uvamcv.sim, index = 1)
##############
# 2 sided CI
uvamcv.sim.ci <- boot.ci(uvamcv.sim, index = 1)
uvamcv.sim.ci
quantile(uvamcv.sim$t, c(.025, .975))
hist(uvamcv.sim$t)
# Plot of difference of UVA and MCV in no. of transplants
uvamcv <- uva$Kidney[-26]-mcv$Kidney[-26]
plot( uvamcv~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Difference Between UVA and MCV Kidney Transplants", ylim = c(uvamcv.sim.ci$percent[4], max(uvamcv[-26])))
#Bootstrapped predictions
points(2013, uvamcv.boot$t0[1], pch = 19)
segments(2013, uvamcv.boot$t0,  2013, uvamcv.ci$percent[4], lty = "dashed" )
segments(2013, uvamcv.boot$t0,  2013, uvamcv.ci$percent[5], lty = "dashed" )
#Simulation predictions
points(2013.5, median(uvamcv.sim$t), pch = 17)
segments(2013.5, median(uvamcv.sim$t),  2013.5, uvamcv.sim.ci$percent[4], lty = "dashed" )
segments(2013.5, median(uvamcv.sim$t),  2013.5, uvamcv.sim.ci$percent[5], lty = "dashed" )
abline(h = 0)
points(2012.5, uva$Kidney[26]-mcv$Kidney[26]) # Current value
legend(1990,-40, legend = c("Bootstrap", "Current", "Simulation"), pch = c(19, 1, 17))
# are the ranges close to 0??? including ConfInt? the 
# subdata functions??? 
