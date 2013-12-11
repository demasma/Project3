#------------------------------------------------------------------------------
# Things we need:
#  - model
#    - uvaduke.e.lm
#  - data
#    - uva and mcv xplant
#  - dataframe from model
#    - uvaduke.e.e.dm
#  - residuals
#    - uvaduke.e.lm.e1
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
uvaketh <- subdata("Kidney", uvaeth)

mcvketh <- subdata("Kidney", mcveth)

dukeketh <- subdata("Kidney", dukeeth)



# Remove year 2013 and combine all ethnic groups other than white into one category

# UVA
#uvake <- data.frame(uvaketh[-26, "Kidney.W"], Kidney.O = apply(uvaketh[-26,which(colnames(uvaketh) != "Kidney.W")], 1, sum))
#uva.e <- data.frame(uvaketh[-26, "Kidney.W"], Kidney.O = apply(uvaketh[-26,which(colnames(uvaketh) != "Kidney.W")], 1, sum))
uva.e <- data.frame(uvaketh[, "Kidney.W"], Kidney.O = apply(uvaketh[,which(colnames(uvaketh) != "Kidney.W")], 1, sum))

# MCV
#mcvke <- data.frame(mcvketh[-26, "Kidney.W"], Kidney.O = apply(mcvketh[-26,which(colnames(mcvketh) != "Kidney.W")], 1, sum))
#mcv.e <- data.frame(mcvketh[-26, "Kidney.W"], Kidney.O = apply(mcvketh[-26,which(colnames(mcvketh) != "Kidney.W")], 1, sum))
mcv.e <- data.frame(mcvketh[, "Kidney.W"], Kidney.O = apply(mcvketh[,which(colnames(mcvketh) != "Kidney.W")], 1, sum))

# Duke
#dukeke <- data.frame(dukeketh[-26, "Kidney.W"], Kidney.O = apply(dukeketh[-26,which(colnames(dukeketh) != "Kidney.W")], 1, sum))
#duke.e <- data.frame(dukeketh[-26, "Kidney.W"], Kidney.O = apply(dukeketh[-26,which(colnames(dukeketh) != "Kidney.W")], 1, sum))
duke.e <- data.frame(dukeketh[, "Kidney.W"], Kidney.O = apply(dukeketh[,which(colnames(dukeketh) != "Kidney.W")], 1, sum))

#uva.e <- data.frame(uva.eth[, "Kidney.W"], Kidney.O = apply(uva.eth[,which(colnames(uva.eth) != "Kidney.W")], 1, sum))
#mcv.e <- data.frame(mcv.eth[, "Kidney.W"], Kidney.O = apply(mcv.eth[,which(colnames(mcv.eth) != "Kidney.W")], 1, sum))
#duke.e <- data.frame(duke.eth[, "Kidney.W"], Kidney.O = apply(duke.eth[,which(colnames(duke.eth) != "Kidney.W")], 1, sum))

# TS Difference
uvaduke.e <- uva.e$Kidney.O[-26] - duke.e$Kidney.O[-26]
# LM Model
r11k <- r11donor$Kidney[-26] # need lag of use 1-24 to pred 2-25
#uvaduke.e.lm <- lm(uvaduke.e ~ r11donor$Kidney[-26])
uvaduke.e.lm <- lm(uvaduke.e ~ r11k)
uvaduke.e.lm.e1 <- uvaduke.e.lm$resid[-26] # new var..1 AR term put resid into AR
uvaduke.e.dm <- data.frame(uvaduke.e, r11k, uvaduke.e.lm.e1)
acf(uvaduke.e.lm$resid)
pacf(uvaduke.e.lm$resid)

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
# uvaduke.e.nd <- data.frame(r11k = r11.ar.f$mean,uvaduke.e.lm.e1 = uvaduke.e.lm.e1[25])
uvaduke.e.nd <- data.frame(r11k = r11.ar.f$mean)#, uvaduke.e.lm.e1 = uvaduke.e.lm.e1[24])
# include r11 forecast and resid term and the use predict
# Predicting the linear model
#predict(uvaduke.e.lm, newdata = uvaduke.e.nd, se.fit = T)
predict(uvaduke.e.lm, newdata = uvaduke.e.nd, se.fit = T)
uvaduke.e.nd
uvaduke.e.lm
# Bootstrap prediction
uvaduke.e.boot <- RFB(uvaduke.e.dm, model = uvaduke.e.lm, 
                   ndata = uvaduke.e.nd, num = 2000 )
# Bootstrap plot 
plot(uvaduke.e.boot, index = 1)
##figure diffhist.pdf
plot(uvaduke.e.boot, index = 1, main = "UVA−MCV Kidey Transplants")
# 2 Sided CI
uvaduke.e.ci <- boot.ci(uvaduke.e.boot,index =1)
uvaduke.e.ci
# Plot of predictions for each value of the pw and the confidence intervals
uvaduke.e <- uva.e$Kidney.O[-26]-duke.e$Kidney.O[-26]
plot(uvaduke.e)
plot( uvaduke.e[-26]~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Numbers of UVA Kidney Transplants", ylim = c(-100, 100))
points(2013, uvaduke.e.boot$t0[1], pch = 19)
segments(2013, uvaduke.e.boot$t0,  2013, uvaduke.e.ci$percent[4], lty = "dashed" )
segments(2013, uvaduke.e.boot$t0,  2013, uvaduke.e.ci$percent[5], lty = "dashed" )
points(2012.5, uva.e$Kidney.O[26]-duke.e$Kidney.O[26])
uva.e$Kidney.O[26]-duke.e$Kidney.O[26]
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
uvaduke.e.sim <- RFBSIM2(uvaduke.e.dm, model = uvaduke.e.lm, ndata = uvaduke.e.nd, num = 2000, predse = data.frame(r11k = c(1, r11.ar.f$se)) )
plot(uvaduke.e.sim, index = 1)
##############
# 2 sided CI
uvaduke.e.sim.ci <- boot.ci(uvaduke.e.sim, index = 1)
uvaduke.e.sim.ci
quantile(uvaduke.e.sim$t, c(.025, .975))
hist(uvaduke.e.sim$t)
# Plot of difference of UVA and MCV in no. of transplants
uvaduke.e <- uva.e$Kidney.O[-26] - duke.e$Kidney.O[-26]
png('~/Box Documents/project/figures/pred_uva-duke_kid_eth.png', width=900, height=600)
plot( uvaduke.e~uva$Year[-26], xlim = c(1988, 2013), type = "l", xlab = "Years", ylab = "No. of Kidney", main = "Predicted Difference Between UVA and Duke Kidney Non-White Patient Transplants", ylim = c(-50,10))#c(uvaduke.e.sim.ci$percent[4], max(uvaduke.e[-26]))) #c(-1000,1000))
#Bootstrapped predictions
points(2013, uvaduke.e.boot$t0[1], pch = 19)
segments(2013, uvaduke.e.boot$t0,  2013, uvaduke.e.ci$percent[4], lty = "dashed" )
segments(2013, uvaduke.e.boot$t0,  2013, uvaduke.e.ci$percent[5], lty = "dashed" )
#Simulation predictions
points(2013.5, median(uvaduke.e.sim$t), pch = 17)
segments(2013.5, median(uvaduke.e.sim$t),  2013.5, uvaduke.e.sim.ci$percent[4], lty = "dashed" )
segments(2013.5, median(uvaduke.e.sim$t),  2013.5, uvaduke.e.sim.ci$percent[5], lty = "dashed" )
median(uvaduke.e.sim$t)
uvaduke.e.sim.ci$percent
abline(h = 0)
points(2012.5, uva.e$Kidney.O[26]-duke.e$Kidney.O[26]) # Current value
legend(1990,-40, legend = c("Bootstrap", "Current", "Simulation"), pch = c(19, 1, 17))
dev.off()
# are the ranges close to 0??? including ConfInt? the 
# subdata functions??? 
median(uvaduke.e.sim$t)
uvaduke.e.sim.ci$percent[4]
uvaduke.e.sim.ci$percent[5]

uvaduke.e.boot$t0[1]
uvaduke.e.ci$percent[4]
uvaduke.e.ci$percent[5]

uva.e$Kidney.O[26]-duke.e$Kidney.O[26]
