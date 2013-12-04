#***************************************************************
#
#  Read in the data
#
#***************************************************************

uva <- read.table("UVAxplant.csv", sep = ",", header = T)

mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

duke <- read.table("Dukexplant.csv", sep = ",", header = T)

unc <- read.table("UNCxplant.csv", sep = ",", header = T)

uvaeth <- read.table("UVAethnic.csv", sep = ",", header =T)

mcveth <- read.table("MCVethnic.csv", sep = ",", header =T)

dukeeth <- read.table("DukeEthnic.csv", sep = ",", header =T)

r11donor<-read.table("R11donor.csv", sep = ",", header = T)


#***************************************************************
#
#  Source files and libraries
#
#***************************************************************

# Source 
setwd("~/Box Documents/project/figures/")
source("TSbootfunctions.R") # Get the new version of this.
source("Transplant.plots.R")
source("DemographicFunctions.R")

library(boot)
library(forecast)

#***********************************
# Center plot
#***********************************

#center.plot(cbind( uva$Kidney[-26], unc$Kidney[-26], mcv$Kidney[-26], duke$Kidney[-26]), Year = seq(1988,2012, 1), title = "Kidney")
center.plot(cbind( uva.xplant$Kidney[-26], unc.xplant$Kidney[-26], mcv.xplant$Kidney[-26], duke.xplant$Kidney[-26]), Year = seq(1988,2012, 1), title = "Kidney")

##figure for lecture
center.plot(cbind( uva.xplant$Kidney[-26],  mcv.xplant$Kidney[-26]), Year = seq(1988,2012, 1), title = "Kidney")
plot(uva.xplant$Year[-26], uva.xplant$Kidney[-26], col = "orange", type = "l", ylim = c(min(uva.xplant$Kidney), max(mcv.xplant$Kidney)), xlab = "Year", ylab = "No. of Transplants", main = "Kidney Transplants")
lines(mcv.xplant$Year[-26], mcv.xplant$Kidney[-26], col = "purple")
legend(1990, 120, legend = c("UVA", "MCV"), lwd = 2, col = c("orange", "purple"))

## What do you conclude from this center plot?



#*******************************************
#
#  	Ethnic Diversity Analysis
#
#*******************************************

# Form data sets for the kidney transplants
colnames(uva.eth)
uva.l.eth <- uva.eth[-26, which(substr(colnames(uva.eth),1,5) == "Liver")]
uva.k.eth <- subdata("Kidney", uva.eth)
mcv.l.eth <- mcv.eth[-26, which(substr(colnames(mcv.eth),1,5) == "Liver")]
mcv.k.eth <- subdata("Kidney", mcv.eth)
duke.l.eth <- duke.eth[-26, which(substr(colnames(duke.eth),1,5) == "Liver")]
duke.k.eth <- subdata("Kidney", duke.eth)
?subdata


# Remove year 2013 and combine all ethnic groups other than white into one category

# UVA
uva.l.e <- data.frame(uva.l.eth[-26, "Liver.W"], Liver.O = apply(uva.l.eth[-26,which(colnames(uva.l.eth) != "Liver.W")], 1, sum))
uva.l.e <- data.frame(uva.l.eth$Liver.W, Liver.O = apply(uva.l.eth[,which(colnames(uva.l.eth) != "Liver.W")], 1, sum))
uva.k.e <- data.frame(uva.k.eth[-26, "Kidney.W"], Kidney.O = apply(uva.k.eth[-26,which(colnames(uva.k.eth) != "Kidney.W")], 1, sum))

# MCV
mcv.l.e <- data.frame(mcv.l.eth[-26, "Liver.W"], Liver.O = apply(mcv.l.eth[-26,which(colnames(mcv.l.eth) != "Liver.W")], 1, sum))
mcv.k.e <- data.frame(mcv.k.eth[-26, "Kidney.W"], Kidney.O = apply(mcv.k.eth[-26,which(colnames(mcv.k.eth) != "Kidney.W")], 1, sum))

# Duke
duke.l.e <- data.frame(duke.l.eth[-26, "Liver.W"], Liver.O = apply(duke.l.eth[-26,which(colnames(duke.l.eth) != "Liver.W")], 1, sum))
duke.k.e <- data.frame(duke.k.eth[-26, "Kidney.W"], Kidney.O = apply(duke.k.eth[-26,which(colnames(duke.k.eth) != "Kidney.W")], 1, sum))


# UVA Ethnic plot
png("eth_liv.png", width=1500, height=900)
par(mfrow = c(1,3), ps=20)
#plot(uva.xplant$Year[-26], uva.k.e[,1], type = "l", ylim = c(min(uva.k.e), max(uva.k.e)), xlab = "Year", ylab = "No. of Transplants", main = "UVA Transplants by Ethnic Group")
plot(uva.xplant$Year[-26], uva.l.e[,1], type = "l", ylim = c(0,100), xlab = "Year", ylab = "No. of Transplants", main = "UVA Transplants by Ethnic Group")
lines(uva.xplant$Year[-26], uva.l.e[,2], col = "green")
legend(1990, 70, legend = c("White", "Other"), lwd = 2, col = c("black", "green"))

#plot(mcv.xplant$Year[-26], mcv.k.e[,1], type = "l", ylim = c(min(mcv.k.e), max(mcv.k.e)), xlab = "Year", ylab = "No. of Transplants", main = "MCV Transplants by Ethnic Group")
plot(mcv.xplant$Year[-26], mcv.l.e[,1], type = "l", ylim = c(0,100), xlab = "Year", ylab = "No. of Transplants", main = "MCV Transplants by Ethnic Group")
lines(mcv.xplant$Year[-26], mcv.l.e[,2], col = "green")
legend(1990, 80, legend = c("White", "Other"), lwd = 2, col = c("black", "green"))

#plot(duke.xplant$Year[-26], duke.k.e[,1], type = "l", ylim = c(min(duke.k.e), max(duke.k.e)), xlab = "Year", ylab = "No. of Transplants", main = "Duke Transplants by Ethnic Group")
plot(duke.xplant$Year[-26], duke.l.e[,1], type = "l", ylim = c(0,100), xlab = "Year", ylab = "No. of Transplants", main = "Duke Transplants by Ethnic Group")
lines(duke.xplant$Year[-26], duke.l.e[,2], col = "green")
legend(1990, 60, legend = c("White", "Other"), lwd = 2, col = c("black", "green"))
par(mfrow = c(1,1))
dev.off()


# UVA Ethnic plot
png("eth_kid.png", width=1500, height=900)
par(mfrow = c(1,3), ps=20)
#plot(uva.xplant$Year[-26], uva.k.e[,1], type = "l", ylim = c(min(uva.k.e), max(uva.k.e)), xlab = "Year", ylab = "No. of Transplants", main = "UVA Transplants by Ethnic Group")
plot(uva.xplant$Year[-26], uva.k.e[,1], type = "l", ylim = c(0,100), xlab = "Year", ylab = "No. of Transplants", main = "UVA Transplants by Ethnic Group")
lines(uva.xplant$Year[-26], uva.k.e[,2], col = "green")
legend(1990, 70, legend = c("White", "Other"), lwd = 2, col = c("black", "green"))

#plot(mcv.xplant$Year[-26], mcv.k.e[,1], type = "l", ylim = c(min(mcv.k.e), max(mcv.k.e)), xlab = "Year", ylab = "No. of Transplants", main = "MCV Transplants by Ethnic Group")
plot(mcv.xplant$Year[-26], mcv.k.e[,1], type = "l", ylim = c(0,100), xlab = "Year", ylab = "No. of Transplants", main = "MCV Transplants by Ethnic Group")
lines(mcv.xplant$Year[-26], mcv.k.e[,2], col = "green")
legend(1990, 80, legend = c("White", "Other"), lwd = 2, col = c("black", "green"))

#plot(duke.xplant$Year[-26], duke.k.e[,1], type = "l", ylim = c(min(duke.k.e), max(duke.k.e)), xlab = "Year", ylab = "No. of Transplants", main = "Duke Transplants by Ethnic Group")
plot(duke.xplant$Year[-26], duke.k.e[,1], type = "l", ylim = c(0,100), xlab = "Year", ylab = "No. of Transplants", main = "Duke Transplants by Ethnic Group")
lines(duke.xplant$Year[-26], duke.k.e[,2], col = "green")
legend(1990, 60, legend = c("White", "Other"), lwd = 2, col = c("black", "green"))
par(mfrow = c(1,1))
dev.off()

## What do you conclude from these demographic plots?



# Time series plots at the university medical centers

png("ts_eth_liv.png", width=900, height=1100)
par(mfrow = c(2,1), ps=20)
plot(uva.xplant$Year[-26], uva.l.e[,1], type = "l", ylim = c(10, 100), col = "orange", main = "Transplants for Whites", ylab = "Percent", xlab = "Year")
lines(uva.xplant$Year[-26], mcv.l.e[,1], type = "l", col = "purple")
lines(uva.xplant$Year[-26], duke.l.e[,1], type = "l", col = "blue3")
legend(1990, 100, legend = c("UVA", "MCV", "Duke"), lwd = 2, col = c("orange", "purple", "blue3"))

plot(uva.xplant$Year[-26], uva.l.e[,2], type = "l", ylim = c(0, 100), col = "orange", main = "Transplants for Non-Whites", ylab = "Percent", xlab = "Year")
lines(uva.xplant$Year[-26], mcv.l.e[,2], type = "l", col = "purple")
lines(uva.xplant$Year[-26], duke.l.e[,2], type = "l", col = "blue3")
legend(1990, 100, legend = c("UVA", "MCV", "Duke"), lwd = 2, col = c("orange", "purple", "blue3"))
par(mfrow = c(1,1))
dev.off()



png("ts_eth_kid.png", width=900, height=1100)
par(mfrow = c(2,1), ps=20)
plot(uva.xplant$Year[-26], uva.k.e[,1], type = "l", ylim = c(10, 100), col = "orange", main = "Transplants for Whites", ylab = "Percent", xlab = "Year")
lines(uva.xplant$Year[-26], mcv.k.e[,1], type = "l", col = "purple")
lines(uva.xplant$Year[-26], duke.k.e[,1], type = "l", col = "blue3")
legend(1990, 100, legend = c("UVA", "MCV", "Duke"), lwd = 2, col = c("orange", "purple", "blue3"))

plot(uva.xplant$Year[-26], uva.k.e[,2], type = "l", ylim = c(0, 100), col = "orange", main = "Transplants for Non-Whites", ylab = "Percent", xlab = "Year")
lines(uva.xplant$Year[-26], mcv.k.e[,2], type = "l", col = "purple")
lines(uva.xplant$Year[-26], duke.k.e[,2], type = "l", col = "blue3")
legend(1990, 100, legend = c("UVA", "MCV", "Duke"), lwd = 2, col = c("orange", "purple", "blue3"))
par(mfrow = c(1,1))
dev.off()


## What do you conclude from these time series plots?
uva is good at white people , but not good on non-white people



# ACF analysis
png("acf_eth_liv.png", width=1200, height=600)
par(mfrow = c(1,3), ps=20)
acf(uva.l.e[,2], main = "ACF UVA NW Liver")
acf(mcv.l.e[,2], main = "ACF MCV NW Liver")
acf(duke.l.e[,2], main = "ACF Duke NW Liver")
par(mfrow = c(1,1))
dev.off()

# PACF analysis
png("pacf_eth_liv.png", width=1200, height=600)
par(mfrow = c(1,3), ps=20)
pacf(uva.l.e[,2], main = "PACF UVA NW Liver")
pacf(mcv.l.e[,2], main = "PACF MCV NW Liver")
pacf(duke.l.e[,2], main = "PACF Duke NW Liver")
par(mfrow = c(1,1))
dev.off()


# ACF analysis
png("acf_eth_kid.png", width=1200, height=600)
par(mfrow = c(1,3), ps=20)
acf(uva.k.e[,2], main = "ACF UVA NW Kidney")
acf(mcv.k.e[,2], main = "ACF MCV NW Kidney")
acf(duke.k.e[,2], main = "ACF Duke NW Kidney")
par(mfrow = c(1,1))
dev.off()

# PACF analysis
png("pacf_eth_kid.png", width=1200, height=600)
par(mfrow = c(1,3), ps=20)
pacf(uva.k.e[,2], main = "PACF UVA NW Kidney")
pacf(mcv.k.e[,2], main = "PACF MCV NW Kidney")
pacf(duke.k.e[,2], main = "PACF Duke NW Kidney")
par(mfrow = c(1,1))
dev.off()
## What do you conclude from the acf and pacf plots?



# Classical tests

t.test(uva.l.e[-26,1], mcv.l.e[-26,1])
t.test(uva.l.e[-26,1], duke.l.e[-26,1])

t.test(uva.l.e[-26,2], mcv.l.e[-26,2])
t.test(uva.l.e[-26,2], duke.l.e[-26,2])


t.test(uva.k.e[-26,1], mcv.k.e[-26,1])
t.test(uva.k.e[-26,1], duke.k.e[-26,1])

t.test(uva.k.e[-26,2], mcv.k.e[-26,2])
t.test(uva.k.e[-26,2], duke.k.e[-26,2])

## What do you conclude from the t tests?


#******************************************************************
#
#  		Testing the Difference with MCV
#		Boostrapping & Simulation for the Difference
#
#******************************************************************

uvamcv <- uva.xplant$Kidney[-26]-mcv$Kidney[-26]

# Plot the difference between uva and mcv
plot(uva$Year[-26], uvamcv, col = "blue", type = "l", xlab = "Time", ylab = "MCV - UVA", main = "Difference between Kidney Transplants at UVA and MCV")
abline(h=0)

# Buidl a linear model uvamcv.lm to predict uvamcv with r11 donor

uvamcv.lm <- lm(uvamcv~r11donor$Kidney[-26])

summary(uvamcv.lm)

# diagnostics

par(mfrow = c(2,2))
plot(uvamcv.lm)
par(mfrow = c(1,1))

# Evaluate correlation of the model residuals
par(mfcol = c(1,2))
acf(uvamcv.lm$residuals)
pacf(uvamcv.lm$residuals)
par(mfcol = c(1,1))

# Buid a time series model of residuals using yule-walker method or mle
uvamcv.ar <- ar(uvamcv.lm$residuals)
uvamcv.ar

# Plot the aic for different numbers of ar terms
plot(uvamcv.ar$aic, type = "h")

##How many ar terms do we need? 


# Adding the time series model
# AR(1)

uvamcv.lm.e1 <- uvamcv.lm$resid[1:24] # new var..1 AR term put resid into AR
uvamcv.lm.eStupid <- uvamcv.lm$resid[1:25] # new var..1 AR term put resid into AR

r11k <- r11donor$Kidney[2:25] # need lag of use 1-24 to pred 2-25

uvamcv <- uva$Kidney[2:25] - mcv$Kidney[2:25] # lag from 1-24 to pred 2-25 diff

uvamcv.dm <- data.frame(uvamcv, r11k, uvamcv.lm.e1)

# Linear model with time series component
uvamcv.lm2<- lm(uvamcv~., data = uvamcv.dm)
summary(uvamcv.lm2)

# diagnostics

par(mfrow = c(2,2))
plot(uvamcv.lm2)
par(mfrow = c(1,1))
# this is $w_t$ may indicate white noise (MA) term

par(mfrow =c(1,2))
acf(uvamcv.lm2$residuals)
pacf(uvamcv.lm2$residuals)
par(mfrow =c(1,1))


##########################
# Forecasting R11 
# We need to forecast the r11 donors and the model residuals.
# Time series model of r11 donors

par(mfrow = c(2,1))
acf(r11donor$Kidney)
pacf(r11donor$Kidney) # AR(1) is good
par(mfrow = c(1,1))

# Time series model of R11 donors.
r11.ar <- ar(r11donor$Kidney[-26])

# Forecast of R11 donors
r11.ar.f <- forecast(r11.ar, h = 1) 

# Creating the new data data fame
uvamcv.nd <- data.frame(r11k = r11.ar.f$mean,uvamcv.lm.e1 = uvamcv.lm.e1[24])
# include r11 forecast and resid term and the use predict

# Predicting the linear model
predict(uvamcv.lm2, newdata = uvamcv.nd, se.fit = T)


# Bootstrap prediction
uvamcv.boot <- RFB(uvamcv.dm, model = uvamcv.lm2, ndata = uvamcv.nd, num = 2000 )

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