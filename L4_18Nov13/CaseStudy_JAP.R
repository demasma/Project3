#Set working directory

#Read data
r11xplant <- read.table("R11xplant.csv", sep = ",", header = T)
r11donor<-read.table("R11donor.csv", sep = ",", header = T)
uva <- read.table("UVAxplant.csv", sep = ",", header = T)
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

# Source the bootstrapping functions
library(boot) #If you don't have this library, install it by: install.packages('boot')
source("TSbootfunctions.R")

#***************************************************************
#
# Part 1: Basic Statistics (15 mins)
#
#***************************************************************

#Step 1.1 Get the distribution of uva$Liver, uva$Liver_DD, uva$Liver_LD. What do you observe? On average, how many liver transplants are performed at UVa per year? What about DD liver transplants and LD liver transplants?

par(mfrow=c(1,3))
hist(uva$Liver,col='steel blue',main='UVa Liver Transplants')
hist(uva$Liver_DD,col='steel blue',main='UVa Deceased-Donor Liver Transplants')
hist(uva$Liver_LD,col='steel blue',main='UVa Living-Donor Liver Transplants')
par(mfrow=c(1,1))

mean(uva$Liver)
mean(uva$Liver_DD)
mean(uva$Liver_LD)



#Step 1.2 Get the distribution of r11donor$Liver, r11donor$Liver_DD, r11donor$Liver_LD. What do you observe? On average, how many liver donors are there in R11 per year? What about DD liver donors and LD liver donors?

par(mfrow=c(1,3))
hist(r11donor$Liver,col='steel blue',main='R11 Liver Donors')
hist(r11donor$Liver_DD,col='steel blue',main='R11 Liver Donors (DD)')
hist(r11donor$Liver_LD,col='steel blue',main='R11 Liver Donors (LD)')
par(mfrow=c(1,1))

mean(r11donor$Liver)
mean(r11donor$Liver_D)
mean(r11donor$Liver_LD)

#Step 1.3 In step 1.1, we get the mean of uva$Liver_DD and uva$Liver_LD. What are the standard errors of the means? What're the 95% confidence intervals? Use bootstrapping to answer this question.

bs.mean<-function(x,i)
{
  return(mean(x[i]))
}

DDboot<-boot(uva$Liver_DD, bs.mean, R=2000)
LDboot<-boot(uva$Liver_LD, bs.mean, R=2000)

boot.ci(DDboot,0.95,type=c('bca','perc'))
boot.ci(LDboot,0.95,type=c('bca','perc'))


#Step 1.4 Use bootstrapping to test the hypothesis: there are more deceased liver donors than living liver donors in R11.

r11.liver.diff.bs<-boot(r11donor$Liver_DD-r11donor$Liver_LD,bs.mean,R=2000)
boot.ci(r11.liver.diff.bs,0.95,type=c('bca','perc'))

#Step 1.5 Get the scatter plot matrix with the above 6 variables. Describe what you observe. You can use either uva.pairs() {source("SPM_Panel.R")} or pairs().

source("SPM_Panel.R")
uva.pairs("~uva$Liver+uva$Liver_DD+uva$Liver_LD+r11donor$Liver+r11donor$Liver_DD+r11donor$Liver_LD")


#Step 1.6* Compare the performance of UVa with MCV



#***************************************************************
#
# Part 2: Linear Regression Models (10 mins)
#
#***************************************************************
#Step 2.1 Build a linear model: uva$Liver=b0+b1*r11donor$Liver+e. Call it uva.liver.lm.
# Analyze the result: R^2, model utility test, t-tests, etc.
uva.liver.lm<-lm(uva$Liver[-25]~r11donor$Liver[-25])
summary(uva.liver.lm)

#Step 2.2 Generate the diagnostic plots. Do you see any problem?
par(mfrow=c(2,2))
plot(uva.liver.lm)
par(mfrow=c(1,1))

#Step 2.3 Estimate the model with bootstrapping (by residuals). Is b1 significant?

#Step 2.3 Estimate the model with bootstrapping (by residuals). Is b1 significant?
# Get the fitted values from the regression model
uva.lfit <- fitted(uva.liver.lm)
# Get the residuals from the regression model
uva.le <- residuals(uva.liver.lm)
# Get the regression model
uva.mod <- model.matrix(uva.liver.lm)
# Bootstrapping LM
uva.liver.boot <- RTSB(uva$Liver[-25], r11donor$Liver[-25], uva.lfit, uva.le, uva.mod,5000)
uva.liver.boot$t
sqrt(var(uva.liver.boot$t))

# 95% CI of r11donor
boot.ci(uva.liver.boot, .95, index=2)

# Distribution of b1
par(mfrow = c(1,2))
hist(uva.liver.boot$t[,2], main = "Region 11 Donors",xlab ="Coefficient Values", col = "steelblue", breaks = 50)
qqnorm(uva.liver.boot $t[,2])
qqline(uva.liver.boot $t[,2])
par(mfrow = c(1,1))


#Step 2.4* What about MCV? Repeat the above steps and compare the coefficients. 



#***************************************************************
#
# Part 3: Time Series Models (20 mins)
#
#***************************************************************
#Step 3.1 Generate the ACF and PACF plots of the residuals from uva.liver.lm. What's your conclusion? 
plot(ts(uva.le))
par(mfrow = c(1,2))
acf(uva.le)
pacf(uva.le)
par(mfrow = c(1,1))

#Step 3.2 Based on the above ACF and PACF plots, what time series do you suggest to model the residuals?
# Fit an ar model to the residuals
# Let's try the AR(2) model first:
uva.liver.ts.ar2 <- lm(uva$Liver[3:25]~r11donor$Liver[3:25]+ uva.liver.lm$residuals[2:24] + uva.liver.lm$residuals[1:23])
summary(uva.liver.ts.ar2)
Michael Vedomske (Nov 20, 2013 2:50 PM EST) # The problem here is we have only a few observations (23). The confidence intervals are wide in ACF and PACF.

#Step 3.3 Let's use AR(1) to model the residuals. Add the AR(1) model of the residuals to regression linear model. Call this model uva.liver.ts. Analyze the regression results
uva.liver.ts <- lm(uva$Liver[2:25]~r11donor$Liver[2:25]+ uva.liver.lm$residuals[1:24])
summary(uva.liver.ts)

par(mfrow=c(2,2))
plot(uva.liver.ts)
par(mfrow=c(1,1))

#Step 3.4 Bootstrap the above time series model. Are the coefficients significant?
# Get the fitted values from the regression model
uva.lfit2 <- fitted(uva.liver.ts)
# Get the residuals from the regression model
uva.le2 <- residuals(uva.liver.ts)
# Get the regression model
uva.mod2 <- model.matrix(uva.liver.ts)
# Use the RTSB function to obtain the bootstrap
uva.liver.boot2 <- RTSB(uva$Liver[2:25], r11donor$Liver[2:25], uva.lfit2, uva.le2, uva.mod2,2000)
# The estimates
uva.liver.boot2
# Plot the results for the coeffiecient for region 11 donors
plot(uva.liver.boot2, index = 2)
# Plot the results for the coeffiecient for time series components
plot(uva.liver.boot2, index = 3)
Michael Vedomske (Nov 20, 2013 3:17 PM EST) #***************************************************************
#
# Part 4: UVA - MCV Differences in Liver Transplants (15 mins)
#
#***************************************************************
#Step 4.1 Use t-test to compare the liver transplants performed at UVa and MCV
uva.liver<-uva$Liver
mcv.liver<-mcv$Liver

t.test(uva.liver, mcv.liver,paired=T)

# Step 4.2 Build an AR model to predict the difference in 2012

diff.kid <- ts((uva.liver - mcv.liver), 1988, 2011)
diff.ar <- ar(diff.kid[1:23], method = "yule-walker") #uses yule walker

# Step 4.3 Bootstrapping the difference
# To obtain a bootstrap estimate of the prediction for 2011
# use the TSB function in the source file.

# It takes three arguments:
# tsint - the time series
# oth.arg - the data for the new estimate
# boot.number- number of replications (default=1000)

diff.pred <- predict(diff.ar, n.ahead = 2)
diff.boot1 <- TSB(diff.kid, diff.kid[24], 1000)
diff.boot2 <- TSB(diff.kid, diff.kid[25], 1000)
diff.boot3 <- TSB(diff.kid, diff.pred$pred, 1000)

#***************************************************************
#
# Part 5*: More Explanatory Variables
#
#***************************************************************