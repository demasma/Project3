#***************************************************************
#
#      Lecture 22: Simulation & Bootstrapping
#     	 A Case Study: Transplant Center
#   		   
#***************************************************************


#***************************************************************
#
#  Read in the data
#
#***************************************************************

#Set working directory

#Read data
r11xplant <- read.table("R11xplant.csv", sep = ",", header = T)
r11donor<-read.table("R11donor.csv", sep = ",", header = T)
uva <- read.table("UVAxplant.csv", sep = ",", header = T)
mcv <- read.table("MCVxplant.csv", sep = ",", header = T)

#    Source the bootstrapping functions
library(boot) #If you don't have this library, install it by: install.packages('boot')
source("TSbootfunctions.R")

#***************************************************************
#
# Part 1: Basic Statistics (15 mins)
#
#***************************************************************

#Step 1.1 Get the distribution of uva$Liver, uva$Liver_DD, uva$Liver_LD. What do you observe? On average, how many liver transplants are performed at UVa per year? What about DD liver transplants and LD liver transplants?


#Step 1.2 Get the distribution of r11donor$Liver, r11donor$Liver_DD, r11donor$Liver_LD. What do you observe? On average, how many liver donors are there in R11 per year? What about DD liver donors and LD liver donors?


#Step 1.3 In step 1.1, we get the mean of uva$Liver_DD and uva$Liver_LD. What are the standard errors of the means? What're the 95% confidence intervals? Use bootstrapping to answer this question.


#Step 1.4 Use bootstrapping to test the hypothesis: there are more deceased liver donors than living liver donors in R11.


#Step 1.5 Get the scatter plot matrix with the above 6 variables. Describe what you observe. You can use either uva.pairs() {source("SPM_Panel.R")} or pairs().


#Step 1.6* Compare the performance of UVa with MCV



#***************************************************************
#
# Part 2: Linear Regression Models (10 mins)
#
#***************************************************************
#Step 2.1 Build a linear model: uva$Liver=b0+b1*r11donor$Liver+e. Call it uva.liver.lm.
#   	   Analyze the result: R^2, model utility test, t-tests, etc.


#Step 2.2 Generate the diagnostic plots. Do you see any problem?


#Step 2.3 Estimate the model with bootstrapping (by residuals). Is b1 significant?

#Step 2.3 Estimate the model with bootstrapping (by residuals). Is b1 significant?
#    Get the fitted values from the regression model

#    Get the residuals from the regression model

#    Get the regression model


#   Bootstrapping LM


#    95% CI of r11donor


#    Distribution of b1



#Step 2.4* What about MCV? Repeat the above steps and compare the coefficients. 



#***************************************************************
#
# Part 3: Time Series Models (20 mins)
#
#***************************************************************
#Step 3.1 Generate the ACF and PACF plots of the residuals from uva.liver.lm. What's your conclusion?  


#Step 3.2 Based on the above ACF and PACF plots, what time series do you suggest to model the residuals?
#    Fit an ar model to the residuals
 

##For practice fit a model with 2 autoregressor


#Step 3.3 Let's use AR(1) to model the residuals. Add the AR(1) model of the residuals to regression linear model. Call this model uva.liver.ts. Analyze the regression results


#Generate the diagnostic plots for uva.liver.ts


#Step 3.4 Bootstrap the above time series model (uva.liver.ts). Are the coefficients significant?
#    Get the fitted values from the regression model

#    Get the residuals from the regression model

#    Get the regression model

#     Use the RTSB function to obtain the bootstrap

#     The estimates

#    Plot the results for the coeffiecient for region 11 donors

#    Plot the results for the coeffiecient for time series components



#***************************************************************
#
# Part 4: UVA - MCV Differences in Liver Transplants (15 mins)
#
#***************************************************************
#Step 4.1 Use t-test to compare the liver transplants performed at UVa and MCV


# Step 4.2 Build an AR model to predict the difference in 2011 & 2012


# Step 4.3 Bootstrapping the difference
#    To obtain a bootstrap estimate of the prediction for 2011
#    use the TSB function in the source file.

#     It takes three arguments:
#    tsint - the time series
#    oth.arg - the data for the new estimate
#    boot.number- number of replications (default=1000)



#***************************************************************
#
# Part 5*: More Explanatory Variables
#
#***************************************************************