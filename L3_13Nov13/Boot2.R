#***************************************************************
#
# Part 2- Bootstrap Regression and Time Series
#
#***************************************************************

##  Source the bootstrapping functions and load the 'boot' library


#Kidney Model:

##  Build a linear model, uva.kid.lm that predicts uva kidney transplants by region 11 kidney donors from 1988-2011 (exclude 2012)



##  Use summary to evaluate the model
##  Is your model significant?  r11donor$Kidney?



##  Plot diagnostics uva.kid.lm.  What do you examine?


#  Bootstrapping the linear model

##  Get the fitted values from the regression model uva.kid.lm and store in uva.kfit
##  use the fitted() function with the linear model as the option
uva.kfit <- fitted(uva.kid.lm)

##  Get the residuals from the regression model uva.kid.lm and store in uva.ke
##  Hint: use the residuals() function with the linear model as the option



#  Get the regression model for uva.kid.lm- model.matrix()
uva.mod <- model.matrix(uva.kid.lm)


##  Use the RTSB function (from TSbootfunctions.R) to obtain the bootstrap
##  distribution for the coefficients. This may take a few minutes.
##	Be patient.	

uva.kid.boot <- RTSB(uva$Kidney[-25], r11donor$Kidney[-25], uva.kfit, uva.ke, uva.mod,2000)


##  What are the estimates in uva.kid.boot? compare with summary on orignal model




##  Get the 99% CI for uva.kid.boot



##  Get the 95% CI for uva.kid.boot



#	Plot the results for the coeffiecient for region 11 donors
plot(uva.kid.boot, index = 2) 

##  A set of configurable plots for each term
par(mfrow = c(1,2))
hist(uva.kid.boot$t[,2], main = "Region 11 Donors",
     xlab ="Coefficient Values",   col = "steelblue", breaks = 50)
qqnorm(uva.kid.boot$t[,2])
qqline(uva.kid.boot$t[,2])
par(mfrow = c(1,1))

#  Bootstrapping TS

##  Evaluating residual correlation from the model uva.kid.lm
##  Hint: use the acf() and pcf()



##  Fit an ar model to the residuals using the yule-walker method
diff.ar <- ar(uva.kid.lm$residuals, method = "yule-walker") 

##  How many autoregressive terms are needed?



##  Add the ar model of the residuals to regression, linear model.
uva.kid.lm2<- lm(uva$Kidney[3:24]~r11donor$Kidney[3:24]+ uva.kid.lm$residuals[2:23] + uva.kid.lm$residuals[1:22])

##  Does the summary of the model uva.kid.lm2 still show siginificance for the donor time series

summary(uva.kid.lm2)

#	Bootstrap the above time series model:


##  Get the fitted values from the regression model, store in uva.kfit2



##  Get the residuals from the regression model, store in uva.ke2



##  Get the regression model, store in uva.mod2



##  Use the RTSB function as before on the new model to obtain the bootstrap 
##	distribution for the coefficients. This may take a few minutes.
##	Be patient.	Call it uva.kid.boot2


##  What are the estimates in uva.kid.boot2?
uva.kid.boot2

##  Plot the results for the coeffiecient for region 11 donors
plot(uva.boot2, index = 2)	

##  Plot the results for the coeffiecient for time series components
plot(uva.kid.boot2, index = 3)
plot(uva.kid.boot2, index = 4)
