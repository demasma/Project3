#Step 3.4 Bootstrap the above time series model. Are the coefficients significant?
# Get the fitted values from the regression model
uva.lfit2 <- fitted(uva.kid.glm4)
# Get the residuals from the regression model
uva.le2 <- residuals(uva.kid.glm4)
# Get the regression model
uva.mod2 <- model.matrix(uva.kid.glm4)
# Use the RTSB function to obtain the bootstrap
UVAL = uva$Kidney[3:25], R11D = r11donor$Kidney[3:25], 
LUVAL1 = uva.kid.res[2:24],
LUVAL2 = uva.kid.res[1:23],
Roan = Roan[3:25]
uva.liver.boot2 <- RTSB(, uva.lfit2, uva.le2, uva.mod2,50000)
# The estimates
uva.liver.boot2
# Plot the results for the coeffiecient for region 11 donors
plot(uva.liver.boot2, index = 1)
plot(uva.liver.boot2, index = 2)
# Plot the results for the coeffiecient for time series components
plot(uva.liver.boot2, index = 3)

plot(uva.liver.boot2, index = 2)
boot.ci(uva.liver.boot2,0.95,type=c('bca','perc'))
uva.duke.l.e.lm2

uva.duke.l.e.dm <- data.frame(uva.duke.l.e.ar1, r11k, uva.duke.l.e.lm.e1)
?RTSB