#!/usr/bin/python

models = {\
'modelname': "uva.duke.l.e", \
'dfin2':     "duke.l.e$Liver.O", \
'dfin1':     "uva.l.e$Liver.O", \
'file':      "uva_duke_l_e", \
'title':     "Duke for Non-White Patients", \
'center2':   'Duke', \
'organ':     'Liver'}

print """
# TS Difference
{modelname} <- {dfin1} - {dfin2}

# TS Plot
png("./ts_diff_{file}.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, {modelname}, col = "blue", type = "l", 
     xlab = "Time", 
     ylab = "UVa - {center2}", 
     main = "Difference between {organ} Transplants at UVA and {title}")
abline(0,0)
dev.off()

# LM Model
{modelname}.lm <- lm({modelname} ~ r11.donor${organ})
summary({modelname}.lm)

# LM Model Diagnostics
png("./lm_diag_diff_{file}.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot({modelname}.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_{file}.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf({modelname}.lm$residuals)
pacf({modelname}.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
({modelname}.ar <- ar({modelname}.lm$residuals))

# AIC for Different AR Lags
png("./aic_ar_diff_{file}.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot({modelname}.ar$aic, type = "h")
dev.off()

# Adding the time series model
# AR(1)

{modelname}.lm.e1 <- {modelname}.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor${organ}[2:25] # need lag of use 1-24 to pred 2-25

{modelname}.ar1 <- {dfin1}[2:25] - {dfin2}[2:25] # lag from 1-24 to pred 2-25 diff

{modelname}.dm <- data.frame({modelname}.ar1, r11k, {modelname}.lm.e1)

summary({modelname}.dm)

# Linear model with time series component
{modelname}.lm2<- lm({modelname}.ar1 ~ ., data = {modelname}.dm)
summary({modelname}.lm2)

# diagnostics

png("./lm-ar1_diag_diff_{file}.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot({modelname}.lm2)
par(mfrow = c(1,1))
dev.off()

png("./lm-ar1_acf_diff_{file}.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf({modelname}.lm2$residuals)
pacf({modelname}.lm2$residuals)
par(mfrow =c(1,1))
dev.off()
""".format(**models) 
