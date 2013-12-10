# TS Difference
uva.duke.l.e <- uva.l.e$Liver.O - duke.l.e$Liver.O

# TS Plot
png("./ts_diff_uva_duke_l_e.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.duke.l.e, col = "blue", type = "l", 
     xlab = "Time", 
     ylab = "UVa - Duke", 
     main = "Difference between Liver Transplants at UVA and Duke for Non-White Patients")
abline(0,0)
dev.off()

# LM Model
uva.duke.l.e.lm <- lm(uva.duke.l.e ~ r11.donor$Liver)
summary(uva.duke.l.e.lm)
AIC(uva.duke.l.e.lm)
lm.fitted <- fitted(uva.duke.l.e.lm)
lm.resid <- residuals(uva.duke.l.e.lm)
lm.model <- model.matrix(uva.duke.l.e.lm)
lm.boot <- RTSB(uva.duke.l.e, r11.donor$Liver, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=2)
# LM Model Diagnostics
png("./lm_diag_diff_uva_duke_l_e.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.l.e.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_uva_duke_l_e.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf(uva.duke.l.e.lm$residuals)
pacf(uva.duke.l.e.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
(uva.duke.l.e.ar <- ar(uva.duke.l.e.lm$residuals))
uva.duke.l.e.ar$aic
# AIC for Different AR Lags
png("./aic_ar_diff_uva_duke_l_e.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.duke.l.e.ar$aic, type = "h")
dev.off()

# Adding the time series model
# AR(1)

uva.duke.l.e.lm.e1 <- uva.duke.l.e.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor$Liver[2:25] # need lag of use 1-24 to pred 2-25

uva.duke.l.e.ar1 <- uva.l.e$Liver.O[2:25] - duke.l.e$Liver.O[2:25] # lag from 1-24 to pred 2-25 diff

uva.duke.l.e.dm <- data.frame(uva.duke.l.e.ar1, r11k, uva.duke.l.e.lm.e1)

summary(uva.duke.l.e.dm)

# Linear model with time series component
uva.duke.l.e.lm2<- lm(uva.duke.l.e.ar1 ~ ., data = uva.duke.l.e.dm)
summary(uva.duke.l.e.lm2)

# diagnostics

png("./lm-ar1_diag_diff_uva_duke_l_e.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.l.e.lm2)
par(mfrow = c(1,1))
dev.off()

png("./lm-ar1_acf_diff_uva_duke_l_e.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.duke.l.e.lm2$residuals)
pacf(uva.duke.l.e.lm2$residuals)
par(mfrow =c(1,1))
dev.off()
