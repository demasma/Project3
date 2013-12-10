# TS Difference
uva.duke.l <- uva.xplant$Liver - duke.xplant$Liver

# TS Plot
png("./ts_diff_uva_duke_l.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.duke.l, col = "blue", type = "l", 
     xlab = "Time", 
     ylab = "UVa - Duke", 
     main = "Difference between Kidney Transplants at UVA and Duke")
abline(0,0)
dev.off()

# LM Model
uva.duke.l.lm <- lm(uva.duke.l ~ r11.donor$Liver)
summary(uva.duke.l.lm)

# LM Model Diagnostics
png("./lm_diag_diff_uva_duke_l.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.l.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_uva_duke_l.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf(uva.duke.l.lm$residuals)
pacf(uva.duke.l.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
(uva.duke.l.ar <- ar(uva.duke.l.lm$residuals))

# AIC for Different AR Lags
png("./aic_ar_diff_uva_duke_l.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.duke.l.ar$aic, type = "h")
dev.off()

# Adding the time series model
# AR(1)

uva.duke.l.lm.e1 <- uva.duke.l.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor$Liver[2:25] # need lag of use 1-24 to pred 2-25

uva.duke.l.ar1 <- uva.xplant$Liver[2:25] - duke.xplant$Liver[2:25] # lag from 1-24 to pred 2-25 diff

uva.duke.l.dm <- data.frame(uva.duke.l.ar1, r11k, uva.duke.l.lm.e1)

summary(uva.duke.l.dm)

# Linear model with time series component
uva.duke.l.lm2<- lm(uva.duke.l.ar1 ~ ., data = uva.duke.l.dm)
summary(uva.duke.l.lm2)
AIC(uva.duke.l.lm2)
lm.fitted <- fitted(uva.duke.l.lm2)
lm.resid <- residuals(uva.duke.l.lm2)
lm.model <- model.matrix(uva.duke.l.lm2)
lm.boot <- RTSB(uva.duke.l.ar1, r11k, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=2)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=3)
# diagnostics

png("./lm-ar1_diag_diff_uva_duke_l.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.l.lm2)
par(mfrow = c(1,1))
dev.off()

png("./lm-ar1_acf_diff_uva_duke_l.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.duke.l.lm2$residuals)
pacf(uva.duke.l.lm2$residuals)
par(mfrow =c(1,1))
dev.off()
