# TS Difference
uva.mcv.l <- uva.xplant$Liver - mcv.xplant$Liver

# TS Plot
png("./ts_diff_uva_mcv_l.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.mcv.l, col = "blue", type = "l", 
     xlab = "Time", 
     ylab = "UVa - MCV", 
     main = "Difference between Kidney Transplants at UVA and MCV")
abline(0,0)
dev.off()

# LM Model
uva.mcv.l.lm <- lm(uva.mcv.l ~ r11.donor$Liver)
summary(uva.mcv.l.lm)

# LM Model Diagnostics
png("./lm_diag_diff_uva_mcv_l.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.mcv.l.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_uva_mcv_l.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf(uva.mcv.l.lm$residuals)
pacf(uva.mcv.l.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
(uva.mcv.l.ar <- ar(uva.mcv.l.lm$residuals))

# AIC for Different AR Lags
png("./aic_ar_diff_uva_mcv_l.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.mcv.l.ar$aic, type = "h")
dev.off()

# Adding the time series model
# AR(1)

uva.mcv.l.lm.e1 <- uva.mcv.l.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor$Liver[2:25] # need lag of use 1-24 to pred 2-25

uva.mcv.l.ar1 <- uva.xplant$Liver[2:25] - mcv.xplant$Liver[2:25] # lag from 1-24 to pred 2-25 diff

uva.mcv.l.dm <- data.frame(uva.mcv.l.ar1, r11k, uva.mcv.l.lm.e1)

summary(uva.mcv.l.dm)

# Linear model with time series component
uva.mcv.l.lm2<- lm(uva.mcv.l.ar1 ~ ., data = uva.mcv.l.dm)
summary(uva.mcv.l.lm2)
AIC(uva.mcv.l.lm2)
lm.fitted <- fitted(uva.mcv.l.lm2)
lm.resid <- residuals(uva.mcv.l.lm2)
lm.model <- model.matrix(uva.mcv.l.lm2)
lm.boot <- RTSB(uva.mcv.l.ar1, r11k, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=2)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=3)
# diagnostics

png("./lm-ar1_diag_diff_uva_mcv_l.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.mcv.l.lm2)
par(mfrow = c(1,1))
dev.off()

png("./lm-ar1_acf_diff_uva_mcv_l.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.mcv.l.lm2$residuals)
pacf(uva.mcv.l.lm2$residuals)
par(mfrow =c(1,1))
dev.off()
