# TS Difference
uva.mcv.l.e <- uva.l.e$Liver.O - mcv.l.e$Liver.O

# TS Plot
png("./ts_diff_uva_mcv_l_e.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.mcv.l.e, col = "blue", type = "l", 
     xlab = "Time", 
     ylab = "UVa - MCV", 
     main = "Difference between Liver Transplants at UVA and MCV for Non-White Patients")
abline(0,0)
dev.off()

# LM Model
uva.mcv.l.e.lm <- lm(uva.mcv.l.e ~ r11.donor$Liver)
summary(uva.mcv.l.e.lm)

# LM Model Diagnostics
png("./lm_diag_diff_uva_mcv_l_e.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.mcv.l.e.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_uva_mcv_l_e.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf(uva.mcv.l.e.lm$residuals)
pacf(uva.mcv.l.e.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
(uva.mcv.l.e.ar <- ar(uva.mcv.l.e.lm$residuals))

# AIC for Different AR Lags
png("./aic_ar_diff_uva_mcv_l_e.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.mcv.l.e.ar$aic, type = "h")
dev.off()

# Adding the time series model
# AR(1)

uva.mcv.l.e.lm.e1 <- uva.mcv.l.e.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor$Liver[2:25] # need lag of use 1-24 to pred 2-25

uva.mcv.l.e.ar1 <- uva.l.e$Liver.O[2:25] - mcv.l.e$Liver.O[2:25] # lag from 1-24 to pred 2-25 diff

uva.mcv.l.e.dm <- data.frame(uva.mcv.l.e.ar1, r11k, uva.mcv.l.e.lm.e1)

summary(uva.mcv.l.e.dm)

# Linear model with time series component
uva.mcv.l.e.lm2<- lm(uva.mcv.l.e.ar1 ~ ., data = uva.mcv.l.e.dm)
summary(uva.mcv.l.e.lm2)

# diagnostics

png("./lm-ar1_diag_diff_uva_mcv_l_e.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.mcv.l.e.lm2)
par(mfrow = c(1,1))
dev.off()

png("./lm-ar1_acf_diff_uva_mcv_l_e.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.mcv.l.e.lm2$residuals)
pacf(uva.mcv.l.e.lm2$residuals)
par(mfrow =c(1,1))
dev.off()
