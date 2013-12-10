mcv.k.e
uva.k.e

# TS Difference
uva.mcv.e <- uva.k.e$Kidney.O - mcv.k.e$Kidney.O 

# TS Plot
png("./ts_diff_uva-mcv_kid_eth.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.mcv.e, col = "blue", type = "l", xlab = "Time", 
     ylim=c(-80,10),
     ylab = "MCV - UVA", 
     main = "Difference between Kidney Transplants at UVA and MCV for 
     Non-White Patients")
abline(h=0)
dev.off()

# LM Model
uva.mcv.e.lm <- lm(uva.mcv.e ~ r11.donor$Kidney)
summary(uva.mcv.e.lm)

# Diagnostics
png("./lm_diag_diff_uva-mcv_kid_eth.png", width=900, height=900)
par(mfrow = c(2,2), ps=20)
plot(uva.mcv.e.lm)
par(mfrow = c(1,1))
dev.off()

# ACF PACF
png("./acf_diff_uva-mcv_kid_eth.png", width=900, height=900)
par(mfcol = c(1,2), ps=20)
acf(uva.mcv.e.lm$residuals)
pacf(uva.mcv.e.lm$residuals)
par(mfcol = c(1,1))
dev.off()

(uva.mcv.e.ar <- ar(uva.mcv.e.lm$residuals))

# Plot the aic for different numbers of ar terms
png("./aic_ar_diff_uva-mcv_kid_eth.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.mcv.e.ar$aic, type = "h") # need 
dev.off()

# Adding the time series model
# AR(1)

uva.mcv.e.lm.e1 <- uva.mcv.e.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor$Kidney[2:25] # need lag of use 1-24 to pred 2-25

uva.mcv.e.ar1 <- uva.k.e$Kidney.O[2:25] - mcv.k.e$Kidney.O[2:25] # lag from 1-24 to pred 2-25 diff

uva.mcv.e.dm <- data.frame(uva.mcv.e.ar1, r11k, uva.mcv.e.lm.e1)

summary(uva.mcv.e.dm)

# Linear model with time series component
uva.mcv.e.lm2<- lm(uva.mcv.e.ar1 ~ ., data = uva.mcv.e.dm)
AIC(uva.mcv.e.lm2)
summary(uva.mcv.e.lm2)
lm.fitted <- fitted(uva.mcv.e.lm2)
lm.resid <- residuals(uva.mcv.e.lm2)
lm.model <- model.matrix(uva.mcv.e.lm2)
lm.boot <- RTSB(uva.mcv.e.ar1, r11k, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=2)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=3)

# diagnostics

png("./lm-ar1_diag_diff_uva-mcv_kid_eth.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.mcv.e.lm2)
par(mfrow = c(1,1))
# this is $w_t$ may indicate white noise (MA) term
dev.off()

png("./lm-ar1_acf_diff_uva-mcv_kid_eth.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.mcv.e.lm2$residuals)
pacf(uva.mcv.e.lm2$residuals)
par(mfrow =c(1,1))
dev.off()

#=======================================================
# UVa-Duke TS LM model
#=======================================================
# Adding the time series model
# AR(4)

uva.duke.lm.e1 <- uva.duke.lm$resid[4:24] # new var..1 AR term put resid into AR
uva.duke.lm.e2 <- uva.duke.lm$resid[3:23] # new var..1 AR term put resid into AR
uva.duke.lm.e3 <- uva.duke.lm$resid[2:22] # new var..1 AR term put resid into AR
uva.duke.lm.e4 <- uva.duke.lm$resid[1:21] # new var..1 AR term put resid into AR

r11k <- r11.donor$Kidney[5:25] # need lag of use 1-24 to pred 2-25

uva.duke.ar4 <- uva.xplant$Kidney[5:25] - duke.xplant$Kidney[5:25] # lag from 1-24 to pred 2-25 diff

uva.duke.dm <- data.frame(uva.duke.ar4, r11k, uva.duke.lm.e1,uva.duke.lm.e2,uva.duke.lm.e3,uva.duke.lm.e4)

summary(uva.mcv.dm)

# Linear model with time series component
uva.duke.lm2<- lm(uva.duke.ar4 ~ ., data = uva.duke.dm)
summary(uva.duke.lm2)

# diagnostics

png("./lm-ar4_diag_diff_uva-duke.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.lm2)
par(mfrow = c(1,1))
# this is $w_t$ may indicate white noise (MA) term
dev.off()

png("./lm-ar4_acf_diff_uva-duke.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.duke.lm2$residuals)
pacf(uva.duke.lm2$residuals)
par(mfrow =c(1,1))
dev.off()

#----------------------
# UVa-Duke TS LM model
#----------------------
# Adding the time series model
# AR(5)

uva.duke.lm.e1 <- uva.duke.lm$resid[5:24] 
uva.duke.lm.e2 <- uva.duke.lm$resid[4:23] 
uva.duke.lm.e3 <- uva.duke.lm$resid[3:22] 
uva.duke.lm.e4 <- uva.duke.lm$resid[2:21] 
uva.duke.lm.e5 <- uva.duke.lm$resid[1:20] 

r11k <- r11.donor$Kidney[6:25] # need lag of use 1-24 to pred 2-25

uva.duke.ar5 <- uva.xplant$Kidney[6:25] - duke.xplant$Kidney[6:25] # lag from 1-24 to pred 2-25 diff

uva.duke.dm <- data.frame(uva.duke.ar5, r11k, uva.duke.lm.e1,uva.duke.lm.e2,uva.duke.lm.e3,uva.duke.lm.e4, uva.duke.lm.e5)

# Linear model with time series component
uva.duke.lm3<- lm(uva.duke.ar5 ~ ., data = uva.duke.dm)
summary(uva.duke.lm3)

# diagnostics
png("./lm-ar5_diag_diff_uva-duke.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.lm3)
par(mfrow = c(1,1))
# this is $w_t$ may indicate white noise (MA) term
dev.off()

png("./lm-ar5_acf_diff_uva-duke.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.duke.lm3$residuals)
pacf(uva.duke.lm3$residuals)
par(mfrow =c(1,1))
dev.off()

