# TS Difference
uva.duke.e <- uva.k.e$Kidney.O - duke.k.e$Kidney.O

# TS Plot
png("./ts_diff_uva-duke_kid_eth.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.duke.e, col = "blue", type = "l", ylim=c(-50,10),
     xlab = "Time", 
     ylab = "MCV - UVA", 
     main = "Difference between Kidney Transplants at UVA and Duke\n
     for non-white patients")
abline(0,0)
dev.off()

# LM Model
uva.duke.e.lm <- lm(uva.duke.e ~ r11.donor$Kidney)
summary(uva.duke.e.lm)

# LM Model Diagnostics
png("./lm_diag_diff_uva-duke_kid_eth.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.e.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_uva-duke_kid_eth.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf(uva.duke.e.lm$residuals)
pacf(uva.duke.e.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
(uva.duke.e.ar <- ar(uva.duke.e.lm$residuals))
uva.duke.e.ar$aic
# AIC for Different AR Lags
png("./aic_ar_diff_uva-duke_kid_eth.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.duke.e.ar$aic, type = "h")
dev.off()

# Adding the time series model
# AR(1)

uva.duke.e.lm.e1 <- uva.duke.e.lm$resid[1:24] # new var..1 AR term put resid into AR

r11k <- r11.donor$Kidney[2:25] # need lag of use 1-24 to pred 2-25

uva.duke.e.ar1 <- uva.k.e$Kidney.O[2:25] - duke.k.e$Kidney.O[2:25] # lag from 1-24 to pred 2-25 diff

uva.duke.e.dm <- data.frame(uva.duke.e.ar1, r11k, uva.duke.e.lm.e1)

summary(uva.duke.e.dm)

# Linear model with time series component
uva.duke.e.lm2<- lm(uva.duke.e.ar1 ~ ., data = uva.duke.e.dm)
summary(uva.duke.e.lm2)

# diagnostics

png("./lm-ar1_diag_diff_uva-duke_kid_eth.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.e.lm2)
par(mfrow = c(1,1))
# this is $w_t$ may indicate white noise (MA) term
dev.off()

png("./lm-ar1_acf_diff_uva-duke_kid_eth.png", width=900, height=900)
par(mfrow =c(1,2),ps=20)
acf(uva.duke.e.lm2$residuals)
pacf(uva.duke.e.lm2$residuals)
par(mfrow =c(1,1))
dev.off()
