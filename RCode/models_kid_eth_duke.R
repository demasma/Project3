# TS Difference
uva.duke.e <- uva.k.e$Kidney.O - duke.k.e$Kidney.O

# TS Plot
png("./ts_diff_uva-duke.png", width=900, height=900)
par(mfrow=c(1,1), ps=20)
plot(uva.xplant$Year, uva.duke, col = "blue", type = "l", xlab = "Time", 
     ylab = "MCV - UVA", 
     main = "Difference between Kidney Transplants at UVA and Duke")
abline(h=0)
dev.off()

# LM Model
uva.duke.lm <- lm(uva.duke ~ r11.donor$Kidney)
summary(uva.duke.lm)

# LM Model Diagnostics
png("./lm_diag_diff_uva-duke.png", width=900, height=900)
par(mfrow = c(2,2),ps=20)
plot(uva.duke.lm)
par(mfrow = c(1,1))
dev.off()

# ACF/PACF of Residuals
png("./acf_diff_uva-duke.png", width=900, height=900)
par(mfcol = c(1,2),ps = 20)
acf(uva.duke.lm$residuals)
pacf(uva.duke.lm$residuals)
par(mfcol = c(1,1))
dev.off()

# AR Suggestion
(uva.duke.ar <- ar(uva.duke.lm$residuals))

# AIC for Different AR Lags
png("./air_ar_diff_uva-duke.png", width=900, height=900)
par(mfcol = c(1,1),ps = 20)
plot(uva.duke.ar$aic, type = "h")
dev.off()