uva.duke <- uva.xplant$Kidney - duke.xplant$Kidney
uva.duke.lm <- lm(uva.duke ~ r11.donor$Kidney)
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
# Linear model with time series component
uva.duke.lm2<- lm(uva.duke.ar4 ~ ., data = uva.duke.dm)

summary(uva.duke.lm2)

lm.fitted <- fitted(uva.duke.lm2)
lm.resid <- residuals(uva.duke.lm2)
lm.model <- model.matrix(uva.duke.lm2)
lm.boot <- RTSB(uva.duke.ar4, r11k, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=2)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=3)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=4)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=5)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=6)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=7)

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
AIC(uva.duke.lm3)
summary(uva.duke.lm3)
lm.fitted <- fitted(uva.duke.lm3)
lm.resid <- residuals(uva.duke.lm3)
lm.model <- model.matrix(uva.duke.lm3)
lm.boot <- RTSB(uva.duke.ar5, r11k, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=2)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=3)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=4)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=5)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=6)
boot.ci(lm.boot,0.95,type=c('bca','perc'), index=7)


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

