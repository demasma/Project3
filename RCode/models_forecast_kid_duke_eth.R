# TS Difference
uvaduke.e <- uva.e$Kidney.O - duke.e$Kidney.O
# LM Model
uvaduke.e.lm <- lm(uvaduke.e ~ r11donor$Kidney)
uvaduke.e.lm.e1 <- uvaduke.e.lm$resid[-26] # new var..1 AR term put resid into AR
r11k <- r11donor$Kidney[-26] # need lag of use 1-24 to pred 2-25
uvaduke.e.dm <- data.frame(uvaduke.e, r11k, uvaduke.e.lm.e1)
# Adding the time series model
# AR(1)
summary(uva.duke.e.lm)
AIC(uva.duke.e.lm)
lm.fitted <- fitted(uva.duke.e.lm)
lm.resid <- residuals(uva.duke.e.lm)
lm.model <- model.matrix(uva.duke.e.lm)
lm.boot <- RTSB(uva.duke.e, r11.donor$Kidney, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=2)

