

uvamcv <- uva$Kidney[-26] - mcv$Kidney[-26]
uvamcv.lm <- lm(uvamcv~r11donor$Kidney[-26])
# Adding the time series model
# AR(1)
uvamcv.lm.e1 <- uvamcv.lm$resid[1:24] # new var..1 AR term put resid into AR
r11k <- r11donor$Kidney[2:25] # need lag of use 1-24 to pred 2-25
uvamcv <- uva$Kidney[2:25] - mcv$Kidney[2:25] # lag from 1-24 to pred 2-25 diff
uvamcv.dm <- data.frame(uvamcv, r11k, uvamcv.lm.e1)
# Linear model with time series component
uvamcv.lm2<- lm(uvamcv ~ ., data = uvamcv.dm)
summary(uva.mcv.lm2)
AIC(uva.mcv.lm2)
lm.fitted <- fitted(uva.mcv.lm2)
lm.resid <- residuals(uva.mcv.lm2)
lm.model <- model.matrix(uva.mcv.lm2)
lm.boot <- RTSB(uva.mcv.ar1, r11k, lm.fitted, lm.resid, lm.model, 5000)
lm.boot
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=1)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=2)
boot.ci(lm.boot,0.95,type=c('bca','perc'),index=3)


