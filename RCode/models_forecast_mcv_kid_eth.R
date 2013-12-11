
uva.e <- data.frame(uva.k.eth[, "Kidney.W"], Kidney.O = apply(uva.k.eth[,which(colnames(uva.k.eth) != "Kidney.W")], 1, sum))
mcv.e <- data.frame(mcv.k.eth[, "Kidney.W"], Kidney.O = apply(mcv.k.eth[,which(colnames(mcv.k.eth) != "Kidney.W")], 1, sum))
duke.e <- data.frame(duke.k.eth[, "Kidney.W"], Kidney.O = apply(duke.k.eth[,which(colnames(duke.k.eth) != "Kidney.W")], 1, sum))

# TS Difference
uvamcv.e <- uva.e$Kidney.O - mcv.e$Kidney.O 
# LM Model
uvamcv.e.lm <- lm(uvamcv.e ~ r11donor$Kidney)
# Adding the time series model
# AR(1)
uvamcv.e.lm.e1 <- uvamcv.e.lm$resid[1:24] # new var..1 AR term put resid into AR
r11k <- r11donor$Kidney[2:25] # need lag of use 1-24 to pred 2-25
uvamcv.e <- uva.k.e$Kidney.O[2:25] - mcv.k.e$Kidney.O[2:25] # lag from 1-24 to pred 2-25 diff
uvamcv.e.dm <- data.frame(uvamcv.e, r11k, uvamcv.e.lm.e1)

# Linear model with time series component
uvamcv.e.lm2<- lm(uvamcv.e ~ ., data = uvamcv.e.dm)
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

