library(DAAG)
data(rice)
summary(rice)

summary(xplant)

xplant.aov <- aov(Liver ~ r11.donor.Liver + Location, data = Xplant)
xplant.aov <- aov(Liver ~ LiverDonorR11 + Location, data = Xplant)
Xplant[!complete.cases(Xplant),]
summary(xplant.aov)
summary(r11.donor)

#=============================================================================
# I did not handle the input of data very carefully in this case.
#=============================================================================
> xplant.aov <- aov(Liver ~ r11.donor.Liver + Location, data = Xplant)
> summary(xplant.aov)
Df Sum Sq Mean Sq F value   Pr(>F)    
r11.donor.Liver  1   8555    8555  49.743 7.83e-10 ***
  Location         2   1026     513   2.983   0.0568 .  
Residuals       74  12726     172                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
51 observations deleted due to missingness


#=============================================================================
# I only used MCV, Duke, and UVa to create the xplant data frame here.
#=============================================================================
> xplant.aov <- aov(Liver ~ LiverDonorR11 + Location, data = Xplant)
> summary(xplant.aov)
Df Sum Sq Mean Sq F value   Pr(>F)    
LiverDonorR11  1   8518    8518  48.933 1.06e-09 ***
  Location       2   1044     522   2.999    0.056 .  
Residuals     73  12707     174                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Xplant$Location = factor(Xplant$Location, levels=c("uva", "mcv", "duke"),
                         ordered = TRUE)
xplant.aov <- aov(Liver ~ LiverDonorR11 + Location, data = Xplant[-26,])
summary.lm(xplant.aov)
summary(xplant.aov)
summary(Xplant)
Xplant$Location = factor(Xplant$Location, levels=c("uva", "mcv", "duke"),
                         ordered = FALSE)
summary(Xplant)

