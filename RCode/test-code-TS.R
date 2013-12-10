library(DAAG)
data(ACF1)
summary(ACF1)
plot(count ~ endtime, data=ACF1)

acf1 <- ACF1
acf1$endtime <- factor(acf1$endtime)
summary(acf1)

summary(ACF.glm0 <- glm(formula = count ~ endtime,
                        family = poisson, data = ACF1))

summary(ACF.glm1 <- glm(formula = count ~ endtime + I(endtime^2),
                        family = poisson, data = ACF1))

(etime <- unique(ACF1$endtime))
exp(-0.3215 + 0.1192 * etime)
exp(1.72235 - 0.26235*etime + 0.01514 * etime^2)
unique(fitted(ACF.glm0))
unique(fitted(ACF.glm1))

sum(resid(ACF.glm0, type="pearson")^2)/19
sum(resid(ACF.glm1, type="pearson")^2)/19

summary(ACFq.glm0 <- glm(formula = count ~ endtime,
                        family = quasipoisson, data = ACF1))
