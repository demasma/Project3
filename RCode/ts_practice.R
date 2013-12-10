library(DAAG)

data(LakeHuron)
plot(LakeHuron, ylab="Depth (ft)", xlab="Time (years)")
lag.plot(LakeHuron, lags=3, do.lines=FALSE)
acf(LakeHuron)
pacf(LakeHuron)
ar(LakeHuron, method="mle") # uses AIC

library(forecast)
auto.arima(LakeHuron)

acf(resid(arima(LakeHuron, order=c(p=1, d=1, q=2))))
pacf(resid(arima(LakeHuron, order=c(p=1, d=1, q=2))))
