data(iris)

summary(iris)
library(psych)
names(iris)

pairs.panels(iris[,1:4])

pl <- iris$Petal.Length
pl.b <- sample(x=pl, replace=T)
pl.b.samples <- as.list(rep(NA,500))

