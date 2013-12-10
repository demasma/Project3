dnorm(100,)
?dnorm
Pop.A <- rnorm(10000, 50, 30)
Pop.B <- rnorm(10000, 50, 30)

wilcox.test(Pop.A,Pop.B)
t.test(Pop.A, Pop.B)

hist(Pop.A)
hist(Pop.B)
