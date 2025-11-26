x <- rnorm(1000, mean = 20, sd = 20)
y <- rpois(1000, lambda = 4)
plot(x, log(y+1))
m <- glm(y~x, family = "poisson")

hist(log(y+1))
