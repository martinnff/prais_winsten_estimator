source("functions.R")

#Simulate AR3 time series

rho1 <- 0.5
rho2 <- 0.3
rho3 <- 0.15
beta <- 0.7
n <- 400

x <- runif(n, 0, 10)
y <- beta * x + arima.sim(n = n, list(ar = c(rho1, rho2, rho3), sd = 0.3))

series <- data.frame(x, y)
plot(series)


mod = prais_winsten(formula = "y ~ -1 + x",
              order = 3,
              tol = 0.000001,
              index = series$x,
              data = series)

mod

lm(y ~ -1 + x, series)