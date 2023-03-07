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

# Fit lm using prais winsten
mod1 <- prais_winsten(formula = "y ~ -1 + x",
              order = 3,
              tol = 0.0001,
              index = series$x,
              data = series)

# fit lm using OLS
mod2 <- lm(y ~ -1 + x, series)


plot(series, pch=20, axes = F)
lines(series$x, series$x * mod1$beta,
    col = '#8FCFBB', lwd = 2)
lines(series$x, series$x * mod2$coef,
    col = '#BF616A', lwd = 2)
axis(1)
axis(2)
legend(x = "bottomright",          # Position
       legend = c("Prais Winsten", "OLS"),  # Legend texts
       fill = c("#8FCFBB","#BF616A"))


summary(mod1$model)
summary(mod2)