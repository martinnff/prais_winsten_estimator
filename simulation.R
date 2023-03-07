source("functions.R")

#Simulate AR2 time series

rho1 <- 0.6
rho2 <- 0.3
beta <- 0.5
n <- 5000

x <- runif(n, 0, 10)
y <- beta * x + arima.sim(n = n, list(ar = c(rho1, rho2), sd = 0.3))

series <- data.frame(x, y)
plot(series)


# Obtain OLS residuals
temp_mod <- lm(y ~ x, data = series)

# Estimate AR parameters
rho_est <- rho_arp(temp_mod$residuals, order = 2)

# obtain the prais winsten transform using the estimated parameters
series_t <- pw_transform(series, rho_est)

# Estimate the model parameters
temp_mod2 <- lm(y ~ -1 + x, data = series_t)

# coefficient estimations without the transformation and
# with the transformation
temp_mod$coef
temp_mod2$coef