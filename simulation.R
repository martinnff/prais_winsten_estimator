source("AR_p.R")

#Simulate AR2 time series

rho1 <- 0.6
rho2 <- 0.3
n <- 500

x <- runif(n, 0, 10)
y <- 0.5*x+arima.sim(n = n, list(ar = c(rho1, rho2) , sd = 0.3))
x <- matrix(x)
y <- matrix(y)
plot(x ,y)


# Obtain OLS residuals
temp_mod <- lm(y~x)

# Estimate AR parameters
rho <- rho_arp(temp_mod$residuals, order = 2)

# Create weight matrix

w <- diag(n)[]

w[1, 1] <- sqrt(1 - rho_est^2)
for (i in 1:(n-1)) {
    w[i + 1, i] <- -rho_est
}

xt <- w %*% x
yt <- w %*% y

temp_mod = lm(yt~-1+xt)
rho_est <- acf(temp_mod$residuals)$acf[2]
plot(xt ,yt)
rho_est