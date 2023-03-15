source("functions.R")

#Simulate AR3 time series

rho1 <- 0.2
rho2 <- 0.3
rho3 <- 0.15
beta <- 0.7
n <- 200
x <- runif(n, 0, 10)
y <- beta * x + arima.sim(n = n, list(ar = c(rho1, rho2, rho3), sd = 0.5))

series <- data.frame(x, y)

# Fit lm using prais winsten
mod1 <- prais_winsten(formula = "y ~ -1 + x",
              order = 3,
              tol = 0.0001,
              index = series$x,
              data = series)

# fit lm using OLS
mod2 <- lm(y ~ -1 + x, series)



plot(series, pch = 20, axes = FALSE)
lines(series$x, series$x * mod1$beta,
    col = '#8FCFBB', lwd = 2)
lines(series$x, series$x * mod2$coef,
    col = '#BF616A', lwd = 2)
axis(1)
axis(2)
legend(x = "bottomright",          # Position
       legend = c("Prais Winsten", "OLS"),  # Legend texts
       fill = c("#8FCFBB", "#BF616A"))


summary(mod1$model)
summary(mod2)$r.squared


# simulation
nsim <- 100
params <- list(n = c(50, 100, 200, 600, 1000),
            beta = c(0.1, 0.5, 1),
            std = c(0.1, 0.4, 1),
            rho = list(rho1 = c(0.3, 0.3, 0.3),
                       rho2 = c(0.5, 0.2, 0.1)))

params <- expand.grid(params)
results <- data.frame(lm_r2_m = rep(0, nrow(params)),
                     pw_r2_m = rep(0, nrow(params)),
                     lm_r2_std = rep(0, nrow(params)),
                     pw_r2_std = rep(0, nrow(params)),
                     lm_beta_m = rep(0, nrow(params)),
                     pw_beta_m = rep(0, nrow(params)),
                     lm_beta_std = rep(0, nrow(params)),
                     pw_beta_std = rep(0, nrow(params)),
                     lm_mse_m = rep(0, nrow(params)),
                     pw_mse_m = rep(0, nrow(params)),
                     lm_mse_std = rep(0, nrow(params)),
                     pw_mse_std = rep(0, nrow(params)))

for (i in 1:nrow(params)) {
    print(paste("param: ", as.character(i), sep = ""))
    lm_r2 <- 0
    pw_r2 <- 0
    lm_beta <- 0
    pw_beta <- 0
    lm_mse <- 0
    pw_mse <- 0

    n <- params[i, 1]
    beta <- params[i, 2]
    std <- params[i, 3]
    rho1 <- unlist(params[i, 4])[1]
    rho2 <- unlist(params[i, 4])[2]
    rho3 <- unlist(params[i, 4])[3]

    for (j in 1:nsim) {
        print(paste("sim: ", as.character(j), sep = ""))
        x <- runif(n, 0, 10)
        y <- beta * x + arima.sim(n = n,
         list(ar = c(rho1, rho2, rho3),
          sd = 0.3))
        series <- data.frame(x, y)
        # Fit lm using prais winsten
        mod1 <- prais_winsten(formula = "y ~ -1 + x",
                    order = 3,
                    tol = 0.0001,
                    index = series$x,
                    data = series)
        # fit lm using OLS
        mod2 <- lm(y ~ -1 + x, series)
        lm_r2[j] <- summary(mod2)$r.squared
        pw_r2[j] <- summary(mod1$model)$r.squared
        lm_beta[j] <- mod2$coefficients
        pw_beta[j] <- mod1$beta
        lm_mse[j] <- sum(mod2$residuals^2) / length(mod2$residuals)
        pw_mse[j] <- sum(mod1$model$residuals^2) / length(mod1$model$residuals)
    }
    results[i, 1] <- round(mean(lm_r2), 4)
    results[i, 2] <- round(mean(pw_r2), 4)
    results[i, 3] <- round(sd(lm_r2), 4)
    results[i, 4] <- round(sd(lm_r2), 4)
    results[i, 5] <- round(mean(lm_beta), 4)
    results[i, 6] <- round(mean(pw_beta), 4)
    results[i, 7] <- round(sd(lm_beta), 4)
    results[i, 8] <- round(sd(pw_beta), 4)
    results[i, 9] <- round(mean(lm_mse), 4)
    results[i, 10] <- round(mean(pw_mse), 4)
    results[i, 11] <- round(sd(lm_mse), 4)
    results[i, 12] <- round(sd(pw_mse), 4)

}
results <- cbind(params, results)
results$rho <- as.character(results$rho)
write.csv(results, "results.csv")