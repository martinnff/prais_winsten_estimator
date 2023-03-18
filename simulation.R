source("functions.R")

#Simulate AR3 time series
set.seed(123)
rho1 <- 0.6
rho2 <- 0.3

b <- 0.7
a <- 5
n <- 1000
x <- runif(n, 0, 10)
y <- a + b * x + arima.sim(n = n, list(ar = c(rho1,rho2), sd = 0.5))

series <- data.frame(x, y)

prais_winsten("x~y", series,
    response = y, predictors = x, order = 2, tol = 1e-5)

# simulation
nsim <- 200
params <- list(n = c(50, 100, 200, 600, 1000),
            beta = c(0.1, 0.5, 1),
            beta0 = c(0, 1, 4),
            std = 1,
            rho = list(rho1 = c(0.7, 0.2),
                       rho2 = c(0.5, 0.4)))

params <- expand.grid(params)
results <- data.frame(lm_r2_m = rep(0, nrow(params)),
                     pw_r2_m = rep(0, nrow(params)),
                     lm_r2_std = rep(0, nrow(params)),
                     pw_r2_std = rep(0, nrow(params)),
                     lm_beta1_m = rep(0, nrow(params)),
                     pw_beta1_m = rep(0, nrow(params)),
                     lm_beta1_std = rep(0, nrow(params)),
                     pw_beta1_std = rep(0, nrow(params)),
                     lm_beta0_m = rep(0, nrow(params)),
                     pw_beta0_m = rep(0, nrow(params)),
                     lm_beta0_std = rep(0, nrow(params)),
                     pw_beta0_std = rep(0, nrow(params)),
                     lm_mse_m = rep(0, nrow(params)),
                     pw_mse_m = rep(0, nrow(params)),
                     lm_mse_std = rep(0, nrow(params)),
                     pw_mse_std = rep(0, nrow(params)))

for (i in 1:nrow(params)) {
    print(paste("param: ", as.character(i), sep = ""))
    lm_r2 <- 0
    pw_r2 <- 0
    lm_beta1 <- 0
    pw_beta1 <- 0
    lm_beta0 <- 0
    pw_beta0 <- 0
    lm_mse <- 0
    pw_mse <- 0

    n <- params[i, 1]
    beta1 <- params[i, 2]
    beta0 <- params[i, 3]
    std <- params[i, 4]
    rho1 <- unlist(params[i, 5])[1]
    rho2 <- unlist(params[i, 5])[2]


    for (j in 1:nsim) {
        #print(paste("sim: ", as.character(j), sep = ""))
        x <- runif(n, 0, 10)
        y <- beta0 + beta1 * x + arima.sim(n = n,
         list(ar = c(rho1, rho2),
          sd = std))
        series <- data.frame(x, y)
        # Fit lm using prais winsten
        mod1 <- prais_winsten(formula = "y ~ x",
                    order = 2,
                    tol = 0.0001,
                    predictors = series$x,
                    response = series$y,
                    data = series)
        # fit lm using OLS
        mod2 <- lm(y ~ x, series)
        lm_r2[j] <- summary(mod2)$r.squared
        pw_r2[j] <- summary(mod1$model)$r.squared
        lm_beta1[j] <- mod2$coefficients[2]
        pw_beta1[j] <- mod1$model$coefficients[2]
        lm_beta0[j] <- mod2$coefficients[1]
        pw_beta0[j] <- mod1$model$coefficients[1]
        lm_mse[j] <- sum(mod2$residuals^2) / length(mod2$residuals)
        pw_mse[j] <- sum(mod1$model$residuals^2) / length(mod1$model$residuals)
    }
    results[i, 1] <- round(mean(lm_r2), 4)
    results[i, 2] <- round(mean(pw_r2), 4)
    results[i, 3] <- round(sd(lm_r2), 4)
    results[i, 4] <- round(sd(lm_r2), 4)
    results[i, 5] <- round(mean(lm_beta1), 4)
    results[i, 6] <- round(mean(pw_beta1), 4)
    results[i, 7] <- round(sd(lm_beta1), 4)
    results[i, 8] <- round(sd(pw_beta1), 4)
    results[i, 9] <- round(mean(lm_beta0), 4)
    results[i, 10] <- round(mean(pw_beta0), 4)
    results[i, 11] <- round(sd(lm_beta0), 4)
    results[i, 12] <- round(sd(pw_beta0), 4)
    results[i, 13] <- round(mean(lm_mse), 4)
    results[i, 14] <- round(mean(pw_mse), 4)
    results[i, 15] <- round(sd(lm_mse), 4)
    results[i, 16] <- round(sd(pw_mse), 4)

}
results <- cbind(params, results)
results$rho <- as.character(results$rho)
write.csv(results, "results.csv")