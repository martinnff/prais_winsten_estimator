library(dplyr)
results <- read.csv("results2.csv")[-1, ]

names(results)

beta1_sim <- 1 # 0.1, 0.5, 1
beta0_sim <- 4
rho <- list(rho1 = "c(0.7, 0.2)",
           rho2 = "c(0.5, 0.4)")

data <- results %>%
    filter(beta == beta1_sim) %>%
    filter(beta0 == beta0_sim) %>%
    filter(rho == rho[[2]])


par(mfrow=c(1, 2))
jitt = 5
plot(data$n, data$pw_beta1_m, col = "#8FCFBB", pch = 20,
    ylim = c(0.95*min(c(data$pw_beta1_m, data$lm_beta1_m)),
        max(c(data$pw_beta1_m, data$lm_beta1_m)) / 0.95),
        xlab = "Nº Samples", ylab = "Beta_1")
points(data$n + jitt, data$lm_beta1_m,
    col = "#BF616A", pch = 20)
segments(x0 = data$n, y0 = data$pw_beta1_m,
    x1 = data$n, y1 = data$pw_beta1_m + data$pw_beta1_std / 2,
    col = "#8FCFBB")
segments(x0 = data$n, y0 = data$pw_beta1_m,
    x1 = data$n, y1 = data$pw_beta1_m - data$pw_beta1_std / 2,
    col = "#8FCFBB")
segments(x0 = data$n + jitt, y0 = data$lm_beta1_m,
    x1 = data$n + jitt, y1 = data$lm_beta1_m + data$lm_beta1_std / 2,
    col = "#BF616A")
segments(x0 = data$n + jitt, y0 = data$lm_beta1_m,
    x1 = data$n + jitt, y1 = data$lm_beta1_m - data$lm_beta1_std / 2,
    col = "#BF616A")
abline(h = beta1_sim)
axis(1)
axis(2)
legend(x = "bottomright",
       legend = c("Prais Winsten", "OLS", "True beta_1"),
       fill = c("#8FCFBB", "#BF616A", "#000000"))

plot(data$n, data$pw_beta0_m, col = "#8FCFBB", pch = 20,
    ylim = c(0.95 * min(c(data$pw_beta0_m - data$pw_beta0_std,
                            data$lm_beta0_m - data$lm_beta0_std)),
                    max(c(data$pw_beta0_m + data$pw_beta0_std,
                            data$lm_beta0_m + data$lm_beta0_std)) / 0.95),
    xlab = "Nº Samples", ylab = "Beta_0")
points(data$n + jitt, data$lm_beta0_m,
    col = "#BF616A", pch = 20)

segments(x0 = data$n, y0 = data$pw_beta0_m,
    x1 = data$n, y1 = data$pw_beta0_m + data$pw_beta0_std / 2,
    col = "#8FCFBB")
segments(x0 = data$n, y0 = data$pw_beta0_m,
    x1 = data$n, y1 = data$pw_beta0_m - data$pw_beta0_std / 2,
    col = "#8FCFBB")
segments(x0 = data$n + jitt, y0 = data$lm_beta0_m,
    x1 = data$n + jitt, y1 = data$lm_beta0_m + data$lm_beta0_std / 2,
    col = "#BF616A")
segments(x0 = data$n + jitt, y0 = data$lm_beta0_m,
    x1 = data$n + jitt, y1 = data$lm_beta0_m - data$lm_beta0_std / 2,
    col = "#BF616A")
abline(h = beta0_sim)
axis(1)
axis(2)
legend(x = "bottomright",
       legend = c("Prais Winsten", "OLS", "True beta_0"),
       fill = c("#8FCFBB", "#BF616A", "#000000"))