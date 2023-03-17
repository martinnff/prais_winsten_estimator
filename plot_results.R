library(dplyr)
results <- read.csv("results.csv")[-1, ]

names(results)

beta_sim <- 1 # 0.1, 0.5, 1
std_sim <- 1 # 0.1, 0.4, 1
rho <- list(rho1 = "c(0.3, 0.3, 0.3)",
           rho2 = "c(0.5, 0.2, 0.1)")

data <- results %>%
    filter(beta == beta_sim) %>%
    filter(std == std_sim) %>%
    filter(rho == rho[[2]])



plot(data$n, data$pw_beta_m, col = "#8FCFBB", pch = 20,
    ylim = c(max(c(data$pw_beta_m, data$lm_beta_m)),
        min(c(data$pw_beta_m, data$lm_beta_m))),
        xlab = "Nº Samples", ylab = "Beta")

points(data$n, data$lm_beta_m,
    col = "#BF616A", pch = 20)
abline(h = beta_sim)
axis(1)
axis(2)
legend(x = "bottomright",
       legend = c("Prais Winsten", "OLS", "True beta"),
       fill = c("#8FCFBB", "#BF616A", "#000000"))