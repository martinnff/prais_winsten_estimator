
# Rho estimation for AR1
rho_ar1 <- function(y) {
    n <- length(y)
    sum(y[2:n] * y[1:(n - 1)]) / sum(y[2:(n - 1)]^2)
}


# Rho estimation for AR2
rho_ar2 <- function(y) {
    n <- length(y)
    mat1 <- matrix(ncol = 2, nrow = 2)
    mat1[1, 1] <- sum(y[2:(n - 1)]^2)
    mat1[2, 1] <- mat1[1, 2] <- sum(y[3:(n - 1)] * y[2:(n - 2)])
    mat1[2, 2] <- sum(y[3:(n - 2)]^2)
    mat2 <- matrix(ncol = 1, nrow = 2)
    mat2[1, 1] <- sum(y[2:n] * y[1:(n - 1)])
    mat2[2, 1] <- sum(y[3:n] * y[1:(n - 2)])

    solve(mat1) %*% mat2
}

restrict_parameters <- function(rho) {
    for (i in seq_len(length(rho))) {
        if (rho[i] > 1) {
            rho[i] <- 1
        }
        if (rho[i] < -1) {
            rho[i] <- -1
        }
    }
    return(rho)
}

# Rho estimation for AR with order p
rho_arp <- function(y, order) {
    n <- length(y)
    mat1 <- matrix(ncol = order, nrow = order)
    mat2 <- matrix(ncol = 1, nrow = order)
    mat1[1, 1] <- sum(y[2:(n - 1)]^2)
    for (i in 1:order) {
        for (j in (i + 1):order) {
            t1 <- (2 * order - 1)
            t2 <- (2 * order)
                if (i < order) {
                    mat1[i, i] <- sum(y[(t1 - order + 1):(n - order + 1)]^2)
                    mat1[i, j] <- mat1[j, i] <-
                        sum(y[(t2 - order + 1):(n - order + 1)] *
                        y[(t2 - order):(n - order)])
                }else {
                    mat1[i, i] <- sum(y[t1:(n - order)]^2)
                }
            }
        mat2[i, 1] <- sum(y[(i + 1):n] * y[1:(n - i)])
        }

    restrict_parameters((solve(mat1) %*% mat2))
    }

pw_transform <- function(data, rho) {
    rho <- unlist(rho)
    data_t <- as.matrix(data)
    order <- length(rho)
    for (i in seq_len(order)) {
        n <- nrow(data_t)
        data_t <- data_t[(i + 1):n, ] - c(rho[i]) * data_t[1:(n - i), ]
    }
    as.data.frame(data_t)
}



get_bic <- function(model, order) {
    df_ll <- order + length(model$coef) + 1
    n <- length(model$residuals)
    w <- rep(1, n)
    res <- model$residuals
    ll <- 0.5 * (sum(log(w)) - n *
        (log(2 * pi) + 1 - log(n) +
        log(sum(w * res^2))))
    (-2 * ll + log(n) * df_ll)
}


prais_winsten <- function(formula, data,
    response, predictors, order, tol = 1e-5) {
    update <- TRUE
    names <- 0
    niter <- 1
    while (update) {
        if (niter > 1) {
            data_t <- pw_transform(data, rho[niter, ])
        } else {
            data_t <- data
        }

        temp_mod <- lm(as.formula(formula), data = data_t)

        if (niter > 1) {

            temp_mod$coefficients[1] <- mean(response - c(temp_mod$coefficients[-1])*predictors)

        }

        res <- predict(temp_mod, as.data.frame(predictors)) - response
        rho_est <- unlist(unname(ar(res, aic = FALSE, order.max = order)[2]))
        rho_est <- data.frame(t(rho_est))
        if (niter <= 1) { # initialize dataframe with rho values
            rho <- data.frame(matrix(data = 0, ncol = length(rho_est)))
            for (i in seq_len(ncol(rho))) {
                names[i] <- paste("rho_", as.character(i), sep = "")
            }
            colnames(rho) <- names
        }else { # check for convergence and update dataframe
            for (i in seq_len(nrow(rho))) {
                if (all(abs(rho_est - rho[i, ]) < tol)) {
                    update <- FALSE
                }
            }
        }
        colnames(rho_est) <- names
        rho <- rbind(rho, rho_est)
        niter <- niter + 1
    }

    bic <- get_bic(temp_mod, ncol(rho))
    colnames(rho) <- names
    list(rho = rho[-nrow(rho), ],
        model = temp_mod,
        BIC = bic)
}
