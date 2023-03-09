
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
    data <- as.matrix(data)
    n <- nrow(data)
    w <- diag(n)
    for (i in seq_len(length(rho))) {
        p <- sum(rho[i:length(rho)]^2)
        if (p > 1){
            p <- 1
        }
        if (p < -1){
            p <- -1
        }
        w[i, i] <- sqrt(1 - p)
    }
    for (i in 1:n) {
        for (j in seq_len(length(rho))){
            if ((i + j) < n) {
                w[i + j, i] <- -rho[j]
            }
        }
    }
    as.data.frame(w %*% data)
}


get_bic <- function(model, order) {
    n <- length(model$residuals)
    mse <- sum(model$residuals^2) / length(model$residuals)
    (order + length(model$coef)) * log(n) - 2 * log(mse)
}



prais_winsten <- function(formula, data, index, order, tol = 1e-5) {
    update <- TRUE
    names <- 0
    beta <- FALSE
    for (i in 1:order) {
        names[i] <- paste("rho_", as.character(i), sep = "")
    }
    rho <- data.frame(
        matrix(
            rep(1000, order),
            nrow = 1,
            ncol = order))
    colnames(rho) <- names
    while (update) {
        if (beta != FALSE) {
            res <-  matrix(beta %*% index) - matrix(data$y)
        }else {
            res <- stats::lm(as.formula(formula), data = data)$residuals
        }
        rho_est <- rho_arp(res, order)
        data_t <- pw_transform(data, rho_est)
        temp_mod <- stats::lm(as.formula(formula), data = data_t)
        beta <- temp_mod$coef
        rho_est <- data.frame(t(rho_est))
        colnames(rho_est) <- names

        for (i in seq_len(nrow(rho))) {
            if (all(abs(rho_est - rho[i, ]) < tol)) {
                update <- FALSE
            }
        }
        rho <- rbind(rho_est, rho)
    }
    bic <- get_bic(temp_mod, order)

    list(rho = rho[-nrow(rho), ],
        beta = beta,
        model = temp_mod,
        BIC = bic)
}
