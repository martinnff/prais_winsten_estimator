
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


# Rho estimation for AR_p
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
                    mat1[i, j] <- mat1[j, i] <- sum(y[(t2 - order + 1):(n - order + 1)] * y[(t2 - order):(n - order)])
                }else {
                    mat1[i, i] <- sum(y[t1:(n - order)]^2)
                }
            }
        mat2[i, 1] <- sum(y[(i + 1):n] * y[1:(n - i)])
        }

    return(solve(mat1) %*% mat2)
    }

