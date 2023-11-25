#' initialize_uzawa
#' @param A
#' @param b
#' @param C
#' @param d
#' @param x0
#' @param y0
#' @param rho
#' @param alpha
#' @param max_iter
#' @param tol
#'@export

#'@example
#' A <- matrix(c(1, 2, 3, 4), nrow = 2)
#' b <- c(5, 6)
#' C <- matrix(c(1, 1), nrow = 1)
#' d <- 1
#' x0 <- c(0, 0)
#' y0 <- c(0)
#' rho <- 1
#' alpha <- 0.1
#' max_iter <- 1000
#' tol <- 1e-6
#' initialize_uzawa
initialize_uzawa <- function (A, b, C, d, x0, y0, rho, alpha, max_iter, tol) {
  x <- x0
  y <- y0
  for (i in 1:max_iter) {
    x_prev <- x
    y_prev <- y
    # Update x
    x <- x - alpha * (A %*% x - b + t(C) %*% y)
    # Update y
    y <- y + rho * (C %*% x - d)
    # Check for convergence
    if (sqrt(sum((x - x_prev)^2)) < tol && sqrt(sum((y - y_prev)^2)) < tol) {
      break
    }
  }
  list(x = x, y = y)
}

