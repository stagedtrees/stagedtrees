#' Distances between probabilities
#'
#' @param x vector of probabilities.
#' @param y vector of probabilities.
#' @details Functions to compute distances between probabilities:
#' * \code{lp}: the \eqn{L^p} distance, \eqn{||x - y||_p^p} for \eqn{p = 1,2}
#' * \code{ry}: the symmetric Renyi divergence of order \eqn{\alpha = 2}
#' * \code{kl}: the symmetrized Kullback-Leibler divergence
#' * \code{tv}: the total variation or \eqn{L^1} norm
#' * \code{hl}: the (squared) Hellinger distance
#' * \code{bh}: the Bhattacharyya distance
#' * \code{cd}: the Chan-Darwiche distance
#' @return The distance between \code{p} and \code{q}
#' @name probdist
#' @keywords internal
NULL



#' @rdname probdist
#' @keywords internal
probdist.l2 <- function(x, y) {
  (sum(abs(x - y)^2))^(1 / 2)
}

#' @rdname probdist
#' @keywords internal
probdist.l1 <- function(x, y) {
  sum(abs(x - y))
}

#' @rdname probdist
#' @keywords internal
probdist.ry <- function(x, y) {
  rmix <- (x == 0) & (y == 0)
  x <- x[!rmix]
  y <- y[!rmix]
  alpha <- 2
  (log(sum(x^(alpha) / y^(alpha - 1))) +
    log(sum(y^(alpha) / x^(alpha - 1)))) / (alpha - 1)
}

#' @rdname probdist
#' @keywords internal
probdist.kl <- function(x, y) {
  rmix <- (x == 0) & (y == 0)
  x <- x[!rmix]
  y <- y[!rmix]
  return(sum(x * (log(x) - log(y))) +
    sum(y * (log(y) - log(x))))
}

#' @rdname probdist
#' @keywords internal
probdist.tv <- function(x, y) {
  sum(abs(x - y))
}

#' @rdname probdist
#' @keywords internal
probdist.hl <- function(x, y) {
  sum((sqrt(x) - sqrt(y))^2)
}

#' @rdname probdist
#' @keywords internal
probdist.bh <- function(x, y) {
  -log(sum(sqrt(x * y)))
}

#' @rdname probdist
#' @keywords internal
probdist.cd <- function(x, y) {
  rmix <- (x == 0) & (y == 0)
  x <- x[!rmix]
  y <- y[!rmix]
  log(max(x / y)) - log(min(x / y))
}

#' Compute the distance matrix
#'
#' Compute the matrix of distances between probabilities,
#' e.g the transition probabilities for a given variable in a
#' staged event tree.
#' @param x list of conditional probabilities for each stage.
#' @param distance the distance function e.g. \code{\link{probdist.kl}}.
#' @return The matrix with the distances between stages.
#' @keywords internal
distance_mat_stages <- function(x, distance = probdist.kl) {
  d <- length(x)
  M <- matrix(nrow = d, ncol = d, 0)
  for (i in 1:d) {
    for (j in 1:i) {
      M[i, j] <- distance(x[[i]], x[[j]])
    }
  }
  M <- M[lower.tri(M)]
  class(M) <- "dist"
  attr(M, "Size") <- length(x)
  attr(M, "Labels") <- names(x)
  return(M)
}
