#' noisy xor function
#'
#' @param x a vector of +1 and -1.
#' @param eps the uniform noise amount.
#' @return the computed noisy xor.
#' @keywords internal
#' @importFrom stats runif
noisy_xor <- function(x, eps = 0) {
  return(sign(prod(x) + runif(
    n = 1, min = -eps, max = eps
  )))
}

#' Generate a xor dataset
#'
#' @param p number of variables.
#' @param n number of observations.
#' @param eps error.
#' @return The xor dataset with \code{n} + 1 variables, where the first one is
#' the class variable \code{C} computed as a noisy xor.
#'
#' @export
#' @importFrom stats runif
#' @examples
#' DD <- generate_xor_dataset(p = 5, n = 1000, eps = 1.2)
generate_xor_dataset <- function(p,
                                 n,
                                 eps = 1.2) {
  DD <- data.frame(observation = 1:n)
  for (i in 1:p) {
    DD[[paste("X", i, sep = "")]] <- sample(
      c(-1, +1),
      prob = runif(2, min = 0, max = 1),
      size = n,
      replace = TRUE
    )
  }
  DD$C <- apply(
    DD,
    MARGIN = 1,
    FUN = function(x) {
      return(noisy_xor(x = x[-1], eps = eps))
    }
  )
  DD <- DD[, -1]
  for (i in 1:(p + 1)) {
    DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
  }
  return(DD[, c(p + 1, 1:p)])
}

#' Generate a random binary dataset for classification
#'
#' Randomly generate a simple classification problem.
#' @param p number of variables.
#' @param n number of observations.
#' @param eps noise.
#' @param gamma numeric.
#' @param alpha numeric vector of length \code{n}.
#'
#' @return A data.frame with \code{n} independent random variables and
#'  one class variable \code{C} computed as
#'  \code{sign(sum(x * alpha) + runif(1, -eps, eps) + gamma)}.
#' @export
#' @importFrom stats runif
#' @examples
#' DD <- generate_linear_dataset(p = 5, n = 1000)
generate_linear_dataset <-
  function(p,
           n,
           eps = 1.2,
           gamma = runif(1, min = -p, max = p),
           alpha = runif(p, min = -p, max = p)) {
    DD <- data.frame(observation = 1:n)
    for (i in 1:p) {
      DD[[paste("X", i, sep = "")]] <- sample(c(-1, +1),
        prob = runif(2),
        size = n,
        replace = TRUE
      )
    }
    DD$C <- apply(
      DD,
      MARGIN = 1,
      FUN = function(x) {
        return(sign(
          sum(x[-1] * alpha) + runif(
            n = 1,
            min = -eps,
            max = eps
          ) + gamma
        ))
      }
    )
    DD$C[DD$C == 0] <- 1
    DD <- DD[, -1]
    for (i in 1:(p + 1)) {
      DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
    }
    return(DD[, c(p + 1, 1:p)])
  }

#' Generate a random binary dataset
#'
#' Randomly generate a data.frame of independent binary variables.
#' @param p number of variables.
#' @param n number of observations.
#' @return A data.frame with \code{n} independent random variables.
#' @export
#' @importFrom stats runif
#' @examples
#' DD <- generate_random_dataset(p = 5, n = 1000)
generate_random_dataset <-
  function(p,
           n) {
    DD <- data.frame(observation = 1:n)
    for (i in 1:p) {
      DD[[paste("X", i, sep = "")]] <- sample(c(-1, +1),
        prob = runif(2),
        size = n,
        replace = TRUE
      )
    }
    DD <- DD[, -1]
    for (i in 1:(p)) {
      DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
    }
    return(DD)
  }
