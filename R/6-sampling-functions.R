#' Sample from a staged event tree
#'
#' Generate a random sample from the distribution encoded
#' in a staged event tree object.
#' @param object the staged event tree object
#' @param n number of observations to sample
#' @details It samples \code{n} observations according to
#' the transition probabilities (\code{object$prob}) in the model.
#' @return A data frame containing a sample of size \code{n}
#' @examples
#' model <- naive.sevt(full(PhDArticles, lambda = 1))
#' sample.sevt(model, 10)
#' @export
sample.sevt <- function(object, n = 1) {
  stopifnot(n > 0)
  stopifnot(is(object, "sevt"))
  stopifnot(is_fitted.sevt(object))
  p <- length(object$tree)
  # extract var names as vars
  vars <- names(object$tree)
  # create empty array with colnames == vars
  S <- array(
    dim = c(n, p),
    data = NA,
    dimnames = list(NULL, vars)
  )
  # sample from the distribution of the first variable
  S[, vars[1]] <-
    sample(
      object$tree[[vars[1]]],
      replace = TRUE,
      size = n,
      prob = object$prob[[vars[1]]][[1]]
    )
  # sequentially sample the other variables
  for (i in 2:p) {
    for (j in 1:n) {
      # find the corresponding stage
      stage <- find_stage(object, S[j, 1:(i - 1)])
      # sample from the conditional prob of the stage
      S[j, i] <- sample(object$tree[[vars[i]]],
        size = 1,
        prob = object$prob[[vars[i]]][[stage]]
      )
    }
  }
  return(as.data.frame(S))
}
