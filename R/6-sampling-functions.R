#' Sample from a staged event tree
#'
#' Generate a random sample from the distribution encoded
#' in a staged event tree object.
#' @param object the staged event tree object
#' @param nsim number of observations to sample
#' @param seed an object specifying if and how the random number generator 
#'             should be initialized (‘seeded’). Either NULL or an integer 
#'             that will be used in a call to set.seed. 
#' @details It samples \code{n} observations according to
#' the transition probabilities (\code{object$prob}) in the model.
#' @return A data frame containing a sample of size \code{n}
#' @examples
#' model <- fbhc(full(PhDArticles, lambda = 1))
#' sample_from(model, 10)
#' @export
sample_from <- function(object, nsim = 1, seed = NULL) {
  stopifnot(nsim > 0)
  stopifnot(is(object, "sevt"))
  stopifnot(has_prob(object))
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  if (is.null(seed)) 
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  p <- length(object$tree)
  # extract var names as vars
  vars <- names(object$tree)
  # create empty array with colnames == vars
  S <- array(
    dim = c(nsim, p),
    data = NA,
    dimnames = list(NULL, vars)
  )
  # sample from the distribution of the first variable
  S[, vars[1]] <-
    sample(
      object$tree[[vars[1]]],
      replace = TRUE,
      size = nsim,
      prob = object$prob[[vars[1]]][[1]]
    )
  # sequentially sample the other variables
  for (i in 2:p) {
    for (j in 1:nsim) {
      # find the corresponding stage
      stage <- find_stage(object, S[j, 1:(i - 1)])
      # sample from the conditional prob of the stage
      S[j, i] <- sample(object$tree[[vars[i]]],
        size = 1,
        prob = object$prob[[vars[i]]][[stage]]
      )
    }
  }
  S <- as.data.frame(S)
  attr(S, "seed") <- RNGstate
  S
}
