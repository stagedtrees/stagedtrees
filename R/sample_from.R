#' Sample from a staged event tree
#'
#' Generate a random sample from the distribution encoded
#' in a staged event tree object.
#' @param object an object of class \code{sevt} with fitted probabilities.
#' @param size number of observations to sample.
#' @param seed an object specifying if and how the random number generator
#'             should be initialized (‘seeded’). Either NULL or an integer
#'             that will be used in a call to set.seed.
#' @details It samples \code{size} observations according to
#' the transition probabilities (\code{object$prob}) in the model.
#' @return A data frame containing \code{size} observations from the
#' variables in \code{object}.
#' @examples
#' model <- stages_fbhc(full(PhDArticles, lambda = 1))
#' sample_from(model, 10)
#' @export
sample_from <- function(object, size = 1, seed = NULL) {
  if (!is.numeric(size)){
    cli::cli_abort(c(
      "{.arg size} must be a positive integer.",
      "x" = "You've supplied {.arg size} which is {.type {size}}."
    ))
  }
  if (size <= 0){
    cli::cli_abort(c(
      "{.arg size} should be a positive integer.",
      "x" = "You've supplied {.arg size} which is {size}."
    ))
  }
  check_sevt_prob(object)
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    runif(1)
  }
  if (is.null(seed)) {
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  } else {
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
    dim = c(size, p),
    data = NA,
    dimnames = list(NULL, vars)
  )
  # sample from the distribution of the first variable
  S[, vars[1]] <-
    sample(
      object$tree[[vars[1]]],
      replace = TRUE,
      size = size,
      prob = object$prob[[vars[1]]][[1]]
    )
  # sequentially sample the other variables
  for (i in 2:p) {
    for (j in 1:size) {
      if (is.na(S[j, i - 1])) {
        S[j, i] <- NA
      } else {
        # find the corresponding stage
        stage <- find_stage(object, S[j, 1:(i - 1)])
        if (stage %in% object$name_unobserved |
          NA %in% object$prob[[vars[i]]][[stage]]) {
          S[j, i] <- NA
        } else {
          # sample from the conditional prob of the stage
          S[j, i] <- sample(object$tree[[vars[i]]],
            size = 1,
            prob = object$prob[[vars[i]]][[stage]]
          )
        }
      }
    }
  }
  S <- as.data.frame(S)
  for (i in 1:p) {
    S[, i] <- factor(S[, i], levels = object$tree[[i]])
  }
  attr(S, "seed") <- RNGstate
  return(S)
}
