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
  ## The situation each sample has reached is carried down the tree rather
  ## than recomputed from its path at every variable, exactly as
  ## path_probability does: idx_j = (idx_{j-1} - 1) * ls_j + m_j. A sample
  ## whose path has already failed carries NA, which propagates.
  idx <- match(S[, vars[1]], object$tree[[vars[1]]])
  # sequentially sample the other variables
  for (i in seq_len(p)[-1]) {
    ## everything here is fixed for the whole sweep over samples, so look it
    ## up once rather than once per sample
    vi <- vars[i]
    probs_i <- object$prob[[vi]]
    lvls_i <- object$tree[[vi]]
    unobserved <- object$name_unobserved
    st <- as.character(object$stages[[vi]])
    stage <- st[(idx - 1) %% length(st) + 1]
    ## a stage that was never observed, or whose probabilities are missing,
    ## cannot be sampled from and yields NA -- as does a path already failed
    na_stage <- names(probs_i)[vapply(probs_i, anyNA, logical(1))]
    bad <- is.na(stage) | stage %in% unobserved | stage %in% na_stage
    out <- rep(NA_character_, size)
    ok <- which(!bad)
    if (length(ok) > 0) {
      ## Draw every sample sitting in a stage with one call, instead of one
      ## call per sample. sample() rebuilds its lookup tables on each call, so
      ## the per-sample version paid that `size` times per variable. This
      ## changes which values a given seed produces; the distribution is the
      ## same, since repeating sample(x, 1, prob = p) is the same as drawing
      ## with replacement in one go.
      for (g in split(ok, stage[ok])) {
        out[g] <- sample(lvls_i,
          size = length(g),
          replace = TRUE,
          prob = probs_i[[stage[g[1]]]]
        )
      }
    }
    S[, i] <- out
    if (i < p) idx <- (idx - 1) * length(lvls_i) + match(out, lvls_i)
  }
  S <- as.data.frame(S)
  for (i in 1:p) {
    S[, i] <- factor(S[, i], levels = object$tree[[i]])
  }
  attr(S, "seed") <- RNGstate
  return(S)
}
