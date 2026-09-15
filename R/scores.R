#' Predefined scores for stages structure search
#'
#' Registry of the scores that the stages search algorithms can evaluate
#' incrementally.
#'
#' @details Each entry provides two equivalent views of the same score:
#'
#' * \code{full}: the score of a complete \code{sevt} object, maximized by
#'   the search algorithms.
#' * \code{delta}: the change in \code{full} induced by a change of
#'   \code{dll} in the log-likelihood and \code{ddf} in the degrees of
#'   freedom, for a model fitted on \code{nobs} observations.
#'
#' Supplying a score by name lets the search algorithms use \code{delta} and
#' avoid building a candidate model for every move, which is substantially
#' faster. A score given as a function can only be evaluated on a complete
#' object, so the slower path is used.
#'
#' To add a score, add an entry here: both views must agree, which is checked
#' in the package tests.
#' @importFrom stats AIC BIC
#' @keywords internal
.stages_scores <- list(
  BIC = list(
    full = function(x) -BIC(x),
    delta = function(dll, ddf, nobs) 2 * dll - ddf * log(nobs)
  ),
  AIC = list(
    full = function(x) -AIC(x),
    delta = function(dll, ddf, nobs) 2 * dll - 2 * ddf
  )
)

#' Resolve a score argument
#'
#' @param score a string naming a predefined score, or a function.
#' @param arg the argument name, used in error messages.
#' @param call the calling environment, used in error messages.
#' @return \code{NULL} if \code{score} is a function, otherwise the matching
#' entry of \code{\link{.stages_scores}}.
#' @keywords internal
resolve_score <- function(score, arg = "score", call = rlang::caller_env()) {
  if (is.function(score)) {
    return(NULL)
  }
  if (is.character(score) && length(score) == 1L) {
    if (!(score %in% names(.stages_scores))) {
      cli::cli_abort(c(
        "{.arg {arg}} must be one of the predefined scores or a function.",
        "x" = "You've supplied {.val {score}}.",
        "i" = "Predefined scores are: {.val {names(.stages_scores)}}."
      ), call = call)
    }
    return(.stages_scores[[score]])
  }
  cli::cli_abort(c(
    "{.arg {arg}} must be a function or a string naming a predefined score.",
    "x" = "You've supplied {.type {score}}.",
    "i" = "Predefined scores are: {.val {names(.stages_scores)}}."
  ), call = call)
}
