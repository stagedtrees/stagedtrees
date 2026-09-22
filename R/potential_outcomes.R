#' Potential Outcomes
#'
#' Compute potential outcomes by randomizing
#' the treatment variable on the given model.
#'
#' @param object a fitted object of class \code{sevt}.
#' @param treatment the treatment variable. Defaults to the variable
#'                   preceding \code{outcome}, or to the second-to-last
#'                   variable of \code{object} if \code{outcome} is not
#'                   given either.
#' @param outcome the outcome variable. Defaults to the variable following
#'                 \code{treatment}, or to the last variable of
#'                 \code{object} if \code{treatment} is not given either.
#' @return a matrix with potential outcomes.
#' @details
#' The \code{potential_outcome} function _randomize_
#' the treatment variable in
#' the given staged event tree with the \code{randomize_sevt}
#' function and then compute the conditional probabilities
#' of the outcome variable given the values of the
#' treatment variable.
#'
#' The \code{randomize_sevt} function builds the
#' staged event tree associated with a randomized experiment over
#'
#' @examples
#' model <- stages_bhc(full(Titanic))
#' potential_outcomes(model, "Class", "Survived")
#'
#' # using the default treatment/outcome, the last two variables in the order
#' # of `model` (here, "Age" and "Survived")
#' potential_outcomes(model)
#' @export
potential_outcomes <- function(object, treatment = NULL, outcome = NULL){
  check_sevt_prob(object)
  defaults <- default_treatment_outcome(treatment, outcome, object)
  treatment <- defaults$treatment
  outcome <- defaults$outcome
  check_scope(c(outcome, treatment), object)
  object0 <- randomize_sevt(object, treatment)
  xx <- c(NA)
  names(xx) <- outcome
  res <- sapply(object$tree[[outcome]], function(vo){
    xx[1] <- vo
    prob(object0, xx, conditional_on = as.data.frame(object$tree[treatment]),
         na0 = FALSE)
  })
  dimnames(res) <- object$tree[c(treatment, outcome)]
  return(res)
}


#' @rdname potential_outcomes
#' @param p the probabilities of treatment
#' @param ignore name of the stages of \code{treatment} which are left as
#'                they are, by default the stage of the situations with no
#'                observations. Every other situation of \code{treatment}
#'                is moved to a single \code{"randomized"} stage carrying
#'                \code{p}, while these keep their own stage and
#'                probabilities. Use \code{ignore = NULL} to randomize the
#'                treatment in every situation.
#' @export
randomize_sevt <- function(object, treatment, p = NULL, ignore = object$name_unobserved){
  check_scope(treatment, object)
  kk <- length(object$tree[[treatment]])
  if (is.null(p)){
    p <- rep.int(1/kk, kk)
  }
  names(p) <- object$tree[[treatment]]
  ## the randomized probabilities are set by design and estimated from
  ## nothing, which is what the missing sample size records
  attr(p, "n") <- NA
  tmp <- object$stages[[treatment]]
  object$stages[[treatment]][!(tmp %in% ignore)] <- "randomized"
  object$prob[[treatment]] <- c(list(randomized = p), object$prob[[treatment]][ignore])
  object$prob[[treatment]] <- object$prob[[treatment]][!is.na(names(object$prob[[treatment]]))]
  ## the returned tree is the one of a randomized experiment, not a model
  ## fitted to the data, and the call is what says so
  object <- record_call(object, match.call())
  return(object)
}
