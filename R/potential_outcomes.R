#' Potential Outcomes
#'
#' Compute potential outcomes by randomizing
#' the treatment variable on the given model.
#'
#' @param object a fitted object of class \code{sevt}.
#' @param outcome the outcome variable.
#' @param treatment the treatment variable.
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
#' potential_outcomes(model, "Survived", "Class")
#' @export
potential_outcomes <- function(object, outcome, treatment){
  check_sevt_prob(object)
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
#' @param ignore name of stages to be ignored
#' @export
randomize_sevt <- function(object, treatment, p = NULL, ignore = object$name_unobserved){
  check_scope(treatment, object)
  kk <- length(object$tree[[treatment]])
  if (is.null(p)){
    p <- rep.int(1/kk, kk)
  }
  names(p) <- object$tree[[treatment]]
  tmp <- object$stages[[treatment]]
  object$stages[[treatment]][!(tmp %in% ignore)] <- "randomized"
  object$prob[[treatment]] <- c(list(randomized = p), object$prob[[treatment]][ignore])
  object$prob[[treatment]] <- object$prob[[treatment]][!is.na(names(object$prob[[treatment]]))]
  return(object)
}
