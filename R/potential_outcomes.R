#' Potential Outcomes
#'
#' Compute potential outcomes by randomizing
#' the treatment variable.
#' @param object a fitted object of class \code{sevt}.
#' @param outcome the outcome variable.
#' @param treatment the treatment variable.
#' @return a matrix with potential outcomes.
#' @examples
#' model <- full(Titanic) |> stages_bhc()
#' potential_outcomes(model, "Survived", "Class", "pstree")
#' @export
potential_outcomes <- function(object, outcome, treatment){
  check_sevt_prob(object)
  check_scope(c(outcome, treatment), object)
  object0 <- randomize(object, treatment)
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
