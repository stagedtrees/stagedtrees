#' Potential Outcomes
#'
#' Compute potential outcomes by either randomizing the treatment variable
#' or via propensity score stratification.
#' @param object a fitted object of class \code{sevt}.
#' @param outcome the outcome variable.
#' @param treatment the treatment variable.
#' @param method string, either \code{"randomization"} or \code{"pstree"}.
#' @return a matrix with potential outcomes.
#' @examples
#' model <- full(Titanic) |> stages_bhc()
#' potential_outcomes(model, "Survived", "Class", "pstree")
#' @export
potential_outcomes <- function(object, outcome, treatment,
                               method = c("randomization", "pstree")){
  check_sevt_fit(object)
  check_scope(c(outcome, treatment), object)
  method <- match.arg(method)
  object0 <- switch(method,
    randomization = randomize(object, treatment),
    pstree = ps_tree(object, outcome, treatment)
  )
  xx <- c(NA)
  names(xx) <- outcome
  res <- sapply(object$tree[[outcome]], function(vo){
    xx[1] <- vo
    prob(object0, xx, conditional_on = as.data.frame(object$tree[treatment]),
         na0 = TRUE)
  })
  dimnames(res) <- object$tree[c(treatment, outcome)]
  return(res)
}
