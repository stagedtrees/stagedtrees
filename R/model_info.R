#' Variable names
#'
#' Utility returning variable-names in a staged event tree
#' model.
#' @param object an object of class \code{sevt}.
#' @return A character vector.
#' @export
sevt_varnames <- function(object) {
  names(object$tree)
}


#' Number of variables
#'
#' Utility returning the number of variables
#' in a staged event tree model.
#' @param object An object of class \code{sevt}.
#' @return integer, the number of variables.
#' @export
sevt_nvar <- function(object) {
  length(names(object$tree))
}


#' Number of parameters of a staged event tree
#'
#' Return the number of parameters of the model.
#' @param x An object of class \code{sevt}.
#' @return integer, degrees of freedom of the staged event tree.
#' @details The degrees of freedom equal
#' \eqn{\sum_v |\text{stages}_v| \times (k_v - 1)}, where the sum runs over
#' all variables, \eqn{|\text{stages}_v|} is the number of distinct stage
#' labels for variable \eqn{v} (1 for the root), and \eqn{k_v} is the number
#' of levels of \eqn{v}. Stages with zero observations are still counted
#' (the df is a property of the model, not of the sample); see
#' \code{\link{join_unobserved}} to merge them before fitting.
#' @seealso \code{\link{logLik.sevt}}
#' @export
sevt_df <- function(x) {
  vars <- sevt_varnames(x)
  sum(c(1L, vapply(
    x$stages[vars[-1]],   # index by name: robust to root entry and reordering
    FUN = function(s) length(unique(s)),
    FUN.VALUE = 1L
  )) * (vapply(x$tree, length, FUN.VALUE = 1L) - 1L))
}
