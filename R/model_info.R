#' Variable names
#'
#' Utility returning variable-names in a staged event tree
#' model.
#' @param object an object of class \code{sevt}.
#' @return A character vector.
#' @keywords internal
sevt_varnames <- function(object) {
  names(object$tree)
}


#' Number of variables
#'
#' Utility returning the number of variables
#' in a staged event tree model.
#' @param object An object of class \code{sevt}.
#' @return integer, the number of variables.
#' @keywords internal
sevt_nvar <- function(object) {
  length(names(object$tree))
}


#' Number of parameters of a staged event tree
#'
#' Return the number of parameters of the model.
#' @param x An object of class \code{sevt}.
#' @return integer, degrees of freedom of the staged event tree.
#' @keywords internal
sevt_df <- function(x) {
  sum(c(1, vapply(
    x$stages,
    FUN = function(x) {
      length(unique(x))
    },
    FUN.VALUE = 1
  )) *
    (vapply(
      x$tree,
      FUN = length, FUN.VALUE = 1
    ) - 1))
}
