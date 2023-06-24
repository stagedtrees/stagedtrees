#' return path index
#'
#' @param path a path from root in the tree.
#' @param tree a symmetric tree given as a list of levels.
#' @param complete logical, if \code{TRUE} the complete indexing
#'                 is returned.
#'
#' @details Compute the integer index of the node associated with the
#' given path in a symmetric tree defined by \code{tree}.
#'
#' @return an integer, the index of the node corresponding to \code{path}
#' @keywords internal
tree_idx <- function(path, tree, complete = FALSE) {
  k <- length(path)
  ls <- sapply(tree, length)
  is <- vapply(1:k, FUN = function(i) {
    (1:ls[i])[tree[[i]] %in% path[i]]
  }, FUN.VALUE = 1)
  if (k <= 1) {
    return(is[1])
  }
  if (complete) {
    sum(vapply(1:(k - 1), FUN = function(i) {
      prod(ls[(i + 1):(k)])
    }, FUN.VALUE = 1) * is[1:(k - 1)]) + is[k]
  } else {
    sum(vapply(1:(k - 1), FUN = function(i) {
      prod(ls[(i + 1):(k)])
    }, FUN.VALUE = 1) * (is[1:(k - 1)] - 1)) + is[k]
  }
}



#' Find the stage of the path
#'
#' no checking is done.
#' @param object a staged event tree object.
#' @param path vector of the path.
#' @return the stage name corresponding of the path.
#' @keywords internal
find_stage <- function(object, path) {
  k <- length(path)
  ix <- tree_idx(path = path, tree = object$tree)
  l <- length(object$stages[[sevt_varnames(object)[k + 1]]])
  ### stages can be defined in a reduced vector
  return(object$stages[[sevt_varnames(object)[k + 1]]][(ix - 1) %% l + 1])
}
