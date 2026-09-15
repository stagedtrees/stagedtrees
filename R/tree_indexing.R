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
  if (k == 0L) {
    return(NA_real_)
  }
  ls <- lengths(tree)
  ## walk the path from the deepest level up, accumulating the stride
  ## (prod of the level sizes below) instead of recomputing it per position
  idx <- 0
  if (k > 1) {
    stride <- 1
    for (i in (k - 1):1) {
      stride <- stride * ls[[i + 1]]
      m <- match(path[[i]], tree[[i]])
      if (is.na(m)) stop_unknown_level(path[[i]], names(tree)[i])
      idx <- idx + (if (complete) m else m - 1) * stride
    }
  }
  m <- match(path[[k]], tree[[k]])
  if (is.na(m)) stop_unknown_level(path[[k]], names(tree)[k])
  idx + m
}

#' @keywords internal
#' @noRd
stop_unknown_level <- function(value, var) {
  cli::cli_abort(c(
    "{.arg path} contains a value which is not a level of {.val {var}}.",
    "x" = "You've supplied {.val {value}}."
  ), call = NULL)
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
  ### stages can be defined in a reduced vector
  stages <- object$stages[[sevt_varnames(object)[k + 1]]]
  return(stages[(ix - 1) %% length(stages) + 1])
}
