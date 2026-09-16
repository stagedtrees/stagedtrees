#' Expand probabilities of a staged event tree
#'
#' Return the list of complete probability tables.
#' @param object a fitted staged event tree object.
#' @return probability tables.
#' @keywords internal
expand_prob <- function(object) {
  check_sevt_prob(object)
  prob <- list()
  vars <- names(object$tree)
  dims <- vapply(object$tree, length, FUN.VALUE = 1)
  if (!is.null(object$prob)) {
    # the first one is easy we just have to forget the (only) stage 
    # (and we check validity)
    if (length(object$prob[[vars[1]]]) > 1) {
      warning("Incorrect number of stages in first variable (should be one)")
    }
    prob[[vars[1]]] <- object$prob[[vars[1]]][[1]]
    if (length(object$tree)>1){
    for (i in 2:length(object$tree)) {
      # let's take care of the other variables
      ## we will create manually the ftable
      ## the dimension are the same as path (-1 for the column)
      ## stack the per-stage probabilities once and select a row per
      ## situation, rather than filling the table one row at a time
      pp <- do.call(rbind, object$prob[[vars[i]]])
      ft <- pp[object$stages[[vars[i]]], , drop = FALSE]
      dimnames(ft) <- NULL
      ## keep the dim exactly as array() produced it, names included
      dim(ft) <- c(prod(dims[1:(i - 1)]), dims[i])
      attr(ft, "row.vars") <- object$tree[vars[1:(i - 1)]]
      attr(ft, "col.vars") <- object$tree[vars[i]]
      class(ft) <- "ftable"
      prob[[vars[i]]] <- ft
    }
    }
  }
  return(prob)
}