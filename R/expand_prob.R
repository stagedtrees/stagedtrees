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
      ft <- array(dim = c(prod(dims[1:(i - 1)]), dims[i]))
      for (j in 1:(dim(ft)[1])) {
        ## fill the ftable
        jstage <- object$stages[[vars[i]]][j]
        ft[j, ] <- object$prob[[vars[i]]][[jstage]]
      }
      attr(ft, "row.vars") <- object$tree[vars[1:(i - 1)]]
      attr(ft, "col.vars") <- object$tree[vars[i]]
      class(ft) <- "ftable"
      prob[[vars[i]]] <- ft
    }
    }
  }
  return(prob)
}