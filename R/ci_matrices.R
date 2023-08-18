#' Conditional independences matrices of stages
#'
#' Generate the sequence of all the
#' conditional independences
#' matrices of stages for a given variable in the model.
#' @param object an object of class \code{sevt}.
#' @param var string, the name of one of the variables in \code{object}.
#' @return A list with \code{i-1} matrices, where \code{i} is the depth
#' of variable \code{var} in the tree.
#' @examples
#' mod <- sevt(list(A = c("a", "aa"),
#'                  B = c("b", "bb", "bbb"),
#'                  C = c("c", "cc")), full = TRUE)
#' stages(mod)["C", A = "a", B = c("b", "bb")] <- "stage1"
#' stages(mod)["C", A = "aa"] <- "stage2"
#' stages(mod)["C", A = "a", B = "bbb"] <- "stage2"
#'
#' ci_matrices(mod, "C")
#' @export
ci_matrices <- function(object, var) {
  check_sevt(object)
  check_var_in(var, object)
  vars <- names(object$tree)
  i <- which(vars == var)
  out <- list()
  out[[vars[i - 1]]] <-
    matrix(object$stages[[var]],
           nrow = length(object$tree[[vars[i - 1]]]),
           dimnames = c(object$tree[vars[i - 1]], context = NULL))

  if (i <= 2) {
    return(out)
  } else {
    for (j in (i - 2):1) {
      if (sum(duplicated(out[[vars[j + 1]]])) == (nrow(out[[vars[j + 1]]]) - 1) ) {
        stgs <- out[[vars[j + 1]]][1,]
      } else {
        stgs <- c(t(out[[vars[j + 1]]]))
      }
      out[[vars[j]]] <-
        matrix(c(t(stgs)),
               nrow = length(object$tree[[vars[j]]]),
               dimnames = c(object$tree[vars[j]], context  = NULL))
    }
  }
  return(out)
}
