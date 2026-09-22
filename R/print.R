#' Print a staged event tree
#'
#' @param x an object of class \code{sevt}.
#' @param max integer, limit on the numebr of variables to print.
#' @param ... additional parameters (compatibility).
#'
#' @return An invisible copy of \code{x}.
#' @details The order of the variables in the staged tree
#'  is printed (from root). In addition the number of levels of each
#'  variable is shown in square brackets.
#'  If available the log-likelihood of the model is printed.
#' @export
#' @examples
#' DD <- generate_xor_dataset(5, 100)
#' model <- full(DD, lambda = 1)
#' print(model)
print.sevt <- function(x, ..., max = 5) {
  check_sevt(x)
  cat(
    "Staged event tree",
    ifelse(is_fitted_sevt(x), "(fitted)", ""), "\n"
    )
  cat(tree_string(x$tree, max = max), "\n")
  if (!is.null(x$call)) {
    ## only the last one, the earlier calls are listed by summary()
    n_before <- length(x$calls) - 1L
    cat("Call: ", deparse(x$call),
        if (n_before > 0L) paste0("(after ", n_before, " more)"), "\n")
  }
  if (!is.null(x$ll)) {
    print(x$ll)
  }
  invisible(x)
}
