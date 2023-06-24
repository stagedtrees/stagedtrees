#' Print a staged event tree
#'
#' @param x an object of class \code{sevt}.
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
print.sevt <- function(x, ...) {
  cat(
    "Staged event tree",
    ifelse(is_fitted_sevt(x), "(fitted) \n", "\n")
  )
  ls <- vapply(x$tree, length, 1)
  cat(paste(paste0(names(x$tree), "[", ls, "] "), collapse = "-> "), "\n")
  if (!is.null(x$ll)) {
    print(x$ll)
  }
  invisible(x)
}