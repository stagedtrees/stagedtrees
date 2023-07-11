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
  check_sevt(x)
  cli::cli_text(
    "Staged event tree",
    ifelse(is_fitted_sevt(x), " (fitted, lambda = {.value {x$lambda}})", "")
    )
  ls <- vapply(x$tree, length, 1)
  cli::cat_line(paste(paste0(sevt_varnames(x), "[", ls, "] "), collapse = "-> "))
  if (!is.null(x$ll)) {
    cli::cat_print(x$ll)
  }
  invisible(x)
}
