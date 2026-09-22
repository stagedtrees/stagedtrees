#' New label
#'
#' give a safe-to-add label that is not in \code{labels}.
#' @param labels vector of labels.
#' @return a string label that is different from each \code{labels}.
#' @keywords internal
new_label <- function(labels) {
  k <- 1 + length(labels)
  labels <- as.character(labels)
  while (TRUE) {
    if (!(as.character(k) %in% labels)) {
      return(as.character(k))
    }
    k <- k + 1
  }
}

#' Unique id from named list
#'
#' @param x a named list.
#' @return A named list with unique ids.
#' @keywords internal
uni_idx <- function(x, sep = "_") {
  nn <- names(x)
  x <- lapply(seq_along(x), function(i) {
    paste0(nn[i], sep, x[[i]])
  })
  names(x) <- nn
  return(x)
}

#' Default treatment and outcome variables
#'
#' Fill in missing \code{treatment}/\code{outcome} arguments with the
#' second-to-last and last variables in the order of \code{object}.
#' @param treatment the treatment variable, or \code{NULL}.
#' @param outcome the outcome variable, or \code{NULL}.
#' @param object a fitted object of class \code{sevt}.
#' @return a list with components \code{treatment} and \code{outcome}.
#' @keywords internal
default_treatment_outcome <- function(treatment, outcome, object) {
  if (is.null(treatment) || is.null(outcome)) {
    order <- sevt_varnames(object)
    n <- length(order)
    if (n < 2) {
      cli::cli_abort(c(
        "{.arg object} must have at least two variables to infer default
        {.arg treatment} and {.arg outcome}.",
        "x" = "{.arg object} only has variable{?s} {.val {order}}."
      ))
    }
    if (is.null(treatment)) treatment <- order[n - 1]
    if (is.null(outcome)) outcome <- order[n]
  }
  list(treatment = treatment, outcome = outcome)
}

#' Find maximum value
#'
#' @param x numerical, the log-probabilities.
#' @param levels the levels to be returned same length as x.
#'
#' @return factor.
#' @keywords internal
which_class <- function(x, levels) {
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) {
    ix <- sample(y, 1L)
  } else {
    ix <- y
  }
  factor(levels[ix], levels = levels)
}
