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
    ## each one is filled in next to the other, so that supplying one of
    ## them moves the other rather than leaving it at the end of the order
    if (is.null(treatment) && is.null(outcome)) {
      treatment <- order[n - 1]
      outcome <- order[n]
    } else if (is.null(outcome)) {
      it <- which(order == treatment)
      if (length(it) == 1 && it == n) {
        cli::cli_abort(c(
          "{.arg outcome} must be given when {.arg treatment} is the last
          variable of {.arg object}.",
          "x" = "{.val {treatment}} is last in {.val {order}}, so no
                 variable follows it."
        ))
      }
      outcome <- order[it + 1]
    } else {
      io <- which(order == outcome)
      if (length(io) == 1 && io == 1) {
        cli::cli_abort(c(
          "{.arg treatment} must be given when {.arg outcome} is the first
          variable of {.arg object}.",
          "x" = "{.val {outcome}} is first in {.val {order}}, so no
                 variable precedes it."
        ))
      }
      treatment <- order[io - 1]
    }
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
