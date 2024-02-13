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
