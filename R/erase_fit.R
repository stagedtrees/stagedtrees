#' Erase the sevt fit
#'
#' @param object an object of class \code{sevt}
#' @return an object of class \code{sevt} without
#'         \code{prob} and \code{ll} field.
#' @keywords internal
erase_fit <- function(object) {
  object$ll <- NULL
  object$prob <- NULL
  return(object)
}
