#' Convert to a \pkg{bnlearn} object
#'
#' Convert a staged tree object into an object of class \code{bn}
#' from the \pkg{bnlearn} package.
#' @param x an R object of class \code{sevt} or \code{parentslist}.
#' @return an object of class \code{bn} from package \pkg{bnlearn}.
#' @export
as_bn <- function(x) {
  UseMethod("as_bn", x)
}

#' @rdname as_bn
#' @export
as_bn.parentslist <- function(x) {
  bnlearn::model2network(as.character(x, only_parents = TRUE))
}

#' @rdname as_bn
#' @export
as_bn.sevt <- function(x) {
  check_sevt(x)
  as_bn.parentslist(as_parentslist.sevt(x))
}
