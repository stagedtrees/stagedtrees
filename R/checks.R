#' Check if the stages event tree has ctables field
#'
#' @param object a staged event tree object.
#' @return logical.
#' @keywords internal
has_ctables <- function(object) {
  isFALSE(is.null(object$ctables))
}

#' Check if the stages event tree has probabilities
#'
#' @param object a staged event tree object.
#' @return logical.
#' @keywords internal
has_prob <- function(object) {
  if (isTRUE(is.null(object$prob))) {
    return(FALSE)
  } else {
    ## check that we have all probabilities
    vars <- sevt_varnames(object)
    if (isTRUE(any(sapply(vars, function(v) is.null(object$prob[[v]]))))) {
      return(FALSE)
    } else {
      ## check probabilities are ok
      isFALSE(any(sapply(vars, function(v) {
        any(sapply(object$prob[[v]], function(pp) {
          isFALSE(length(pp) == length(object$tree[[v]]))
        }))
      })))
    }
  }
}

#' Check if the stages event tree is fitted
#'
#' @param object a staged event tree object.
#' @return logical.
#' @keywords internal
is_fitted_sevt <- function(object) {
  check_sevt(object)
  has_prob(object) && has_ctables(object)
}


#' check sevt object
#' @param object an object of class sevt
#' @keywords internal
check_sevt <- function(object) {
  if (!is.object(object)) {
    stop('object is not of class sevt, check ?"sevt"')
  }
  if (!inherits(object, "sevt")) {
    stop('object is not of class sevt, check ?"sevt"')
  }
  if (is.null(object$tree)) {
    stop('object is missing the required tree component, check ?"sevt"')
  }
  if (is.null(object$stages)) {
    stop('object is missing the required stages component, check ?"sevt"')
  }
}

#' check
#' @param object an object of class sevt
#' @keywords internal
check_sevt_prob <- function(object) {
  check_sevt(object)
  if (!has_prob(object)) {
    stop("The provided sevt object has no probabilitites (prob), \n",
      "use sevt_fit to associate data and compute probabilities for an object of class sevt \n",
      "or check ?full or ?indep for utilities to build fitted staged event trees.",
      call. = FALSE
    )
  }
}


#' @rdname check_sevt
#' @keywords internal
check_sevt_ctables <- function(object) {
  check_sevt(object)
  if (!has_ctables(object)) {
    stop("The provided sevt object has no data (ctables), \n",
      "use sevt_fit to associate data and compute probabilities for an object of class sevt \n",
      "or check ?full or ?indep for utilities to build fitted staged event trees.",
      call. = FALSE
    )
  }
}

#' @rdname check_sevt
#' @keywords internal
check_sevt_fit <- function(object) {
  check_sevt_ctables(object)
  check_sevt_prob(object)
}
