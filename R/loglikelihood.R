#' Log-Likelihood of a staged event tree
#'
#' Compute, or extract the log-likelihood of a staged event tree.
#' @param object an fitted object of class \code{sevt}.
#' @param ... additional parameters (compatibility).
#' @return An object of class \code{\link{logLik}}.
#' @details The \code{df} attribute is computed by \code{\link{sevt_df}}.
#' It counts all distinct stage labels, including those with zero
#' observations; see \code{\link{join_unobserved}} for details.
#' @importFrom stats logLik
#' @export
#' @examples
#' data("PhDArticles")
#' mod <- indep(PhDArticles)
#' logLik(mod)
logLik.sevt <- function(object, ...) {
  if (!is.null(object$ll)) {
    return(object$ll)
  }
  check_sevt_fit(object)
  vars <- sevt_varnames(object)
  prob <- expand_prob(object)
  ll <- sum(vapply(
    seq_along(object$tree),
    FUN = function(i) {
      if (any(is.nan(prob[[vars[i]]]) &
              object$ctables[[vars[i]]] > 0)) {
        return(-Inf)
      }
      ix <-
        prob[[vars[i]]] > 0 & !is.na(prob[[vars[i]]]) &
        !is.nan(prob[[vars[i]]])
      sum(log(prob[[vars[i]]][ix]) *
            object$ctables[[vars[i]]][ix])
    },
    FUN.VALUE = 1
  ))
  attr(ll, "df") <- sevt_df(object)
  attr(ll, "nobs") <- sum(object$ctables[[vars[1]]])
  class(ll) <- "logLik"
  return(ll)
}
