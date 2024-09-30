

#' Fit a staged event tree with the hard Expectation-Maximization (EM) algorithm
#'
#' Estimate transition probabilities in a staged event tree from data containing
#' missing data using the hard (imputed) Expectation-Maximization algorithm.
#' @param object an object of class \code{\link{sevt}}.
#' @param data data.frame with observations of
#'             the variables in \code{object}.
#' @param lambda smoothing parameter. Default (NULL) to
#'               lambda value stored in `object`. If no lambda value is
#'               stored nor provided, 0 will be used with a warning.
#' @param scope which variable should be fitted. The value will be passed to
#'              \code{\link{sevt_fit}}.
#' @param max_iter positive integer, the maximum number of iteration to be used
#'                 in the EM algorithm.
#' @param chain_impute logical. If \code{TRUE} chain predictions
#'              (using the order of the variables in \code{object})
#'              are used to impute
#'              missing values. Otherwise, independent
#'              predictions for each missing values are used.
#' @return A fitted staged event tree,
#'         See the return field of \code{\link{sevt_fit}}.
#' @export
sevt_fit_em <- function(object,  data = object$data_raw,
                        lambda = NULL,
                        scope = NULL,
                        max_iter = 5, chain_impute = FALSE){

  check_sevt_fit(object)
  order <- sevt_varnames(object)
  if (is.null(scope)){
    scope <- order
  }
  if (is.null(data)){
    if (!is.data.frame(object$data_raw)){
      cli::cli_abort(c(
        "{.arg object} must have a data_raw component of class {.cls data.frame}.",
        "x" = "You've supplied {.arg object} where the data_raw component
      is of type {.type {object$data_raw}}."
      ))
    }
    data <- object$data_raw
  } else {
    if (!is.data.frame(data)){
      cli::cli_abort(c(
        "{.arg data} must be of class {.cls data.frame}.",
        "x" = "You've supplied {.arg data} of type {.type {data}}."
      ))
    }
  }
  object$fit_em  <- list()
  for(i in 1:max_iter){
    data_c <- impute(data = data, object = object, chain_impute = chain_impute)
    object <- sevt_fit(object, data = data_c, scope = scope, lambda = lambda, compute_logLik = FALSE)
    object$fit_em$iter <- i
  }
  return(object)
}
