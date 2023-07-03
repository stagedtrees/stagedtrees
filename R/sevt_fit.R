#' Fit a staged event tree
#'
#' Estimate transition probabilities in a staged event tree from data.
#' Probabilities are estimated with the relative frequencies plus,
#' eventually, an additive (Laplace) smoothing.
#' @param object an object of class \code{sevt}.
#' @param data data.frame or contingency table with observations of
#'             the variables in \code{object}.
#' @param lambda smoothing parameter or pseudocount. Default (NULL) to
#'               lambda value stored in `object`. If no lambda value is
#'               stored nor provided, 0 will be used with a warning.
#' @param scope which variable should be fitted. Default (NULL) to
#'                all variables in the model. A partial re-fit is
#'                possible only for model which are already fitted and in
#'                that case the provided \code{lambda} will be ignored if
#'                different from \code{object$lambda}.
#' @param compute_logLik logical value. If \code{TRUE} the log-likelihood
#'                       of the model is computed and stored.
#' @return A fitted staged event tree,
#'         that is an object of class `sevt`
#'         with `ctables` and `prob` components.
#'         Additionally the chosen `lambda` is stored in the returned object
#'         and eventually the log-likelihood of the model is saved in
#'         the `ll` field.
#' @details The data in form of contingency tables and the
#'          log-likelihood of the model is (eventually)
#'          stored in the returned staged event tree.
#'          Partial re-fit of a model can be performed
#'          with the \code{scope} argument.
#'          Partial re-fit can only be done over a
#'          fully fitted model, e.g. when changing
#'          the stages structure of one of the variables.
#'          In case of a partial re-fit, the `data` and `lambda` arguments
#'          will be ignored and the data and lambda value stored in the
#'          sevt object will be used (a warning is issued if such arguments are
#'          supplied).
#' @export
#' @examples
#'
#' #########
#' model <- sevt(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
#' D <- data.frame(
#'   X = c("good", "good", "bad"),
#'   Y = c("high", "low", "low")
#' )
#' model.fit <- sevt_fit(model, data = D, lambda = 1)
sevt_fit <- function(object,
                     data = NULL,
                     lambda = NULL,
                     scope = NULL,
                     compute_logLik = TRUE) {
  ### checking parameters ###
  check_sevt(object)
  # extract order of variables
  order <- sevt_varnames(object)
  if (is.null(scope)){
    scope <- order
    object$prob <- list()
  }else{
    scope <- scope[scope %in% order]
    scope <- unique(scope)
    if (!setequal(scope, order)){
      ## partial fit, check if object is fitted
      if (!is_fitted_sevt(object)){
        cli::cli_abort(c(
          "Partial fitting via {.arg scope} is allowed only
          for completely fitted {.cls sevt} objects.",
          "x" = "You've supplied {.arg object} wich is not fitted.",
          "i" = "Use {.fun stagedtrees::sevt_fit} to associate data and compute
      probabilities for an object of class {.cls sevt}."
        ))
      }
      if (!is.null(data) | !is.null(lambda)){
        cli::cli_warn(c(
          "Partial fitting via {.arg scope} ignores data and/or lambda inputs.",
          "!" = "You've supplied {.arg data} or {.arg lambda} arguments.",
          "i" = "If the data to be fitted or the lambda values need
                to be changed, perform a complete fit of the model
                with {.fun stagedtrees::sevt_fit}."
        ))
      }
      lambda <- object$lambda ## force using same lambda
      data <- NULL ## force using stored data
    }else{
      ## clean prob
      object$prob <- list()
    }
  }
  if (is.null(data)) {
    if (!has_ctables(object)) {
      cli::cli_abort(c(
        "Either {arg. data} must be provided or {.arg object} should have
        associated observations in the {.field ctables} field.",
        "!" = "You've not supplied {.arg data} and {.arg object}
        does not have {.field ctables}."
      ))
    }
  }else{
    object$ctables <- make_ctables(object, data)
  }
  if (is.null(lambda)){
    if (is.null(object$lambda)){
      cli::cli_warn(c(
        "{.arg lambda} should be specified.",
        "x" = "You've not provided {.arg lambda} nor it is
        available in {.arg object}.",
        "i" = "{.code lambda = 0} will be used."
      ))
      lambda <- 0
    }else{
      lambda <- object$lambda
    }
  }
  # store lambda
  object$lambda <- lambda
  ### start fitting ###
  dims <- vapply(object$tree, length, FUN.VALUE = 1)
  # root variable
  if (order[1] %in% scope){
    n <- sum(object$ctables[[order[1]]])
    pp <- object$ctables[[order[1]]] + lambda
    pp <- pp / sum(pp)
    attr(pp, "n") <- n
    object$prob[[order[1]]] <- list("1" = pp)
    scope <- scope[scope != order[1]] ## remove first var from scope if done
  }
  if (length(scope) > 0){
    for (v in scope) {
      stages <- unique(object$stages[[v]])
      object$prob[[v]] <-
        lapply(stages, function(s) {
          ix <- object$stages[[v]] == s
          if (sum(ix) > 1) {
            tt <- apply(object$ctables[[v]][ix, ], MARGIN = 2, sum)
          } else {
            tt <- object$ctables[[v]][ix, ]
          }
          names(tt) <- object$tree[[v]]
          n <- sum(tt) ## compute sample size
          tt <- (tt + lambda) ## smoothing
          tt <- tt / sum(tt) ## normalize
          tt[is.nan(tt)] <- NA  ## replace NaN with NA
          attr(tt, "n") <- n ## save sample size
          return(tt) # return normalized prob
        })
      names(object$prob[[v]]) <- stages
    }
  }
  object$ll <- NULL ## force recompute log-likelihood
  if (compute_logLik){
    object$ll <- logLik(object)
  }
  return(object)
}
