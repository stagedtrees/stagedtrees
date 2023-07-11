#' Join situations with no observations
#'
#' @param object an object of class \code{sevt} with associated data.
#' @param fit if TRUE update model's probabilities.
#' @param name character, name for the new stage storing unobserved situations.
#' @param scope character vector, list of variables in \code{object}.
#' @param trace if \code{> 0} print information to console.
#' @param lambda smoothing parameter for the fitting.
#'
#' @return a staged event tree with at most one stage per variable with
#' no observations.
#' If, as default, \code{fit=TRUE} the model will be re-fitted, if
#' \code{fit=FALSE} probabilities in the output model are not estimated.
#'
#' @details It takes as input a (fitted) staged event tree object
#' and  it joins,
#' in the same stage, all the situations with zero
#' recorded observations.
#' Since such joining does not change
#' the log-likelihood of the model, it is a useful (time-wise)
#' pre-processing prior to others model selection algorithms.
#'
#' Unobserved situations can be joined directly in
#' \code{\link{full}} or \code{\link{indep}}, by setting
#' \code{join_unobserved = TRUE}.
#'
#' @export
#'
#' @examples
#' DD <- generate_xor_dataset(p = 5, n = 10)
#' model_full <- full(DD, lambda = 1, join_unobserved = FALSE)
#' model <- join_unobserved(model_full)
#' logLik(model_full)
#' logLik(model)
#' BIC(model_full, model)
join_unobserved <-
  function(object,
           fit = TRUE,
           trace = 0,
           name = "UNOBSERVED",
           scope = sevt_varnames(object)[-1],
           lambda = object$lambda) {
    check_sevt_ctables(object)
    tot <- 0
    ## make scope valid
    scope <- scope[scope %in% sevt_varnames(object)[-1]]
    for (v in scope) {
      if (is.null(name)) {
        new <- new_label(unique(object$stages[[v]]))
      } else {
        new <- paste0(name)
      }
      ix <- rowSums(object$ctables[[v]]) == 0
      object$stages[[v]][ix] <- new
      tot <- tot + sum(ix)
      if (trace > 1) {
        cli::cli_text("{v}: joined {sum(ix)} situations.")
      }
    }
    if (trace > 0) {
      cli::cli_text("joined a total of {tot} situations.")
    }
    object$prob <- NULL
    object$ll <- NULL
    if (fit) {
      object <- sevt_fit(object, lambda = lambda)
      if (trace > 0) {
        cli::cli_text("object fitted using lambda = {lambda}")
      }
    }
    object$name_unobserved <- c(object$name_unobserved, name) # concatenate names
    return(object)
  }
