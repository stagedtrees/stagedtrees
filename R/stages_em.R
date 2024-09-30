#' Structural EM for stages structure
#'
#' @param object an object of class sevt with fitted probabilities
#'               and data, as returned by \code{\link{full}}
#'               or \code{\link{sevt_fit}}.
#' @param data the data with missing values to be used in the EM algorithm.
#' @param stages_alg a function that performs stages structure estimation. Similar to
#'            \code{\link{stages_bhc}} or \code{\link{stages_hclust}}.
#'            The function \code{alg} must accept the argument
#'            \code{scope}.
#' @param max_iter_em the maximum number of iterations of the EM algorithm.
#' @param incremental logical. If \code{TRUE}, at each iteration of the EM algorithm
#'                    \code{alg_em} is initialized on the results of the
#'                    previous step. If \code{FALSE}, at each iteration
#'                    \code{alg_em} is initialized on the
#'                    initial argument \code{object}.
#' @param chain_impute logical. If \code{TRUE} chain predictions
#'              (using the order of the variables in \code{object})
#'              are used to impute
#'              missing values. Otherwise, independent
#'              predictions for each missing values are used.
#' @param ... additional arguments passed to \code{alg}.
#' @returns The final staged event tree obtained.
#' @export
stages_em <- function(object, data = object$data_raw, stages_alg = stages_bhc,
                      max_iter_em = 5, incremental = FALSE,
                      chain_impute = FALSE, ...){
  check_sevt_fit(object)
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
  object$em <- list()
  start <- copy_sevt(object, c("lambda", "name_unobserved", "em"))
  for(i in 1:max_iter_em){
    data_c <- impute(data = data, object = object, chain_impute = chain_impute)
    if (incremental){
      start <- sevt_fit(object, data = data_c)
    } else {
      start <- sevt_fit(start, data = data_c)
    }
    new <- stages_alg(start, ...)
    new$em$iter <- i
    if(compare_stages(object, new)){
      object$em$iter <- i
      break
    }
    object <- new
  }
  object <- sevt_fit(object, data_c) ## fit with last imputed data
  object$data_raw <- data
  return(object)
}

#' impute data
#'
#' Internal, No checks are performed
#' @param data a data.frame
#' @param object a sevt object
#' @keywords internal
impute <- function(data, object, chain_impute = FALSE){
  object$skip_checks <- TRUE
  new_data <- data
  for (vv in sevt_varnames(object)){
    wh <- is.na(new_data[[vv]])
    if (sum(wh) > 0){
      if (isFALSE(chain_impute)){
        new_data[wh , vv] <- as.character(predict(object, class = vv, newdata =  data[wh,]))
      } else {
        new_data[wh , vv] <- as.character(predict(object, class = vv, newdata =  new_data[wh,]))
      }
    }
  }
  return(new_data)
}
