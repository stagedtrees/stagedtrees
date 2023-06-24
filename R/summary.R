#' Summarizing staged event trees
#'
#' Summary method for class \code{sevt}.
#'
#' @param object an object of class \code{sevt}.
#' @param ... arguments for compatibility.
#' @details Print model information and summary of stages.
#' @return An object of class \code{summary.sevt}
#'         for which a \code{print}
#'         method exist.
#' @examples
#' model <- stages_fbhc(full(PhDArticles, lambda = 1))
#' summary(model)
#' @export
summary.sevt <- function(object, ...) {
  check_sevt(object)
  vns <- sevt_varnames(object)
  nv <- sevt_nvar(object)
  out <- list()
  out[[vns[1]]] <- data.frame(
    stage = "1",
    npaths = 0, stringsAsFactors = FALSE
  )
  if (has_prob(object)) {
    out[[vns[1]]][["sample.size"]] <- attr(object$prob[[vns[1]]][[1]], "n")
    out[[vns[1]]] <- cbind(
      out[[vns[1]]],
      t(as.data.frame(object$prob[[vns[1]]]))
    )
  }
  for (i in 2:nv) {
    v <- vns[i]
    D <- data.frame(
      stage = unique(object$stages[[v]]),
      stringsAsFactors = FALSE,
      row.names = make.names(unique(object$stages[[v]]))
    )
    D$npaths <- vapply(D$stage, function(s) {
      sum(object$stages[[v]] == s)
    }, FUN.VALUE = 1)
    if (has_prob(object)) {
      D[["sample.size"]] <- vapply(D$stage, function(s) {
        ifelse(is.null(attr(object$prob[[v]][[s]], "n")),
          NA, attr(object$prob[[v]][[s]], "n")
        )
      }, FUN.VALUE = 1)
      if (nrow(D) <= 1) {
        D <- cbind(D, t(as.data.frame(object$prob[[v]])))
      } else {
        D <- cbind(D, t(as.data.frame(object$prob[[v]]))[rownames(D), ])
      }
    }
    out[[vns[i]]] <- D
  }
  out <- list(stages.info = out)
  out$call <- object$call
  out$ll <- object$ll
  out$lambda <- object$lambda
  class(out) <- "summary.sevt"
  return(out)
}

#' @rdname summary.sevt
#' @param x an object of class \code{summary.sevt}.
#' @param max the maximum number of variables for which
#'            information is printed.
#' @export
print.summary.sevt <- function(x, max = 10, ...) {
  if (!is.null(x$call)) {
    cat("Call: \n")
    print(x$call)
  }
  if (!is.null(x$lambda)) cat("lambda: ", x$lambda, "\n")
  cat("Stages: \n")
  for (i in 1:min(length(x$stages.info), max)) {
    cat("  Variable: ", names(x$stages.info)[i], "\n")
    print.data.frame(x$stages.info[[i]], row.names = FALSE)
    cat("  ------------ \n")
  }
  if (max < length(x$stages.info)) {
    cat("  only the first ", max, " variables are shown \n")
  }
}
