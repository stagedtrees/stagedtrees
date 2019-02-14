

#' Compute probability of a path from root
#'
#' @param object a staged event tree object
#' @param x the path
#' @param log logical
#' @return The probability of the given path or its logarithm if \code{log=TRUE}
#' @export
path_probability.staged_ev_tree <-
  function(object, x, log = FALSE) {
    if (!is.null(names(x))) {
      #if it's a named vector just order it
      x <- x[names(object$tree)]
    }
    l <- log(object$prob[[1]][[1]][x[1]])
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        s <- find_stage(object, x[1:(i-1)])
        l <- l + log(object$prob[[i]][[s]][x[i]])
      }
    }
    if (log)
      return(l)
    else
      return(exp(l))
  }

#' Compute log lik of a stratified tree
#'
#' @param object the startified event tree object
#' @param ... additional parameters
#'
#' @importFrom stats logLik ftable
#' @export
#'
#' @examples
#' DD <- DD <- as.data.frame(sapply(1:5, function(i){
#'                           return(as.factor(sample(c(1:3), size=100, replace = TRUE)))
#'                      }))
#' sevt <- staged_ev_tree(DD, fit = TRUE)
#' evt <- strt_ev_tree(sevt)
#' logLik(evt)
logLik.strt_ev_tree <- function(object, ...) {
  stopifnot(!is.null(object$ctables)) 
  stopifnot(!is.null(object$prob))
  ll <- sum(vapply(
      1:length(object$tree),
      FUN = function(i) {
        ix <- object$prob[[ i ]] > 0
        sum( (log(object$prob[[ i ]][ix])  ) * 
          object$ctables[[ i ]][ix] )
      },
      FUN.VALUE = 1
    ))
  attr(ll, "df") <-
    prod(vapply(object$tree, FUN = length, FUN.VALUE = 1)) - 1
  attr(ll, "nobs") <- sum(object$ctables[[1]])
  class(ll) <- "logLik"
  return(ll)
}


#' Compute log lik of a staged tree
#'
#' @param object the staged event tree object
#' @param ... additional parameters
#'
#' @importFrom stats logLik
#' @export
#'
#' @examples
#' DD <- DD <- as.data.frame(sapply(1:5, function(i){
#'                           return(as.factor(sample(c(1:3), size=100, replace = TRUE)))
#'                      }))
#' sevt <- staged_ev_tree(DD, fit =TRUE)
#' logLik(sevt)
logLik.staged_ev_tree <- function(object, ...) {
  if (!is.null(object$ll)) {
    return(object$ll)
  }
  stopifnot(!is.null(object$prob))
  stopifnot(!is.null(object$ctables))
  ll <- logLik(strt_ev_tree.staged_ev_tree(object))
  attr(ll, "df") <-
    sum(c(1, vapply(
      object$stages, FUN = function(x) length(unique(x)), FUN.VALUE = 1
    )) *
      (vapply(
        object$tree, FUN = length, FUN.VALUE = 1
      ) - 1))    ## compute the degree of freedom
  return(ll)
}
