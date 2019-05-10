#' Compute probability of a path from root
#'
#' @param object a staged event tree object
#' @param x the path
#' @param log logical
#' @return The probability of the given path or its logarithm if \code{log=TRUE}
#' @examples 
#' DD <- generate_random_dataset(5, 100)
#' model <- staged_ev_tree(DD, fit = TRUE, lambda = 1)
#' path_probability.sevt(model, c("1", "-1"))
#' path_probability.sevt(model, c("1", "-1", "1", "-1", "1"),
#'  log = TRUE)
#' @export
path_probability.sevt <-
  function(object, x, log = FALSE) {
    stopifnot(is(object, "sevt"))
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


#' Compute probabilities for a staged event tree
#' 
#' @param object a (fitted) staged event tree object
#' @param x the vector or data.frame of observations
#' @param log logical, if \code{TRUE} log-probabilities are returned
#' @return the probabilities for each observation
#' @export
prob.sevt <- function(object, x, log = FALSE){
  stopifnot(is(object, "sevt"))
  stopifnot(is_fitted.sevt(object))
  if (is.null(dim(x))){
    x <- as.data.frame(t(x))
  }
  n <- nrow(x)
  i <- ncol(x)
  var <- names(object$tree)
  var1 <- var[var %in% colnames(x)]
  k <- which(var %in% var1[length(var1)])
  res <- vapply(1:n, FUN.VALUE = 1, FUN =  function(i){
    ll <- object$tree[1:k]
    ll[var1] <- vapply(x[i, var1], as.character, FUN.VALUE = "aaa")
    sum(apply(expand.grid(ll), MARGIN = 1, FUN = function(xx){
      path_probability.sevt(object, as.character(xx), log = FALSE)
    }))
  })
  if (log){
    return(log(res))
  }else{
    return(res) 
  }
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
  var <- names(object$tree)
  ll <- sum(vapply(
      1:length(object$tree),
      FUN = function(i) {
        if (any(is.nan(object$prob[[ var[i] ]]) & 
                object$ctables[[ var[i] ]] > 0)){
          return(-Inf)
        }
        ix <- object$prob[[ var[i] ]] > 0 & !is.na(object$prob[[ var[i] ]]) &
              !is.nan(object$prob[[ var[i] ]]) 
        sum( log(object$prob[[ var[i] ]][ix])  * 
          object$ctables[[ var[i] ]][ix] )
      },
      FUN.VALUE = 1
    ))
  attr(ll, "df") <-
    prod(vapply(object$tree, FUN = length, FUN.VALUE = 1)) - 1
  attr(ll, "nobs") <- sum(object$ctables[[var[1]]])
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
logLik.sevt <- function(object, ...) {
  if (!is.null(object$ll)) {
    return(object$ll)
  }
  stopifnot(is(object, "sevt"))
  stopifnot(!is.null(object$prob))
  stopifnot(!is.null(object$ctables))
  ll <- logLik(strt_ev_tree.sevt(object))
  attr(ll, "df") <-
    sum(c(1, vapply(
      object$stages, FUN = function(x) length(unique(x)), FUN.VALUE = 1
    )) *
      (vapply(
        object$tree, FUN = length, FUN.VALUE = 1
      ) - 1))    ## compute the degree of freedom
  return(ll)
}
