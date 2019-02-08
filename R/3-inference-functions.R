
#' Compute probability of a path from root
#'
#' @param object a staged event tree object
#' @param x the path
#' @param log logical
#' @return The probability of the given path or its logarithm if \code{log=TRUE}
#' @export
path_probability.staged_ev_tree <- function(object, x, log = FALSE) {
  if (!is.null(names(x))) {
    #if it's a named vector just order it
    x <- x[names(object$tree)]
  }
  l <- log(object$prob[[1]][[1]][x[1]])
  for (i in 2:length(x)) {
    s <- find_stage(object$paths[[i - 1]], x)
    l <- l + log(object$prob[[i]][[s]][x[i]])
  }
  if (log)
    return(l)
  else
    return(exp(l))
}

#' Compute log lik of a stratified tree
#'
#' @param object the startified event tree object
#' @param data the dataset (default to `NULL``)
#' @param ... additional parameters
#'
#' @importFrom stats logLik ftable
#' @export
#'
#' @examples
#' DD <- DD <- as.data.frame(sapply(1:5, function(i){
#'                           return(as.factor(sample(c(1:3), size=100, replace = TRUE)))
#'                      }))
#' evt <- strt_ev_tree(DD, fit =TRUE)
#' logLik(evt)
#'
#' logLik(evt, DD[1,])
logLik.strt_ev_tree <- function(object, data = NULL, ...){

 if (is.null(data)){
   data <- object$data ## like this AIC and BIC works automatically
 }
 if (is.null(data)){
   warning("Data should be attached to the object or provided")
   return(NULL)
 }
 if (is.null(object$prob)){
   warning("Object if not fitted, impossible compute logLik")
   return(NULL)
 }
 order <- names(object$tree)
 ll <- sum(log(object$prob[[order[1]]] ) * table(data[order[1]]) ) +
             sum(vapply(2:length(order),FUN = function(i){
   sum(log(object$prob[[order[i]]])*ftable(data,col.vars = order[i],
                                           row.vars = order[1:(i-1)]) )
 }, FUN.VALUE = 1) )
 attr(ll, "df") <- prod(vapply(object$tree, FUN = length, FUN.VALUE = 1)) - 1
 attr(ll, "nobs") <- dim(data)[1]
 class(ll) <- "logLik"
 return(ll)
}


#' Compute log lik of a staged tree
#'
#' @param object the staged event tree object
#' @param data the dataset (default to `NULL``)
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
#'
#' logLik(sevt, DD[1,])
logLik.staged_ev_tree <- function(object, data = NULL, ...){
  if (is.null(data)){
    data <- object$data ## like this AIC and BIC works automatically
  }
  if (is.null(data)){
    warning("Data should be attached to the object or provided")
    return(NULL)
  }
  if (is.null(object$prob)){
    warning("Object if not fitted, impossible compute logLik")
    return(NULL)
  }
  ll <- logLik(strt_ev_tree(object), data=data) ## lazy way we should do better
  attr(ll, "df") <- sum(c(1, vapply(object$stages, FUN = length, FUN.VALUE = 1) ) *
                    (vapply(object$tree, FUN = length, FUN.VALUE = 1) - 1)   )    ## compute the degree of freedom
  return(ll)
}


