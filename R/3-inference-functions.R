

#' Compute log lik of a stratified tree
#'
#' @param object the startified event tree object
#' @param data the dataset (default to `NULL``)
#'
#' @importFrom stats logLik
#' @export
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
             sum(sapply(2:length(order), function(i){
   sum(log(object$prob[[order[i]]])*ftable(data,col.vars = order[i],
                                           row.vars = order[1:(i-1)]) )
 }) )
 attr(ll, "df") <- prod(sapply(object$tree, length)) - 1
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
  attr(ll, "df") <- sum(c(1, sapply(object$stages, length) ) *
                    (sapply(object$tree, length) - 1)   )    ## compute the degree of freedom
  return(ll)
}


