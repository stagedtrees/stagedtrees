

#' Compute log lik of a stratified tree
#'
#' @param object the startified event tree object
#' @param data the dataset (default to `NULL``)
#'
#' @importFrom stats logLik
#' @export
logLik.strt_ev_tree <- function(object, data, ...){
 ### sum(log(evt$prob$C) * ftable(data))

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

}
