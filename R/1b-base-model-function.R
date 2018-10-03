#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame or list
#' @param order Vector of variables names, the order to build the event tree
#' @param fit If `TRUE` the conditional probability will be estimated from `data`
#' @return A staged event tree object (see Details)
#' @details A staged event tree object consist
#' @export
staged_ev_tree <- function(x, order = NULL, fit = FALSE ){
  UseMethod("staged_ev_tree", object = x)
}

#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x object that will be coerced to a data.frame
#' @param ... additional parameters that will be passed to the method
#' @return A staged event tree object
#' @export
staged_ev_tree.default <- function(x, ...){
  return(strt_ev_tree.data.frame(as.data.frame(x, ...)))
}


#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame with the observation
#' @param order vector of order, default to the order in `x`
#' @return the staged event tree object
#' @export
staged_ev_tree.data.frame <- function(x, order = colnames(x), fit = FALSE){

}


#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x list named as the variable and containing the vector of the levels
#'          for each variable
#' @return The staged event tree object
#' @details The staged (stratified) event tree returned is the minimal one,
#'          that is the one with just one stage per variable (equivalent to a complete independent model).
#' @export
staged_ev_tree.list <- function(x){
 # to do
}
