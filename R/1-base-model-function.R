#' Stratified event tree
#'
#' Builds the complete stratified event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame or list
#' @param order Vector of variables names, the order to build the event tree
#' @param fit If `TRUE` the conditional probability will be estimated from `data`
#' @export
strt_ev_tree <- function(x, order = NULL, fit = FALSE ){
  UseMethod("strt_ev_tree", object = x)
}

strt_ev_tree.default <- function(x,...){
  return(strt_ev_tree.data.frame(as.data.frame(x)))
}

#' Stratified event tree from data.frame
#'
#' @param x data.frame
#' @param order vector of order to build the tree
#' @param fit
#' @export
strt_ev_tree.data.frame <- function(x, order = colnames(x), fit = FALSE){
  evt <- strt_ev_tree.list(lapply(x, function(v)
    return(levels(as.factor(v))) )[order])
  if (fit) {
    evt <- strt_ev_tree.fit(evt,data = D) }
  return(evt)
}


strt_ev_tree.list <- function(x){
 if (is.null(names(x))){ #if there are no names of variables
   #we assign variables names V1,V2,...
   names(x) <- paste0("V", 1 : length(x))
 }

 if (any(is.null(sapply(x, length)))){ #naive check if levels are vector with lenght
   warning("Levels should be well defined")
   return(NULL) #exit without nothing
 }

 evt <- list( tree = x )
 class(evt) <- "strt_ev_tree"
 return(evt)
}

#' Fit a stratified event tree
#'
#' @param  evt The stratified event tree object to be fitted
#' @param data the data.frame used to fit the event tree
#' @param lambda the laplace smoothing
strt_ev_tree.fit <- function(evt, data, lambda = 0){
   order <- names(evt$tree)
   dims <- lapply(evt$tree, length)
   evt$prob <- lapply(1:length(order), function(i){
     path <- order[i:1]
     tt <- table(data[path],dnn = path ) + lambda
     if (i == 1){
       return(tt/sum(tt))
     }
     return(apply(tt, MARGIN=c(2:i),function(a) return(a/sum(a))))
   } )
   names(evt$prob) <- order
   return(evt)
}

