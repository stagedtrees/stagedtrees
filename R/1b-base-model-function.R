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
staged_ev_tree <- function(x, order = NULL, fit = FALSE , ... ){
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
staged_ev_tree.data.frame <- function(x, order = colnames(x), fit = FALSE, lambda = 0){
  evt <- staged_ev_tree.list(lapply(x, function(v)
    return(levels(as.factor(v))) )[order])
  if (fit) {
    evt <- staged_ev_tree_fit(evt,data = x, lambda = lambda) }
  return(evt)
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
  if (is.null(names(x))){ #if there are no names of variables
    #we assign variables names V1,V2,...
    names(x) <- paste0("V", 1 : length(x))
  }

  if (any(is.null(sapply(x, length)))){ #naive check if levels are vector with lenght
    warning("Levels should be well defined")
    return(NULL) #exit without nothing
  }
  evt <- list()

  evt$tree <- x
  evt$paths <- lapply(1:(length(x)-1), function(i){
     tt <- expand.grid(evt$tree[i:1])[i:1] ## create all the possible paths
     tt[, dim(tt)[2] + 1] <- 1 #put the same color in all the paths
     return(tt)
  })
  evt$stages <- lapply(x[-1], function(xx) return(1))
  names(evt$paths) <- names(x)[-1]
  class(evt) <- "staged_ev_tree"
  return(evt)
}

#' Fit a staged event tree
#'
#' @param sevt The staged event tree object to be fitted
#' @param data the data.frame used to fit the staged event tree
#' @param lambda the laplace smoothing
#' @return A staged event tree object with the conditional probabilities fitted
#' @export
staged_ev_tree_fit <- function(sevt, data, lambda = 0, ... ){
  order <- names(sevt$tree)
  data <- data[order] #order the data
  dims <- lapply(sevt$tree, length)
  sevt$prob <- list()
  tt <- table(data[order[1]]) + lambda
  tt <- tt / sum(tt)
  sevt$prob[[order[1]]] <- list(tt)
  for (i in 2:length(order)){
    sevt$prob[[order[i]]] <-  lapply(sevt$stages[[order[i]]], function(s){
      dt <- data[,1:i] #copy relevant data in dt
      pths <- find_paths(paths = sevt$paths[[order[i]]], s) #find all the paths in that stage
      for (j in 1:(length(pths) - 1) ){  #for every step in the path ( -1 because last var is stage name)
        dt <- dt[ dt[ , j ] %in% pths[,j] ,  ] #reduce dt to the observation that we need
        }
      tt <- table(dt[order[i]]) + lambda #table count plus lambda
      return(tt / sum(tt) ) #return normalized prob
    })
  }
  return(sevt)
}

#' Staged event tree
#'
#' @param x A stratified event tree object
#' @param ... additional parameters
#' @return the equivalent staged event tree object
#' @details The function creates a staged event tree equivalent to the stratified event tree
#' @export
staged_ev_tree.strt_ev_tree <-function(x, ...){
  x$stages <- list()
  x$paths <- list()
   for (i in 1:(length(x$tree)-1) ){
     tt <- expand.grid(x$tree[i:1])[i:1] ## create all the possible paths
     tt[, dim(tt)[2] + 1] <- 1:(dim(tt)[1]) #put different colors/stages
     x$stages[[i]] <- 1:(dim(tt)[1])
     x$paths[[i]] <- tt
   }
   names(x$paths) <- names(x$tree)[-1]
   names(x$stages) <- names(x$tree)[-1]
   ## to do trnasfer probabilities
   return(x)
}


#' Set stage to path
#'
#' Set the given stage to the path for the stage event tree
#' @param sevt Staged event tree
#' @param path Vector of the path
#' @param stage stage to be assigned
set_stage <- function(sevt, path, stage){
 k <- length(path)
 d <- dim(sevt$paths[[order[k]]])[1]
 for (a in 1:d){

 }

}


join_stages <- function(sevt, level,  s1, s2){

}


split_stage <- function(sevt, level,  stage, method = "rand"){

}
