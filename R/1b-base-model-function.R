#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame, list or stratified event tree object
#' @param ... additional parameters to be passed
#'            to the appropiate method, see \link{staged_ev_tree.data.frame}
#' @return A staged event tree object (see Details)
#' @details A staged event tree object is a list with components:
#'\itemize{
#'          \item tree: A named list where for each variable, the levels of
#'                  such variable are listed. The order of the variable is the
#'                  order of the event tree.
#'          \item stages: A named list where each component stores the stages
#'                    for the given variable, stages are represented by numbers
#'                    that do not need to be consecutive.
#'          \item paths: A named list where for each variable we list in a
#'          data.frame object all the possible paths up to that point and the
#'          assigned stages.
#'          \item prob: The conditional probability tables for every variable
#'          and every stage (present only if the staged event tree has been
#'          fitted)
#'          }
#' @export
#' @examples 
#' DD <- generate_random_dataset(n = 5, 1000)
#' sevt <- staged_ev_tree(DD, fit = TRUE, method = "indep")
#' sevt_full <- staged_ev_tree(DD, method = "full", fit = TRUE, lambda = 1)
staged_ev_tree <- function(x, ...) {
  UseMethod("staged_ev_tree", object = x)
}

#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x object that will be coerced to a data.frame
#' @param ... additional parameters that will be passed
#'            to the appropiate method, see \link{staged_ev_tree.data.frame}
#' @return A staged event tree object
#' @export
staged_ev_tree.default <- function(x, ...) {
  return(staged_ev_tree.data.frame(as.data.frame(x, ...)))
}


#' Staged (stratified) event tree
#'
#' Builds the staged event tree from a data set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame with the observation
#' @param order vector of order, default to the order of the columns of \code{x}
#' @param full logical, if the full model should be built instead
#' @param fit logical
#' @param ... additional parameters to be passed to other methods (mainly 
#' \link{fit.staged_ev_tree})
#' @return A staged event tree object
#' @details The staged event trees
#' @examples
#' DD <- generate_random_dataset(n = 5, 1000)
#' sevt <- staged_ev_tree(DD, fit = TRUE, full = FALSE)
#' @export
staged_ev_tree.data.frame <- function(x,
                                      order = colnames(x),
                                      full = FALSE,
                                      fit = TRUE,
                                      ...) {
     tree <- lapply(x, function(v)
        return(levels(as.factor(
        v
       ))))[order]
      sevt <- staged_ev_tree.list(tree, full = full, ...) 
      if (fit) {
        return(fit.staged_ev_tree(sevt, data = x, ...))
      } else {
        return(sevt)
      }
}


#' Staged (stratified) event tree
#'
#' Build the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x list named as the variable and containing the vector of the levels
#'          for each variable
#' @param full if the full models should be created instead
#' @param ... additional parameters
#' @return The staged event tree object
#' @details The staged (stratified) event tree returned is the minimal one,
#'          that is the one with just one stage per variable (equivalent to a complete independent model).
#'
#' @examples #' model <- staged_ev_tree(list(X = c("good", "bad"), Y = c("high", "low")))
#' @export
staged_ev_tree.list <- function(x, full = FALSE, ...) {
  if (is.null(names(x))) {
    #if there are no names of variables
    #we assign variables names V1,V2,...
    names(x) <- paste0("V", 1:length(x))
  }
  
  dims <- vapply(
    x, FUN = length, FUN.VALUE = 1
  )
  if (any(is.null(dims))) {
    #naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }
  
  evt <- list()
  evt$tree <- x
  if (full){
    evt$stages <- lapply(2 : length(x), function(i){
      as.character(1 : prod(dims[1 : (i - 1)]))   
    })
  }else{
    evt$stages <- lapply(2 : length(x), function(i){
      rep( "1", prod(dims[1 : (i - 1)]))   
    })  
  }
  names(evt$stages) <- names(x)[-1]
  class(evt) <- "staged_ev_tree"
  return(evt)
}


#' Fit a staged event tree
#'
#' @param sevt The staged event tree object to be fitted
#' @param data the data.frame used to fit the staged event tree
#' @param lambda the laplace smoothing
#' @param ... additional parameters
#' @return A staged event tree object with the conditional probabilities fitted
#' @export
#' @examples 
#' model <- staged_ev_tree(list(X = c("good", "bad"), Y = c("high", "low")))
#' D <- data.frame(X = c("good", "good", "bad"), 
#'                 Y = c("high", "low", "low"))
#' model.fit <- fit.staged_ev_tree(model, data = D, lambda = 1)
fit.staged_ev_tree <- function(sevt,
                               data = NULL,
                               lambda = 0,
                               ...) {
  if (is.null(data)) {
    if (is.null(sevt$ctables)){
      warning("Data must be provided or included in the model object")
      return(sevt)
    }
  }
  if (is(data, "srt_ev_tree")){
    sevt$ctables <- data$ctables  
  }else if(!is.null(data)){
    sevt$ctables <- strt_ev_tree(data, order = names(sevt$tree), fit = TRUE)$ctables
  }
  sevt$lambda <- lambda
  order <- names(sevt$tree)
  dims <- sapply(sevt$tree, length)
  sevt$prob <- list()
  n <- sum(sevt$ctables[[order[1]]])
  pp <- sevt$ctables[[order[1]]] + lambda
  pp <- pp / sum(pp)
  attr(pp, "n") <- n
  sevt$prob[[order[1]]] <- list("1" = pp)
  lambda <- lambda / dims[1]
  for (i in 2:length(order)) {
    stages <- unique(sevt$stages[[order[i]]])
    sevt$prob[[order[i]]] <-
      lapply(stages, function(s) {
        ix <- sevt$stages[[order[i]]] == s
        if (sum(ix) > 1){
          tt <- apply(sevt$ctables[[order[i]]][ix,], MARGIN = 2, sum) 
        }else{
          tt <- sevt$ctables[[order[i]]][ix,]
        }
        names(tt) <- sevt$tree[[order[i]]]
        n <- sum(tt) ##compute sample size
        tt <- (tt + lambda) ##smoothing 
        tt <- tt / sum(tt)  ##normalize
        attr(tt, "n") <- n ##save sample size
        return(tt) #return normalized prob
      })
    names(sevt$prob[[order[i]]]) <- stages
    lambda <- lambda / dims[i]
  }
  sevt$ll <- NULL ##force recompute log-likelihood
  sevt$ll <- logLik(sevt)
  return(sevt)
}

#' Staged event tree
#'
#' @param x A stratified event tree object
#' @param lambda smoothing of probabilities
#' @param ... additional parameters
#' @return The equivalent staged event tree object
#' @details The function creates a staged event tree equivalent to 
#' the stratified event tree
#' @export
staged_ev_tree.strt_ev_tree <- function(x, lambda = 0, ...) {
  obj <- staged_ev_tree.list(x$tree, full = TRUE)
  vars <- names(x$tree)
  dims <- sapply(x$tree, length)
  if (!is.null(x$ctables)) {
    obj$ctables <- x$ctables
    ## if the x is fitted we can just copy the probabilitites
    n <- sum(x$ctables[[ vars[1] ]])
    pp <- x$ctables[[ vars[1] ]] + lambda
    pp <- pp / sum(pp)
    obj$prob[[vars[1]]] <- list("1" = pp)
    attr(obj$prob[[vars[1]]][["1"]], "n") <- n
    #lambda <- lambda / dims[1]
    for (i in 2:length(x$tree)) {
      obj$prob[[vars[i]]] <-
        lapply(1:(dim(x$ctables[[vars[i]]])[1]), function(k) {
          n <- sum(x$ctables[[ vars[i] ]][k, ])
          pp <- (x$ctables[[ vars[i] ]][k, ] + lambda)
          pp <- pp / sum(pp)
          names(pp) <- x$tree[[vars[i]]]
          attr(pp, "n") <- n
          return(pp)
        })
      #lambda <- lambda / dims[i]
      names(obj$prob[[vars[i]]]) <-
        as.character(1:length(obj$prob[[vars[i]]]))
    }
    names(obj$prob) <- names(x$tree)
  }
  return(obj)
}


#' Set stage to path
#'
#' Set the given stage to the path for the stage event tree
#' @param sevt Staged event tree
#' @param path Vector of the path
#' @param stage stage to be assigned
set_stage <- function(sevt, path, stage) {
  stage <- as.character(stage)
  ## TO DO
  return(sevt)
}

#' Join two stages
#'
#' Probabilities are recomputed
#'
#' @param sevt staged event tree
#' @param v variable
#' @param s1 first stage
#' @param s2 second stage
#' @return the staged event tree where \code{s1} and \code{s2} are joined
#' @export
join_stages <- function(sevt, v,  s1, s2) {
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  d <- dim(sevt$paths[[v]])[2]
  st <- sevt$stages[[v]]
  sevt$stages[[v]][st == s2] <- s1
  if (!is.null(sevt$prob)) {
    n2 <- attr(sevt$prob[[v]][[s2]], "n") 
    n1 <- attr(sevt$prob[[v]][[s1]], "n")
    dll <- sum(sevt$prob[[v]][[s2]] * n2 * log(sevt$prob[[v]][[s2]])) + 
      sum(sevt$prob[[v]][[s1]] * n1 * log(sevt$prob[[v]][[s1]]))
    sevt$prob[[v]][[s1]] <- sevt$prob[[v]][[s2]] * n2 +
                            sevt$prob[[v]][[s1]] * n1 + sevt$lambda    
    attr(sevt$prob[[v]][[s1]], "n") <- n1 + n2
    sevt$prob[[v]][[s1]] <- sevt$prob[[v]][[s1]] / sum(sevt$prob[[v]][[s1]])
    sevt$prob[[v]][[s2]] <- NULL ##delete one of the two
    if (!is.null(sevt$ll)){## update log likelihood
      sevt$ll <- sevt$ll - dll + (n2 + n1) * sum(sevt$prob[[v]][[s1]] *  
        log(sevt$prob[[v]][[s1]] ))
      attr(sevt$ll, "df") <- attr(sevt$ll, "df") - length(sevt$prob[[v]][[s1]]) + 1 
    }
  }
  return(sevt)
}


#' split randomly a stage 
#' 
#' Randomly assign some of the path to a new stage
#' 
#' @param object a staged event tree object
#' @param var the variable where to split the stage
#' @param stage the name of the stage
#' @param p probability
#' 
#' @return a staged event tree object
#' @export
split_stage_random <- function(object, var,  stage, p = 0.5) {
  if (!(stage %in% object$stages[[var]])){
    return(object)
  }
  d <- length(object$stages[[var]])
  label <- new_label(object$stages[[var]])
  ix <- (object$stages[[var]] == stage ) & sample(x = c(TRUE, FALSE), size = d, 
                                                         prob = c(p, 1 - p), replace = TRUE )
  if (any(ix)){
    object$stages[[var]][ix] <- label
    if (is_fitted.staged_ev_tree(object)){
      object <- fit.staged_ev_tree(object, lambda = object$lambda)
    }
  }
  return(object)
}

#' Check if a staged event tree is fitted
#'
#' @param x a staged event tree object
#' @return logical
#'
#' @export
is_fitted.staged_ev_tree <- function(x) {
  return(!is.null(x$prob) && !is.null(x$ctables))
}




#' Print a staged event tree
#' 
#' @param x the staged event tree obejct
#' @param ... additional parameters (compatibility)
#' 
#' @return An invisible copy of \code{x}
#' @details The order of the variables in the stratified tree
#'  is printed (from root). In addition the number of levels of each 
#'  variable is shown in square brackets. 
#'  If available the log-likelihood of the model is printed.  
#' @export
print.staged_ev_tree <- function(x, ...){
  cat("Staged event tree", 
      ifelse(is_fitted.staged_ev_tree(x), "(fitted) \n", "\n"))
  ls <- vapply(x$tree, length, 1)
  cat(paste( paste0(names(x$tree), "[", ls, "] ") , collapse = "->"), "\n")
  #nstages <- vapply(x$stages, function(s) length(unique(s)), FUN.VALUE = 1)
  #cat("n.stages: \n")
  #print(nstages)
  if (x$ll){
    print(x$ll)
  }
  invisible(x)
}
