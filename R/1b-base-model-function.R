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

  if (any(is.null(vapply(
    x, FUN = length, FUN.VALUE = 1
  )))) {
    #naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }
  if (full){
    return(staged_ev_tree.strt_ev_tree(strt_ev_tree.list(x, ...)))
  }
  evt <- list()
  evt$tree <- x
  evt$paths <- lapply(1:(length(x) - 1), function(i) {
    tt <- 
      expand.grid(evt$tree[i:1])[i:1] ## create all the possible paths
    tt[, dim(tt)[2] + 1] <-
      "1" #put the same color in all the paths
    return(tt)
  })
  evt$stages <- lapply(x[-1], function(xx)
    return("1"))
  names(evt$paths) <- names(x)[-1]
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
    data <- sevt$data
    if (is.null(data)) {
      warning("Data must be provided or included in the model object")
      return(sevt)
    }
  }
  sevt$lambda <- lambda
  order <- names(sevt$tree)
  data <- data[order] #order the data
  dims <- sapply(sevt$tree, length)
  sevt$prob <- list()
  tt <- table(data[order[1]]) + lambda
  attr(tt, "n") <- sum(tt)
  lambda <- lambda / dims[1]
  tt <- tt / attr(tt, "n") 
  sevt$prob[[order[1]]] <- list("1" = tt)
  for (i in 2:length(order)) {
    sevt$prob[[order[i]]] <-
      lapply(sevt$stages[[order[i]]], function(s) {
        dt <- data[, 1:i] #copy relevant data in dt
        pths <-
          find_paths(paths = sevt$paths[[order[i]]], s) #find all the paths in that stage
        for (j in 1:(length(pths) - 1)) {
          #for every step in the path ( -1 because last var is stage name)
          dt <-
            dt[dt[, j] %in% pths[, j] ,] #reduce dt to the observation that we need
        }
        tt <- table(dt[order[i]]) + lambda #table count plus lambda
        attr(tt, "n") <- sum(tt)
        return(tt / attr(tt, "n")) #return normalized prob
      })
    names(sevt$prob[[order[i]]]) <- sevt$stages[[order[i]]]
    lambda <- lambda / dims[i]
  }
  sevt$data <- data
  sevt$ll <- NULL
  sevt$ll <- logLik(sevt)
  return(sevt)
}

#' Staged event tree
#'
#' @param x A stratified event tree object
#' @param ... additional parameters
#' @return The equivalent staged event tree object
#' @details The function creates a staged event tree equivalent to 
#' the stratified event tree
#' @export
staged_ev_tree.strt_ev_tree <- function(x, ...) {
  x$stages <- list()
  x$paths <- list()
  vars <- names(x$tree)
  for (i in 1:(length(x$tree) - 1)) {
    tt <- expand.grid(x$tree[i:1])[i:1] ## create all the possible paths
    tt[, dim(tt)[2] + 1] <-
      as.character(1:(dim(tt)[1])) #put different colors/stages
    x$stages[[i]] <- as.character(1:(dim(tt)[1]))
    x$paths[[i]] <- tt
  }
  names(x$paths) <- names(x$tree)[-1]
  names(x$stages) <- names(x$tree)[-1]
  class(x) <- "staged_ev_tree"

  if (!is.null(x$prob)) {
    ## if the x is fitted
    x$prob[[vars[1]]] <- list("1" = x$prob[[vars[1]]])
    attr(x$prob[[vars[1]]][["1"]], "n") <- 1
    for (i in 2:length(x$tree)) {
      x$prob[[vars[i]]] <-
        lapply(1:(dim(x$prob[[vars[i]]])[1]), function(k) {
          pp <- x$prob[[ vars[i] ]][k, ]
          names(pp) <- x$tree[[vars[i]]]
          attr(pp, "n") <- 1
          return(pp)
        })
      names(x$prob[[vars[i]]]) <-
        as.character(1:length(x$prob[[vars[i]]]))
    }
    names(x$prob) <- names(x$tree)
  }
  return(x)
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
  dt <- sevt$data[names(sevt$tree)]
  stgs <- sevt$stages[[v]]
  stgs[stgs == s2] <- s1
  sevt$stages[[v]] <- unique(stgs)
  pths <- sevt$paths[[v]]
  d <- dim(pths)[2]
  st <- pths[, d]
  sevt$paths[[v]][st == s2, d] <- s1
  if (!is.null(sevt$prob)) {
    n2 <- attr(sevt$prob[[v]][[s2]], "n") 
    n1 <- attr(sevt$prob[[v]][[s1]], "n")
    dll <- sum(sevt$prob[[v]][[s2]] * n2 * log(sevt$prob[[v]][[s2]])) + 
      sum(sevt$prob[[v]][[s1]] * n1 * log(sevt$prob[[v]][[s1]]))
    sevt$prob[[v]][[s1]] <- sevt$prob[[v]][[s2]] * n2 +
                            sevt$prob[[v]][[s1]] * n1    
    attr(sevt$prob[[v]][[s1]], "n") <- n1 + n2
    sevt$prob[[v]][[s1]] <- sevt$prob[[v]][[s1]] / attr(sevt$prob[[v]][[s1]], "n")
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
#' @param stratum the variable where to split the stage
#' @param stage the name of the stage
#' @param p probability
#' 
#' @return a staged event tree object
#' @export
split_stage_random <- function(object, stratum,  stage, p = 0.5) {
  if (!(stage %in% object$stages[stratum])){
    return(object)
  }
  label <- new_label(object$stages[[stratum]])
  d <- dim(object$paths[[stratum]])
  ix <- (object$paths[[stratum]][,d[2]] == stage ) & sample(x = c(TRUE, FALSE), size = d[1], 
                                                         prob = c(p, 1 - p), replace = TRUE )
  if (any(ix)){
    object$paths[[stratum]][ix, d[2]] <- label
    object$stages[[stratum]] <- c(object$stages[[stratum]], label)
    
    if (is_fitted.staged_ev_tree(object)){
      object <- fit.staged_ev_tree(object)
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
  return(!is.null(x$prob))
}

#' give a safe-to-add label that is not in \code{labels}
#' INTERNAL USE
#' 
#' @param labels vector of labels (strings)
#' 
#' @return a string label that is different from each \code{labels}
new_label <- function(labels){
  k <- 1
  labels <- as.character(labels)
  while (TRUE){
    if (!(as.character(k) %in% labels)){
      return(as.character(k))
    }
    k <- k + 1
  }
}
