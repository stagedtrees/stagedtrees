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
#' @param order vector of order, default to the order of the columns of `x`
#' @param method String the type of model selection performed
#' @param fit logical
#' @param ... additional parameters to be passed to other methods (see details)
#' @return A staged event tree object
#' @details
#' @export
staged_ev_tree.data.frame <- function(x,
                                      order = colnames(x)
                                      ,
                                      method = "indep"
                                      ,
                                      fit = TRUE
                                      ,
                                      ...) {
  switch (
    method,
    full = {
      evt <- staged_ev_tree(strt_ev_tree(x, fit = FALSE, ...))
      return(fit.staged_ev_tree(evt, data = x, ...))
      },
    indep = {
      evt <- staged_ev_tree.list(lapply(x, function(v)
        return(levels(as.factor(
          v
        ))))[order])
      if (fit) {
        return(fit.staged_ev_tree(evt, data = x, ...))
      } else {
        return(evt)
      }
    },
    back_HC = return(backward_hill_climb(data = x,
                                         order = order, ...)),
    fast_back_HC = return(fast_backward_hill_climb(data = x,
                                                   order = order, ...)),
    forw_HC = return(NULL),
    back_join_KL = return(backward_joining(data = x, order = order, ...)),
    return(staged_ev_tree(strt_ev_tree(x, fit = fit, ...)))
  )
}


#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x list named as the variable and containing the vector of the levels
#'          for each variable
#' @param full if the full models should be created instead
#' @param ... additional parameters
#' @return The staged event tree object
#' @details The staged (stratified) event tree returned is the minimal one,
#'          that is the one with just one stage per variable (equivalent to a complete independent model).
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
  order <- names(sevt$tree)
  data <- data[order] #order the data
  dims <- lapply(sevt$tree, length)
  sevt$prob <- list()
  tt <- table(data[order[1]]) + lambda
  attr(tt, "n") <- sum(tt)
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
  }
  sevt$data <- data
  sevt$lambda <- lambda
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
          pp <- x$prob[[vars[i]]][k, ]
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
      sevt$ll <- sevt$ll - dll + sum(sevt$prob[[v]][[s1]] * (n2 + n1) * 
        log(sevt$prob[[v]][[s1]]))
      attr(sevt$ll, "df") <- attr(sevt$ll, "df") - length(sevt$prob[[v]][[s1]]) + 1 
    }
  }
  return(sevt)
}


split_stage <- function(sevt, level,  stage, method = "rand") {
  ## to do
  return(sevt)
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
