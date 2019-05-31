#' Stratified event tree
#'
#' Builds the complete stratified event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame or list
#' @param order Vector of variables names, the order to build the event tree
#' @param fit If `TRUE` the contingency tables will be computed from `data`
#' @param ... additional parameters
#' @export
#' @examples
#' a <- c(1,2)
strt_ev_tree <- function(x, order = NULL, fit = FALSE, ...) {
  UseMethod("strt_ev_tree", object = x)
}

#' Stratified event tree
#'
#' @param x object to be coerced to data.frame
#' @param ... additional parameters
#' @export
strt_ev_tree.default <- function(x, ...) {
  return(strt_ev_tree.data.frame(as.data.frame(x, ...)))
}

#' Stratified event tree from a table
#'
#' @param x table
#' @param order vector of order to build the tree
#' @param fit logical
#' @param lambda laplace smoothing parameter
#' @param ... additional parameters
#' @export
strt_ev_tree.table <- function(x,
                               order = names(dimnames(x)),
                               fit = FALSE,
                               lambda = 0,
                               ...) {
  evt <- strt_ev_tree.list(dimnames(x)[order])
  if (fit) {
    evt <- fit.strt_ev_tree(evt, data = x, lambda = lambda)
  }
  return(evt)
}

#' Stratified event tree from data.frame
#'
#' @param x data.frame
#' @param order vector of order to build the tree
#' @param fit logical
#' @param lambda laplace smoothing parameter
#' @param ... additional parameters
#' @export
strt_ev_tree.data.frame <- function(x,
                                    order = colnames(x),
                                    fit = FALSE,
                                    lambda = 0,
                                    ...) {
  evt <- strt_ev_tree.list(lapply(x, function(v)
    return(levels(as.factor(
      v
    ))))[order])
  if (fit) {
    evt <- fit.strt_ev_tree(evt, data = x, lambda = lambda)
  }
  return(evt)
}

#' Stratified event tree from list
#'
#' @param x a list with component named as the variables and containing the vector
#'          of factor
#' @param ... additional parameters
#' @return A stratified event tree object, that is a list with a `tree` component
#' @details Build the stratified event tree object from a named list containing the
#'          levels of the variables. The output is an object with the `tree` component.
#' @export
#'
strt_ev_tree.list <- function(x, ...) {
  if (is.null(names(x))) {
    #if there are no names of variables
    #we assign variables names V1,V2,...
    names(x) <- paste0("V", 1:length(x))
  }
  
  if (any(is.null(vapply(
    x, FUN = length, FUN.VALUE = 1
  )))) {
    #naive check if levels are vector with lenght
    warning("Levels should be well defined")
    return(NULL) #exit without nothing
  }
  evt <- list(tree = x)
  class(evt) <- "strt_ev_tree"
  return(evt)
}


#' Stratified event tree from a staged event tree
#'
#' @param x a staged event tree object
#' @param ... additional parameters
#' @return A stratified event tree object, that is a list with a `tree` component
#' @details This function build a stratified event tree object from a staged event tree.
#'          The staged event tree returned will have a different stage for each path.
#'          This is the function that can be used to initialize a staged event tree with
#'          the more complex structure.
#' @export
#'
strt_ev_tree.sevt <- function(x, ...) {
  vars <- names(x$tree)
  dims <- vapply(x$tree, length, FUN.VALUE = 1)
  if (!is.null(x$prob)) {
    #if the model was fitted we have to recompilate the ctables
    #the first one is easy we just have to forget the (only) stage (and we check validity)
    if (length(x$prob[[vars[1]]]) > 1) {
      warning("Incorrect number of stages in first variable (should be one)")
    }
    ## we recover the ctbales from the probability and the sample size
    x$prob[[vars[1]]] <- x$prob[[vars[1]]][[1]]
    for (i in 2:length(x$tree)) {
      #let's take care of the other variables
      ## we will create manually the ftable
      ## the dimension are the same as path (-1 for the column)
      ft <- array(dim = c(prod(dims[1:(i - 1)]), dims[i]))
      for (j in 1:(dim(ft)[1])) {
        ## fill the ftable
        jstage <- x$stages[[vars[i]]][j]
        ft[j,] <- x$prob[[vars[i]]][[jstage]]
        ###here we divide by the number of path in the same stage
      }
      attr(ft, "row.vars") <- x$tree[vars[1:(i - 1)]]
      attr(ft, "col.vars") <- x$tree[vars[i]]
      class(ft) <- "ftable"
      x$prob[[vars[i]]] <- ft
    }
  }
  x$stages <- NULL
  class(x) <- "strt_ev_tree"
  return(x)
}

#' Fit a stratified event tree
#'
#' @param  evt The stratified event tree object to be fitted
#' @param data the data.frame used to fit the event tree
#' @param lambda the laplace smoothing
#' @return A stratified event tree object with the conditional probabilities fitted
#' @export
fit.strt_ev_tree <- function(evt, data = NULL, lambda = 0) {
  if (is.null(data)) {
    data <- evt$data
    if (is.null(data)) {
      warning("Data must be provided or included in the model object")
      return(evt)
    }
  }
  if (is.data.frame(data)) {
    data <- table(data)
  }
  
  order <- names(evt$tree)
  
  evt$ctables <- lapply(1:length(order), function(i) {
    path <- order[i:1]
    tt <- apply(data, MARGIN = path, sum)
    if (i == 1) {
      return(tt)
    }
    return(ftable(tt, col.vars = order[i], row.vars = order[1:(i - 1)]))
  })
  names(evt$ctables) <- order
  evt$lambda <- lambda
  return(evt)
}
