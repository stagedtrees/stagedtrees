#' Staged event tree (sevt) class
#'
#' Structure and usage of S3 class \code{sevt},
#' used to store a staged event tree. 
#' @details  A staged event tree object is a list with components:
#' \itemize{
#'          \item tree (required): A named list with one component
#'                      for each variable in the model,
#'                      a character vector withe the names of
#'                      the levels for that variable.
#'                      The order of the variable is the
#'                      order of the event tree.
#'          \item stages (required): A named list with one component
#'                        for each variable but the first, 
#'                        a character vector storing the stages for 
#'                        the situations related to path ending in that 
#'                        variable.
#'          \item ctables: A named list with one component 
#'          for each variable, the flat contingency table of that variable
#'          given the previous variables.
#'          \item lambda: The smoothing parameter used to compute probabilities.
#'          \item prob: The conditional probability tables for every
#'                      variable and stage. Stored in a named list with 
#'                      one component for each variable, a list with
#'                      one component for each stage,
#'          \item ll: The log-likelihood of the \code{estimated} model.
#'                    If present, \code{\link{logLik.sevt}} will 
#'                    return this value instead of computing the log-likelihood.
#'        }
#'        The tree structure is never defined explicitly, but instead 
#'        is implicitly defined by the list \code{tree} containing the order
#'        of the variables and the names of their levels. This is 
#'        sufficient to define a complete symmetric tree where an 
#'        internal node at a depth related to a variable \code{v} 
#'        has a number of children equal to the cardinality of
#'        the levels of \code{v}.
#'        The stages informations is instead stored as a list of
#'        vectors, where each vector is indexed as the internal nodes
#'        of the tree at a given depth. 
#'        
#' To define a staged tree from data (data frame or table) the 
#' user can call either \code{\link{full}} or \code{\link{indep}}
#' which both construct the staged tree object, attach the data in 
#' \code{ctables} and compute probabilities. After, one of the 
#' available model selection algorithm can be used, see for example 
#' \code{\link{hc}}, \code{\link{bhc}} or 
#' \code{\link{stages_hclust}}. 
#' If, mainly for development, only the staged tree structure is needed 
#' (without data or probabilities) the basic 
#' \code{\link{staged_ev_tree}} constructor can 
#' be used.
#' @name sevt class
NULL

#' Build a staged event tree
#' 
#' Basic constructor for staged event tree class (\code{sevt}).
#' @param x a list, a data frame or table object.
#' @param full logical, if TRUE the full model is created 
#'              otherwise the independence model.
#' @param order character vector, 
#'              order of the variables to build the 
#'              tree, by default the order of the variables
#'              in \code{x}.
#' @return A staged event tree object, element of \code{sevt} 
#' class.
#' @export
staged_ev_tree <- function(x, full = FALSE, order = NULL) {
  UseMethod("staged_ev_tree", object = x)
}

#' @rdname staged_ev_tree
#' @examples 
#' 
#' ######### from table
#' model.titanic <- staged_ev_tree(Titanic, full = TRUE) 
#' @export
staged_ev_tree.table <- function(x,
                                 full = FALSE,
                                 order = names(dimnames(x))) {
  # extract ordered list of levels
  tree <- dimnames(x)[order]
  # check if tree exist
  stopifnot(!is.null(tree))
  # build staged tree from list
  staged_ev_tree.list(tree, full = full)
}

#' @rdname staged_ev_tree
#' @export
#' @examples
#'
#' ######### from data frame
#' DD <- generate_random_dataset(n = 4, 1000)
#' indep <- staged_ev_tree(DD)
#' full <- staged_ev_tree(DD, full = TRUE)
staged_ev_tree.data.frame <- function(x,
                                      full = FALSE,
                                      order = colnames(x)) {
  # extract ordered list of levels
  tree <- lapply(x, function(v) {
    return(levels(as.factor(v)))
  })[order]
  # build staged tree from list 
  sevt <- staged_ev_tree.list(tree, full = full)
  return(sevt)
}

#' @rdname staged_ev_tree
#' @export
#' @examples
#'
#' ######### from list
#' model <- staged_ev_tree(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
staged_ev_tree.list <- function(x, full = FALSE, order = names(x)) {
  if (is.null(names(x))) {
    # if there are no names of variables
    # we assign variables names V1,V2,...
    names(x) <- paste0("V", seq_along(x))
    order <- names(x)
  }
  
  x <- x[order[order %in% names(x)]]
  # extract number of levels for each variable
  dims <- vapply(x, FUN = length, FUN.VALUE = 1)
  if (any(is.null(dims))) {
    # naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }
  if (any(dims == 0)) {
    # naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }
  
  # initialize empty object
  evt <- list()
  # store tree (ordered list of levels)
  evt$tree <- x
  # if a full staged tree is required 
  # build vector of different stages
  if (full) {
    evt$stages <- lapply(2:length(x), function(i) {
      as.character(1:prod(dims[1:(i - 1)]))
    })
  } else {
    # otherwise the independence model is built
    # using the same stage "1"
    evt$stages <- lapply(2:length(x), function(i) {
      rep("1", prod(dims[1:(i - 1)]))
    })
  }
  # stages should be a named list
  names(evt$stages) <- names(x)[-1]
  # assign class name 
  class(evt) <- "sevt"
  # return staged tree object
  return(evt)
}


#' Expand probabilities of a staged event tree
#'
#' Return the list of complete probability tables.
#' @param object a fitted staged event tree object.
#' @return probability tables.
#' @keywords internal
expand_prob <- function(object) {
  stopifnot(is_fitted_sevt(object))
  prob <- list()
  vars <- names(object$tree)
  dims <- vapply(object$tree, length, FUN.VALUE = 1)
  if (!is.null(object$prob)) {
    # the first one is easy we just have to forget the (only) stage 
    # (and we check validity)
    if (length(object$prob[[vars[1]]]) > 1) {
      warning("Incorrect number of stages in first variable (should be one)")
    }
    prob[[vars[1]]] <- object$prob[[vars[1]]][[1]]
    for (i in 2:length(object$tree)) {
      # let's take care of the other variables
      ## we will create manually the ftable
      ## the dimension are the same as path (-1 for the column)
      ft <- array(dim = c(prod(dims[1:(i - 1)]), dims[i]))
      for (j in 1:(dim(ft)[1])) {
        ## fill the ftable
        jstage <- object$stages[[vars[i]]][j]
        ft[j, ] <- object$prob[[vars[i]]][[jstage]]
      }
      attr(ft, "row.vars") <- object$tree[vars[1:(i - 1)]]
      attr(ft, "col.vars") <- object$tree[vars[i]]
      class(ft) <- "ftable"
      prob[[vars[i]]] <- ft
    }
  }
  return(prob)
}

#' Distribute counts along tree
#'
#' Create the list of \code{ftable}s 
#' storing the observations distributed along 
#' the path of the tree.
#' @param object A stratified event tree, a list with a \code{tree} field.
#' @param data table or data.frame containing observations 
#'             of the variable in \code{object}.
#' @return  A list of \code{ftable}s.
#' @details Distribute the counts along the event tree.
#'          This is an internal function, the user will 
#'          usually just directly fit the staged event tree 
#'          model using \code{sevt.fit}.
#'          We refer here to stratified event tree, because actually 
#'          the stage information is never used and thus this function
#'          will work for an object with only a \code{tree} field.
#' @keywords internal
#' @importFrom stats ftable
make_ctables <- function(object, data) {
  order <- names(object$tree)
  if (is.data.frame(data)) {
    data <- table(data[, order])
  }
  stopifnot(is.table(data))
  ctables <- lapply(seq_along(order), function(i) {
    path <- order[i:1]
    tt <- apply(data, MARGIN = path, sum)
    if (i == 1) {
      return(tt)
    }
    return(ftable(tt, col.vars = order[i], row.vars = order[1:(i - 1)]))
  })
  names(ctables) <- order
  return(ctables)
}

#' Check if the stages event tree has ctables field
#' 
#' @param object a staged event tree object.
#' @return logical.
#' @keywords internal
has_ctables <- function(object){
  isFALSE(is.null(object$ctables))
}

#' Check if the stages event tree has probabilitites
#' 
#' @param object a staged event tree object.
#' @return logical.
#' @keywords internal
has_prob <- function(object){
  isFALSE(is.null(object$prob))
}