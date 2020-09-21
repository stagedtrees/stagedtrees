#' Staged event tree (sevt) class
#'
#' Structure and usage of S3 class \code{sevt}
#' used to store a staged event tree. 
#' @details  A staged event tree object is a list with components:
#' \itemize{
#'          \item tree (required): A named list where for each variable,
#'                      the levels of such variable are listed.
#'                      The order of the variable is the
#'                      order of the event tree.
#'          \item stages (required): A named list where each component
#'                        stores the stages for the given variable.
#'          \item ctables: The contingency tables of the data.
#'          \item lambda: The smoothing parameter used to estimate the model.
#'          \item prob: The conditional probability tables for every
#'                      variable and every stage.
#'          \item ll: The log-likelihood of the \code{estimated} model.
#'        }
#'        The tree structure is never defined explicitly, but instead 
#'        it is defined by the list \code{tree} containing the order
#'        of the variables and the names of their levels. This is 
#'        sufficient to define a complete symmetric tree where an 
#'        internal node at a depth related to a variable \code{v} 
#'        has a number of children equal to the cardinality of
#'        the levels of \code{v}.
#'        The stages informations is instead stored as a list of
#'        vectors, where each vector is indexed as the internal nodes
#'        of the tree at a given depth. 
#' @name sevt class
NULL

#' Build a staged event tree
#' 
#' @param x a list, a data frame or a table with data
#' @param full logical, if TRUE the full model is created 
#'              otherwise the independence model.
#' @param ... additional parameters to be passed to the particular 
#'            method.
#' @export
staged_ev_tree <- function(x, full = FALSE, ...) {
  UseMethod("staged_ev_tree", object = x)
}

#' @rdname staged_ev_tree
#' @export
staged_ev_tree.default <- function(x, ...) {
  return(staged_ev_tree.data.frame(as.data.frame(x, ...)))
}

#' @rdname staged_ev_tree
#' @param order order of the variables to build the 
#'              tree, by default the order of the variables
#'              in the table \code{x}. 
#' @export
staged_ev_tree.table <- function(x,
                                 full = FALSE,
                                 order = names(dimnames(x)),
                                 ...) {
  # extract ordered list of levels
  tree <- dimnames(x)[order]
  # check if tree exist
  stopifnot(!is.null(tree))
  # build staged tree from list
  staged_ev_tree.list(tree, full = full)
}

#' @rdname staged_ev_tree
#' @param order order of the variables to build the 
#'              tree, by default the order of the variables
#'              in the data frame \code{x}.
#' @export
#' @examples
#'
#' ######### from dataset
#' DD <- generate_random_dataset(n = 4, 1000)
#' indep <- staged_ev_tree(DD, fit = TRUE)
#' full <- staged_ev_tree(DD, full = TRUE, fit = TRUE, lambda = 1)
#' 
staged_ev_tree.data.frame <- function(x,
                                      full = FALSE,
                                      order = colnames(x),
                                      ...) {
  # extract ordered list of levels
  tree <- lapply(x, function(v) {
    return(levels(as.factor(v)))
  })[order]
  # build staged tree from list 
  sevt <- staged_ev_tree.list(tree, full = full)
  return(sevt)
}

#' Build a staged event tree 
#' @export
#' @examples
#'
#' ######### from list
#' model <- staged_ev_tree(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
staged_ev_tree.list <- function(x, full = FALSE, ...) {
  if (is.null(names(x))) {
    # if there are no names of variables
    # we assign variables names V1,V2,...
    names(x) <- paste0("V", seq_along(x))
  }
  
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
#' Crete the compelte probability tables
#' @param object a fitted staged event tree object
#' @return probability tables
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

#' Add data to a stratified event tree
#'
#' Add the \code{ctables} field with data distributed along 
#' the path of the tree.
#' @param object A stratified event tree, a list with a \code{tree} field
#' @param data table or data.frame containing observations 
#'             of the variable in \code{object}
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
#' @param object a staged event tree object
#' @return logical 
#' @keywords internal
has_ctables <- function(object){
  isFALSE(is.null(object$ctables))
}

#' Check if the stages event tree has probabilitites
#' 
#' @param object a staged event tree object
#' @return logical 
#' @keywords internal
has_prob <- function(object){
  isFALSE(is.null(object$prob))
}