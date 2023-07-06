#' Staged event tree (sevt) class
#'
#' Structure and usage of S3 class \code{sevt},
#' used to store a staged event tree.
#' @details  A staged event tree object is a list with components:
#' \itemize{
#'          \item tree (required): A named list with one component
#'                      for each variable in the model,
#'                      a character vector with the names of
#'                      the levels for that variable.
#'                      The order of the variables in \code{tree} is the
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
#'          \item name_unobserved: The stage name for unobserved situations.
#'          \item prob: The conditional probability tables for every
#'                      variable and stage. Stored in a named list with
#'                      one component for each variable, a list with
#'                      one component for each stage.
#'          \item ll: The log-likelihood of the \code{estimated} model.
#'                    If present, \code{\link{logLik.sevt}} will
#'                    return this value instead of computing the log-likelihood.
#'        }
#'        The tree structure is never defined explicitly, instead it
#'        is implicitly defined by the list \code{tree} containing the order
#'        of the variables and the names of their levels. This is
#'        sufficient to define a complete symmetric tree where an
#'        internal node at a depth related to a variable \code{v}
#'        has a number of children equal to the cardinality of
#'        the levels of \code{v}.
#'        The stages information is instead stored as a list of
#'        vectors, where each vector is indexed as the internal nodes
#'        of the tree at a given depth.
#'
#' To define a staged tree from data (data frame or table) the
#' user can call either \code{\link{full}} or \code{\link{indep}}
#' which both construct the staged tree object, attach the data in
#' \code{ctables} and compute probabilities. After, one of the
#' available model selection algorithm can be used, see for example
#' \code{\link{stages_hc}}, \code{\link{stages_bhc}} or
#' \code{\link{stages_hclust}}.
#' If, mainly for development, only the staged tree structure is needed
#' (without data or probabilities) the basic
#' \code{\link{sevt}} constructor can
#' be used.
#' @name sevt
NULL

#' @rdname sevt
#' @param x a list, a data frame or table object.
#' @param full logical, if TRUE the full model is created
#'              otherwise the independence model.
#' @param order character vector,
#'              order of the variables to build the
#'              tree, by default the order of the variables
#'              in \code{x}.
#' @return A staged event tree object, an object of class \code{sevt}.
#'
#' @export
sevt <- function(x, full = FALSE, order = NULL) {
  UseMethod("sevt", object = x)
}

#' @rdname sevt
#' @examples
#'
#' ######### from table
#' model.titanic <- sevt(Titanic, full = TRUE)
#' @export
sevt.table <- function(x,
                       full = FALSE,
                       order = names(dimnames(x))) {
  # extract ordered list of levels
  tree <- dimnames(x)[order]
  # check if tree exist
  if (is.null(tree)) {
    cli::cli_abort(c(
      "{.arg x} must be a
      table with named dimensions including variables
      listed in {.arg order}.",
      "x" = "Unable to extract valid tree from table dimnames."
    ))
  }
  # build staged tree from list
  sevt.list(tree, full = full)
}

#' @rdname sevt
#' @export
#' @examples
#'
#' ######### from data frame
#' DD <- generate_random_dataset(p = 4, n = 1000)
#' model.indep <- sevt(DD)
#' model.full <- sevt(DD, full = TRUE)
sevt.data.frame <- function(x,
                            full = FALSE,
                            order = colnames(x)) {
  # extract ordered list of levels
  tree <- lapply(x, function(v) {
    return(levels(as.factor(v)))
  })[order]
  # build staged tree from list
  sevt <- sevt.list(tree, full = full)
  return(sevt)
}

#' @rdname sevt
#' @export
#' @examples
#'
#' ######### from list
#' model <- sevt(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
sevt.list <- function(x, full = FALSE, order = names(x)) {
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
    cli::cli_abort(c(
      "{.arg x} must be a list defining levels for each variables.",
      "x" = "Unable to extract proper levels from {.arg x}."
    ))
  }
  if (any(dims == 0)) {
    # naive check if levels are vector with length
    cli::cli_abort(c(
      "{.arg x} must be a list defining levels for each variables.",
      "x" = "Unable to extract proper levels from {.arg x}."
    ))
  }

  # initialize empty object
  evt <- list()
  # store tree (ordered list of levels)
  evt$tree <- x
  # if only one variable do not build stages
  if (length(evt$tree) > 1) {
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
  } else {
    evt$stages <- list()
  }
  # assign class name
  class(evt) <- "sevt"
  # return staged tree object
  return(evt)
}
