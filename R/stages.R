#' The stages of a staged event tree
#'
#' Functions to get or set the stages of an object of class
#' \code{sevt}.
#' @param object an object of class \code{sevt}.
#' @param x an object of class \code{sevt.stgs}
#'          (obtained by \code{stages(object)}).
#' @param i index of variables in the tree.
#' @param ... a path or context in the event tree.
#' @param value the stages replacement value.
#' @param fit logical, if TRUE (default) the model will be re-fitted.
#' @return For \code{stages()}: returns an object of class
#'         \code{sevt.stgs} which encode the stages of \code{object}.
#'         Objects of class \code{sevt.stgs} have dedicated
#'         method for sub-setting and replacing.
#' @details
#'
#' This functions are the preferred way to access and modify directly
#' the stages of an object of class \code{sevt}.
#' In particular the indexing and replacing methods for the
#' object extracted with the function \code{stages()} take care of checking
#' the stages sanity and refit the object probabilities when needed.
#' This is useful for manually setting some independence statements
#' (see the Examples).
#'
#' @section Stages indexing:
#' Stages can be indexed, retrieved and replaced
#' by the corresponding variables names and/or by
#' paths or contexts.
#'
#' In particular,
#' \code{stages(object)[[var]]} extracts the
#' stages vector corresponding to variable \code{var} (similarly
#' to \code{object$stages[[var]]}.
#' Alternatively \code{stages(object)[[path]]} indexes
#' a stage via the corresponding path from root
#' (similar to \code{\link{get_stage}}); a path is
#' recognized as such if named or if of length > 2.
#'
#' \code{stages(object)[var, context]} extracts multiple stages
#' corresponding to a variable and eventually filtered by
#' a specific context on the preceding variables.
#'
#' @examples
#' # start with full model
#' mod <- full(Titanic)
#'
#' # impose the context independence Survived indep Sex, Age | Class = 1st
#' stages(mod)["Survived", Class = "1st"] <- "C1"
#'
#' # impose Survived indep Class | Class in (2nd 3rd)
#' stages(mod)["Survived", Class = "3rd"] <- stages(mod)["Survived", Class = "2nd"]
#'
#' # impose Age indep Class | Sex
#' stages(mod)["Age", Sex = "Female"] <- "S-female"
#' stages(mod)["Age", Sex = "Male"] <- "S-male"
#'
#' # stages of Survived
#' stages(mod)[["Survived"]]
#'
#' # stages of Survived and Age
#' stages(mod)[c("Survived", "Age")]
#'
#' # stages of Survived in the context Class 2nd or 3rd
#' stages(mod)["Survived", Class = c("2nd", "3rd")]
#'
#' # check independencies
#' as_parentslist(mod)
#' @export
stages <- function(object) {
  UseMethod("stages", object)
}

#' @rdname stages
#' @export
stages.sevt <- function(object) {
  check_sevt(object)
  res <- object$stages
  if (is.null(res[[sevt_varnames(object)[1]]])) {
    res[[sevt_varnames(object)[1]]] <- "NA"
  }
  res <- res[sevt_varnames(object)]
  attr(res, "tree") <- object$tree
  class(res) <- "sevt.stgs"
  return(res)
}

#' @rdname stages
#' @param max integer, limit on the number of variables to print.
#' @export
print.sevt.stgs <- function(x, ..., max = 5) {
  x <- unclass(x)
  tree <- attr(x, "tree")
  cat("Stage structure", "\n")
  cat(tree_string(tree, max = max), "\n")
  mm <- min(max, length(x))
  for (v in names(x)[1:mm]) {
    cat(v, ":", x[[v]], "\n")
  }
  if (mm < length(x)) {
    cat("[ reached 'max' -- omitted ", length(x) - mm, " stage vectors ]\n")
  }
  class(x) <- "sevt.stgs"
  invisible(x)
}


#' @rdname stages
#' @export
"stages<-" <- function(object, value) {
  check_sevt(object)
  value <- unclass(value)
  tofit <- attr(value, "_tofit")
  attr(value, "_tofit") <- NULL
  attr(value, "tree") <- NULL
  object$stages <- value
  check_stages(object)
  if (is_fitted_sevt(object)) {
    if (isFALSE(tofit)) {
      erase_fit(object)
    } else {
      sevt_fit(object, scope = tofit)
    }
  } else {
    erase_fit(object)
  }
}

#' @rdname stages
#' @export
"[.sevt.stgs" <- function(x, i, ...) {
  check_scope(i, x)
  narg <- ...length()
  if (narg == 0) {
    # we assume here i are  variables
    return(unclass(x)[i])
  } else {
    if (length(i) > 1) {
      cli::cli_abort(c(
        "Only one variable can be selected when specifing a context.",
        "x" = "You've supplied {length(i)} variables as {.arg i}"
      ))
    }
    path <- list(...)
    if (narg == 1){ ## check if ... is a context
      if (!is.null(names(path[[1]]))){
        path <- path[[1]]
      }
    }
    tree <- attr(x, "tree")
    check_context(path, i, tree)
    ixv <- which(i == names(tree))
    tree1 <- tree[1:(ixv - 1)]
    tree1[names(path)] <- as.list(path)
    more <- rev(expand.grid(rev(tree1), stringsAsFactors = FALSE))
    ixs <- vapply(1:nrow(more), function(rr) {
      cpath <- unlist(more[rr, ])
      tree_idx(cpath, tree)
    }, FUN.VALUE = 1)
    return(unclass(x)[[i]][ixs])
  }
}

#' @rdname stages
#' @export
"[<-.sevt.stgs" <- function(x, i, ..., fit = TRUE, value) {
  check_scope(i, x)
  if (!is.character(value)) {
    cli::cli_abort(c(
      "{.arg value} must be of type character.",
      "x" = "You've supplied {.type {value}}."
    ))
  }
  x <- unclass(x)
  narg <- ...length()
  if (narg == 0) {
    # we assume here i are variables
    # TODO
    x[i] <- lapply(x[i], function(s) {
      s[] <- value
      return(s)
    })
    if (fit) {
      attr(x, "_tofit") <- i
    } else {
      attr(x, "_tofit") <- FALSE
    }
  } else {
    if (length(i) > 1) {
      cli::cli_abort(c(
        "Only one variable can be selected when specifing a context.",
        "x" = "You've supplied {length(i)} variables as {.arg i}"
      ))
    }
    path <- list(...)
    if (narg == 1){ ## check if ... is a context
      if (!is.null(names(path[[1]]))){
        path <- path[[1]]
      }
    }
    tree <- attr(x, "tree")
    check_context(path, i, tree)
    ixv <- which(i == names(tree))
    tree1 <- tree[1:(ixv - 1)]
    tree1[names(path)] <- as.list(path)
    more <- rev(expand.grid(rev(tree1), stringsAsFactors = FALSE))
    ixs <- vapply(1:nrow(more), function(rr) {
      cpath <- unlist(more[rr, ])
      tree_idx(cpath, tree)
    }, FUN.VALUE = 1)
    x[[i]][ixs] <- value
    if (fit) {
      attr(x, "_tofit") <- i
    } else {
      attr(x, "_tofit") <- FALSE
    }
  }
  class(x) <- "sevt.stgs"
  return(x)
}

#' @rdname stages
#' @export
"[[.sevt.stgs" <- function(x, ...) {
  arg <- list(...)
  narg <- ...length()
  if (narg == 1 && is.null(names(arg)) &&
    length(arg[[1]]) == 1 && is.null(names(arg[[1]]))) {
    return(unclass(x)[[arg[[1]]]])
  } else {
    # assume ... is a path
    path <- unlist(arg)
    n <- length(path)
    tree <- attr(x, "tree")
    if (n >= length(tree)) {
      cli::cli_abort(c(
        "Subscript out of bounds."
      ))
    }
    check_path(path, tree)
    # index the tree with path
    ix <- tree_idx(path, tree)
    return(unclass(x)[[n + 1]][ix])
  }
}

#' @rdname stages
#' @export
"[[<-.sevt.stgs" <- function(x, ..., fit = TRUE, value) {
  arg <- list(...)
  narg <- ...length()
  tree <- attr(x, "tree")
  if (!is.character(value)) {
    cli::cli_abort(c(
      "{.arg value} must be of type character.",
      "x" = "You've supplied {.type {value}}."
    ))
  }
  if (narg == 1 && is.null(names(arg)) &&
    length(arg[[1]]) == 1 && is.null(names(arg[[1]]))) {
    check_scope(arg[[1]], x)
    x <- unclass(x)
    x[[arg[[1]]]][] <- value
    if (fit) {
      attr(x, "_tofit") <- arg[[1]]
    } else {
      attr(x, "_tofit") <- FALSE
    }
  } else {
    # assume ... is a path
    path <- unlist(arg)
    n <- length(path)
    if (n >= length(tree)) {
      cli::cli_abort(c(
        "Subscript out of bounds."
      ))
    }
    check_path(path, tree)
    # index the tree with path
    ix <- tree_idx(path, tree)
    x <- unclass(x)
    x[[names(tree)[n + 1]]][ix] <- value
    if (fit) {
      attr(x, "_tofit") <- names(tree)[n + 1]
    } else {
      attr(x, "_tofit") <- FALSE
    }
  }
  class(x) <- "sevt.stgs"
  return(x)
}
