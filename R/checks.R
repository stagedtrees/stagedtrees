#' @rdname check_sevt
#' @return logical.
#' @keywords internal
has_ctables <- function(object) {
  isFALSE(is.null(object$ctables))
}

#' @rdname check_sevt
#' @return logical.
#' @keywords internal
has_prob <- function(object) {
  if (isTRUE(is.null(object$prob))) {
    return(FALSE)
  } else {
    ## check that we have all probabilities
    vars <- sevt_varnames(object)
    if (isTRUE(any(sapply(vars, function(v) is.null(object$prob[[v]]))))) {
      return(FALSE)
    } else {
      ## check probabilities are ok
      isFALSE(any(sapply(vars, function(v) {
        any(sapply(object$prob[[v]], function(pp) {
          isFALSE(identical(length(pp), length(object$tree[[v]])))
        }))
      })))
    }
  }
}

#' @rdname check_sevt
#' @return logical.
#' @keywords internal
is_fitted_sevt <- function(object) {
  has_prob(object) && has_ctables(object)
}


#' Check sevt objects
#'
#' @param object an object of class sevt
#' @param arg passed arg name
#' @param call passed call
#' @keywords internal
check_sevt <- function(object, arg = rlang::caller_arg(object),
                       call = rlang::caller_env()) {
  if (!is.object(object) | !inherits(object, "sevt")) {
    cli::cli_abort(c(
      "{.arg {arg}} must be of class {.cls sevt}.",
      "x" = "You've supplied a {.cls {class(object)}} object."
    ), call = call, arg = arg)
  }
  if (is.null(object$tree)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a valid object of class {.cls sevt}.",
      "i" = "An object of class {.cls sevt} must have
      a {.field tree} component.",
      "x" = "You've supplied {.arg {arg}} without a {.field tree} component."
    ), call = call, arg = arg)
  }
  check_tree(object$tree, arg = arg, call = call)
  if (is.null(object$stages)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a valid object of class {.cls sevt}.",
      "i" = "An object of class {.cls sevt} must have
      a {.field stages} component.",
      "x" = "You've supplied {.arg {arg}} without a {.field stages} component."
    ), call = call, arg = arg)
  }
  check_stages(object, arg = arg, call = call)
}

#' @rdname check_sevt
#' @param tree tree to be checked, a named list
#' @keywords internal
check_tree <- function(tree, arg = rlang::caller_arg(tree),
                       call = rlang::caller_env()) {
  if (is.null(names(tree))) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a valid object of class {.cls sevt}.",
      "i" = "An object of class {.cls sevt} must have
      a named {.field tree} component.",
      "x" = "You've supplied {.arg {arg}} where the {.field tree}
      component is not named."
    ), call = call, arg = arg)
  }
}

#' @rdname check_sevt
#' @keywords internal
check_stages <- function(object, arg = rlang::caller_arg(object),
                         call = rlang::caller_env()) {
  if (is.null(names(object$stages)) && sevt_nvar(object) > 1) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a valid object of class {.cls sevt}.",
      "i" = "An object of class {.cls sevt} must have
      a named {.field stages} component.",
      "x" = "You've supplied {.arg {arg}} where the {.field stages}
      component is not named."
    ), call = call, arg = arg)
  }
}

#' @rdname check_sevt
#' @keywords internal
check_sevt_prob <- function(object, arg = rlang::caller_arg(object),
                            call = rlang::caller_env()) {
  check_sevt(object)
  if (!has_prob(object)) {
    cli::cli_abort(c(
      "{.arg {arg}} must have associated {.field prob} field.",
      "x" = "You've supplied {.arg {arg}} without probabilities in
      {.field prob}.",
      "i" = "Use {.fun stagedtrees::sevt_fit} to associate data and
      compute probabilities for an object of class {.cls sevt}."
    ), call = call, arg = arg)
  }
}


#' @rdname check_sevt
#' @keywords internal
check_sevt_ctables <- function(object, arg = rlang::caller_arg(object),
                               call = rlang::caller_env()) {
  check_sevt(object)
  if (!has_ctables(object)) {
    cli::cli_abort(c(
      "{.arg {arg}} must have associated {.field ctables} field.",
      "x" = "You've supplied {.arg {arg}} without data in  {.field ctables}.",
      "i" = "Use {.fun stagedtrees::sevt_fit} to associate data and compute
      probabilities for an object of class {.cls sevt}."
    ), call = call, arg = arg)
  }
}

#' @rdname check_sevt
#' @keywords internal
check_sevt_fit <- function(object, arg = rlang::caller_arg(object),
                           call = rlang::caller_env()) {
  check_sevt_ctables(object, arg = arg, call = call)
  check_sevt_prob(object, arg = arg, call = call)
}


#' @rdname check_sevt
#' @param object2 a staged event tree object.
#' @param arg1 passed arg1 name
#' @param arg2 passed arg2 name
#' @keywords internal
check_same_tree <- function(object, object2, arg1 = rlang::caller_arg(object),
                            arg2 = rlang::caller_arg(object2),
                            call = rlang::caller_env()) {
  if (!identical(object$tree, object2$tree)) {
    cli::cli_abort(c(
      "{.arg {arg1}} and {.arg {arg2}}
      must be defined over the same variables.",
      "x" = "You've supplied {.arg {arg1}}
      over {.val {sevt_varnames(x)}} and
      {.arg {arg2}} over {.var {sevt_varnames(y)}}."
    ), call = call, arg1 = arg1, arg2 = arg2)
  }
}

#' @rdname check_sevt
#' @param var name of a variable to be checked.
#' @keywords internal
check_var_in <- function(var, object, arg1 = rlang::caller_arg(var),
                         arg2 = rlang::caller_arg(object),
                         call = rlang::caller_env()) {
  if (!var %in% sevt_varnames(object)) {
    cli::cli_abort(c(
      "{.arg {arg1}} should be a variable in  {.arg {arg2}}.",
      "x" = "You've supplied {.arg {arg2}}
      over {.val {sevt_varnames(object)}} and {.arg {arg1}} = {.val {var}}
      is not among them."
    ), call = call, arg1 = arg1, arg2 = arg2)
  }
}

#' @rdname check_sevt
#' @param x scope, context or path to be checked against a model or tree
#' @keywords internal
check_scope <- function(x, object, arg1 = rlang::caller_arg(x),
                        arg2 = rlang::caller_arg(object),
                        call = rlang::caller_env()) {
  if (inherits(object, "sevt")) {
    vvv <- names(object$tree)
  } else if (inherits(object, "sevt.stgs")) {
    vvv <- names(attr(object, "tree"))
  } else {
    vvv <- names(object)
  }
  if (!all(x %in% vvv)) {
    cli::cli_abort(c(
      "{.arg {arg1}} should be a subset of the variables in  {.arg {arg2}}.",
      "x" = "You've supplied {.arg {arg2}}
      over {.val {vvv}} and
      {.val {x[!(x %in% vvv)]}}
      {?is/are} not among them."
    ), call = call, arg1 = arg1, arg2 = arg2)
  }
}

#' @rdname check_sevt
#' @param tree a list of levels specifying an event tree
#' @keywords internal
check_path <- function(x, tree) {
  n <- length(x)
  if (n > length(tree)) {
    cli::cli_abort(c(
      "Invalid path, excessive length."
    ))
  }
  if (!all(names(x) == names(tree)[1:n])) {
    # it is ok if not named
    cli::cli_abort(c(
      "Invalid path, wrong variables or order."
    ))
  }
  chk <- vapply(seq_along(x), function(i) x[[i]] %in% tree[[i]], FUN.VALUE = TRUE)
  if (!all(chk)) {
    cli::cli_abort(c(
      "Invalid path, wrong levels."
    ))
  }
}

#' @rdname check_sevt
#' @keywords internal
check_context <- function(x, var, tree) {
  n <- length(x)
  if (is.null(names(x))) {
    cli::cli_abort(c(
      "Invalid context, not named."
    ))
  }
  if (n > length(tree)) {
    cli::cli_abort(c(
      "Invalid context, excessive length."
    ))
  }
  if (!all(names(x) %in% names(tree))) {
    cli::cli_abort(c(
      "Invalid context, wrong variables."
    ))
  }
  ixvar <- which(names(tree) == var)
  ixmax <- max(which(names(tree) %in% names(x)))
  if (ixvar <= ixmax) {
    cli::cli_abort(c(
      "Invalid context, {var}
      preceedes some of the variables in the context."
    ))
  }
  chk <- vapply(names(x), function(i) x[[i]] %in% tree[[i]], FUN.VALUE = TRUE)
  if (!all(chk)) {
    cli::cli_abort(c(
      "Invalid context, wrong levels."
    ))
  }
}
