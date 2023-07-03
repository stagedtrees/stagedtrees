#' Coerce to sevt
#'
#' Convert to an equivalent object of class \code{\link{sevt}}.
#'
#' @param x   an R object.
#' @param ... additional parameters to be used by specific methods.
#' @return the equivalent object of class \code{\link{sevt}}.
#' @export
as_sevt <- function(x, ...) {
  UseMethod("as_sevt", x)
}

#' @rdname as_sevt
#' @param order order of the variables.
#' @details In \code{as_sevt.bn.fit} the \code{order}
#' argument, if provided, must be a topological order of the
#' \code{bn.fit} object (no check is performed). If the order is not provided
#' a topological order will be used (the one returned by
#' \code{bnlearn::node.ordering}).
#' @export
as_sevt.bn.fit <- function(x, order = NULL, ...) {
  as_sevt.parentslist(as_parentslist.bn.fit(x, order = order))
}


#' @rdname as_sevt
#' @export
as_sevt.bn <- function(x, order = NULL, values = NULL, ...) {
  as_sevt.parentslist(as_parentslist(x, order = order), values = values, ...)
}


#' @rdname as_sevt
#' @param values the values for each variable, the sample space.
#' @details In \code{as_sevt.parentslist} the \code{order}
#' argument, if provided, must be a topological order of the
#' corresponding DAG (no check is performed).
#' If the order is not provided
#' \code{names(x)} is used.
#'
#' The \code{values} parameter is used to specify the sample space
#' of each variable. For a \code{parentslist} object created with
#' \code{\link{as_parentslist}} from an object of class \code{sevt},
#' it is, usually, not needed to specify the \code{values} parameter,
#' since the sample space is saved in the \code{parentslist} object.
#' @examples
#' model <- stages_hclust(full(Titanic), k = 2)
#' plot(model)
#' pl <- as_parentslist(model)
#' model2 <- as_sevt(pl)
#' plot(model2) ## this is a super-model of the first staged tree
#' ## we can check it with
#' inclusions_stages(model, model2)
#' @export
as_sevt.parentslist <- function(x, order = NULL, values = NULL, ...) {
  if (is.null(order)) {
    order <- names(x)
  }
  if (is.null(values)) {
    values <- lapply(x, function(vv) {
      if (is.null(vv$values)) {
        warning("Missing values for a variable, a binary variable is used", call. = FALSE)
        c(0, 1)
      } else {
        vv$values
      }
    })
  } else { ## combine values with info in the parentslist object
    values <- sapply(names(x), function(nn) {
      if (is.null(values[[nn]])) {
        if (is.null(x[[nn]]$values)) {
          warning("Missing values for a variable, a binary variable is used", call. = FALSE)
          c(0, 1)
        } else {
          x[[nn]]$values
        }
      } else {
        values[[nn]]
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
  }

  # reorder the list
  values <- values[order]
  # create staged tree from list
  object <- sevt(values, order = order)
  # extract parents
  parents <- lapply(x, function(n) {
    n$parents
  })
  # build stages info respecting conditional
  # independences depicted in the Bayesian network
  if (length(order) > 1) {
    for (i in 2:length(order)) {
      # initialize stages for ith variable
      stgs <- "1"
      # build stages by iteratively expanding stages along tree
      for (j in seq(i - 1)) {
        if (order[j] %in% parents[[i]]) {
          # if  jth variable is a parent of ith expand different
          # stages for each value
          stgs <- as.vector(sapply(stgs, function(x) paste0(x, values[[j]])))
        } else {
          # otherwise replicate the same stages, since ith does not depend on jth
          stgs <- as.vector(sapply(stgs,
                                   function(x) rep(x, length(values[[j]]))))
        }
      }
      object$stages[[order[i]]] <- stgs
    }
  }
  object <- stndnaming(object)
  object
}
