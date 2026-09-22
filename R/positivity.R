#' Positivity violations
#'
#' Find the contexts where a treatment variable does not take all its
#' values with positive probability.
#'
#' @param object an object of class \code{sevt} with probabilities.
#' @param treatment the treatment variable.
#' @param outcome the outcome variable, which must follow \code{treatment}
#'                 in the order of \code{object}.
#' @param ignore name of the stages of \code{treatment} whose contexts are
#'                left out of the table, by default the stage of the
#'                unobserved situations. How many were left out is
#'                reported when the result is printed.
#' @return a data frame with one row per offending context, giving the
#'         context, the values of \code{treatment} which are not
#'         attainable in it, and the probability of the context itself,
#'         that is the share of the population the violation concerns. A
#'         data frame with no rows means positivity holds, and a
#'         \code{context_probability} of zero marks a context which does
#'         not occur, see the details.
#' @details
#' Estimating the effect of \code{treatment} on \code{outcome} requires
#' that every context which can occur may receive every value of the
#' treatment, the positivity (or overlap) assumption. A context breaking
#' it carries no information on what the outcome would have been under
#' the values it never takes, so the corresponding potential outcome is
#' not identified.
#'
#' The contexts are the situations of \code{treatment}, i.e. the
#' combinations of the variables preceding it. A value the model gives no
#' probability at all, because the context has no observations, counts as
#' unattainable just as an explicit zero does.
#'
#' The probability of the context separates two cases which are worth
#' reading differently. A positive one is a strict violation: a part of
#' the population which occurs, and never receives that value of the
#' treatment. A zero one is a context which does not occur at all, so it
#' weighs nothing in an average treatment effect and breaks no assumption
#' about the population; but the model has no support there either, and
#' whatever a staging or a prior later says about it is extrapolation
#' rather than evidence. Such a context makes no value of the treatment
#' attainable, and so lists all of them. These are the contexts
#' \code{ignore} leaves out by default, as the situations with no
#' observations are the ones pooled into the unobserved stage; pass
#' \code{ignore = NULL} to see them.
#'
#' The assumption is checked on the probabilities of \code{object}, so
#' which object it is given matters. On a model fitted with
#' \code{lambda = 0} and no staging over \code{treatment}, as returned by
#' \code{\link{full}}, it reports the contexts where a treatment value was
#' never observed. Staging \code{treatment} and smoothing with
#' \code{lambda > 0} both give positive probability to values that were
#' never observed in a context, the former borrowing it from the other
#' contexts in the same stage and the latter from the prior; either will
#' repair a violation that the data does not support, so it is worth
#' checking positivity before the staging is searched.
#'
#' @examples
#' ## no crew member was a child, in either sex
#' positivity(full(Titanic, lambda = 0), treatment = "Age", outcome = "Survived")
#'
#' ## staging can make a violation disappear, by borrowing from the
#' ## other contexts of the same stage
#' positivity(stages_bhc(full(Titanic, lambda = 0)),
#'            treatment = "Age", outcome = "Survived")
#' @seealso \code{\link{potential_outcomes}}, \code{\link{ps_stratify}}
#' @export
positivity <- function(object, treatment, outcome,
                       ignore = object$name_unobserved) {
  check_sevt_prob(object)
  check_scope(c(treatment, outcome), object)
  vars <- sevt_varnames(object)
  it <- which(vars == treatment)
  io <- which(vars == outcome)
  if (io <= it) {
    cli::cli_abort(c(
      "{.arg outcome} must follow {.arg treatment} in the order
      of {.arg object}.",
      "x" = "{.val {treatment}} is at position {it} and {.val {outcome}}
             is at position {io} in {.val {vars}}."
    ))
  }
  lv <- object$tree[[treatment]]
  ## the contexts are the situations of treatment, ordered as its stages
  ## are: the variable just before it varies fastest
  ctx <- if (it > 1) {
    rev(expand.grid(rev(object$tree[seq_len(it - 1)]), stringsAsFactors = FALSE))
  } else {
    data.frame(row.names = 1L)
  }
  p_ctx <- if (it > 1) prob(object, ctx, na0 = FALSE) else 1
  stgs <- stages(object)[[treatment]]

  viol <- lapply(seq_len(nrow(ctx)), function(i) {
    p <- object$prob[[treatment]][[stgs[i]]][lv]
    bad <- is.na(p) | p == 0
    if (!any(bad)) {
      return(NULL)
    }
    rows <- ctx[i, , drop = FALSE]
    rownames(rows) <- NULL
    cbind(rows,
          data.frame(treatment = paste(lv[bad], collapse = ", "),
                     context_probability = p_ctx[[i]],
                     row.names = NULL, stringsAsFactors = FALSE))
  })
  found <- !vapply(viol, is.null, TRUE)
  hidden <- found & (stgs %in% ignore)
  res <- do.call(rbind, viol[found & !hidden])
  if (is.null(res)) {
    res <- cbind(ctx[0, , drop = FALSE],
                 data.frame(treatment = character(0),
                            context_probability = numeric(0)))
  }
  names(res)[names(res) == "treatment"] <- treatment
  rownames(res) <- NULL
  attr(res, "n_ignored") <- sum(hidden)
  attr(res, "ignore") <- ignore
  class(res) <- c("sevt.positivity", "data.frame")
  res
}

#' @rdname positivity
#' @param x an object of class \code{sevt.positivity}, as returned by
#'          \code{positivity}.
#' @param ... additional arguments passed to \code{print.data.frame}.
#' @export
print.sevt.positivity <- function(x, ...) {
  n <- attr(x, "n_ignored")
  ig <- attr(x, "ignore")
  if (nrow(x) == 0) {
    cat("No positivity violations.\n")
  } else {
    print(as.data.frame(x), ...)
  }
  if (isTRUE(n > 0)) {
    cli::cli_alert_info(
      "{n} context{?s} whose stage is {.val {ig}} {cli::qty(n)}{?is/are}
       not shown. Use {.code ignore = NULL} to include
       {cli::qty(n)}{?it/them}."
    )
  }
  invisible(x)
}
