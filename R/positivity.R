#' Positivity violations
#'
#' Find the contexts where a treatment variable does not take all its
#' values with positive probability.
#'
#' @param object an object of class \code{sevt} with probabilities.
#' @param treatment the treatment variable.
#' @param outcome the outcome variable, which must follow \code{treatment}
#'                 in the order of \code{object}.
#' @return a data frame with one row per violation, giving the context,
#'         the value of \code{treatment} which is not attainable in it,
#'         and the probability the model assigns to it (\code{0}, or
#'         \code{NA} if the context has no observations). A data frame
#'         with no rows means positivity holds.
#' @details
#' Estimating the effect of \code{treatment} on \code{outcome} requires
#' that every context which can occur may receive every value of the
#' treatment, the positivity (or overlap) assumption. A context breaking
#' it carries no information on what the outcome would have been under
#' the values it never takes, so the corresponding potential outcome is
#' not identified.
#'
#' The contexts are the situations of \code{treatment}, i.e. the
#' combinations of the variables preceding it. Contexts which cannot
#' occur are not reported: they are outside the population, rather than a
#' part of it that the treatment never reaches.
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
positivity <- function(object, treatment, outcome) {
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
  ## a context which cannot occur is not part of the population
  reachable <- if (it > 1) {
    p <- prob(object, ctx, na0 = FALSE)
    is.na(p) | p > 0
  } else {
    TRUE
  }
  stgs <- stages(object)[[treatment]]

  res <- lapply(which(reachable), function(i) {
    p <- object$prob[[treatment]][[stgs[i]]][lv]
    bad <- is.na(p) | p == 0
    if (!any(bad)) {
      return(NULL)
    }
    cbind(ctx[i, , drop = FALSE],
          data.frame(treatment = lv[bad], probability = as.numeric(p[bad]),
                     row.names = NULL, stringsAsFactors = FALSE))
  })
  res <- do.call(rbind, res)
  if (is.null(res)) {
    res <- cbind(ctx[0, , drop = FALSE],
                 data.frame(treatment = character(0), probability = numeric(0)))
  }
  names(res)[names(res) == "treatment"] <- treatment
  rownames(res) <- NULL
  res
}
