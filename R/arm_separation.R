#' Index the situations of a variable by treatment arm
#'
#' Split the situations of \code{outcome} into the value of
#' \code{treatment} they follow and the rest of their history.
#' @param object an object of class \code{sevt}.
#' @param treatment the treatment variable.
#' @param outcome a variable following \code{treatment}.
#' @return a list with \code{arm}, the index of the treatment value of
#'         each situation, \code{context}, an identifier of its history
#'         with the treatment removed, and \code{k}, the number of values
#'         of the treatment.
#' @details The situations of a variable are indexed with the variable
#'          just before it varying fastest, so the value of
#'          \code{treatment} changes every \code{stride} situations,
#'          where \code{stride} multiplies the number of values of the
#'          variables sitting between the two.
#' @keywords internal
arm_index <- function(object, treatment, outcome) {
  vars <- sevt_varnames(object)
  it <- which(vars == treatment)
  io <- which(vars == outcome)
  ls <- lengths(object$tree[seq_len(io - 1)])
  stride <- if (io > it + 1) prod(ls[(it + 1):(io - 1)]) else 1
  k <- ls[[it]]
  idx <- seq_len(prod(ls)) - 1
  digit <- (idx %/% stride) %% k
  list(arm = digit + 1, context = idx - digit * stride, k = k)
}

#' Situations which differ only in the treatment
#'
#' Find the contexts where the staging of an outcome variable does not
#' separate the treatment arms.
#'
#' @param object an object of class \code{sevt} with probabilities.
#' @param treatment the treatment variable.
#' @param outcome the outcome variable, which must follow
#'                 \code{treatment} in the order of \code{object}.
#' @param ignore name of the stages of \code{outcome} which are not
#'                reported, by default the stage of the situations with no
#'                observations. How many were left out is reported when
#'                the result is printed.
#' @return a data frame with one row per context and shared stage, giving
#'         the context, the values of \code{treatment} which share it, the
#'         stage itself, and the probability of the context. A data frame
#'         with no rows means the arms are separated everywhere.
#' @details
#' Two situations of \code{outcome} which differ only in the value taken
#' by \code{treatment} describe the same history under two different
#' treatments. Putting them in the same stage states that the outcome has
#' the same distribution either way, which is to say that the treatment
#' has no effect in that context, and no data can afterwards say
#' otherwise: the effect is zero there by construction rather than by
#' estimation.
#'
#' A structure search knows nothing of this, and will happily merge such
#' situations whenever their estimated probabilities are close. This is a
#' reasonable thing to do when the staging is read as a description of
#' the joint distribution, and an unreasonable one when it is used to
#' estimate an effect, so it is worth checking before estimating and
#' repairing with \code{\link{separate_arms}} if need be.
#'
#' The staging built by \code{\link{ps_stratify}} separates the arms by
#' construction, since it labels a situation by the pair (propensity
#' stage, treatment value).
#'
#' @examples
#' model <- stages_bhc(full(Titanic, lambda = 1))
#' ## does the staging of Survived state that Age has no effect anywhere?
#' arm_separation(model, treatment = "Age", outcome = "Survived")
#' @seealso \code{\link{separate_arms}}, \code{\link{positivity}},
#'          \code{\link{ps_stratify}}
#' @export
arm_separation <- function(object, treatment, outcome,
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
  ai <- arm_index(object, treatment, outcome)
  stgs <- stages(object)[[outcome]]

  ## the history of a situation, with the treatment left out
  grid <- rev(expand.grid(rev(object$tree[seq_len(io - 1)]),
                          stringsAsFactors = FALSE))
  ctx <- grid[ai$arm == 1, -it, drop = FALSE]
  rownames(ctx) <- NULL
  p_ctx <- if (ncol(ctx) > 0) prob(object, ctx, na0 = FALSE) else 1

  res <- lapply(seq_len(nrow(ctx)), function(i) {
    here <- which(ai$context == ai$context[ai$arm == 1][i])
    s <- stgs[here]
    tied <- unique(s[duplicated(s)])
    tied <- tied[!(tied %in% ignore)]
    if (length(tied) == 0) {
      return(NULL)
    }
    do.call(rbind, lapply(tied, function(tt) {
      rows <- ctx[i, , drop = FALSE]
      rownames(rows) <- NULL
      cbind(rows, stats::setNames(
        data.frame(paste(lv[ai$arm[here][s == tt]], collapse = ", "), tt,
                   p_ctx[[i]], row.names = NULL, stringsAsFactors = FALSE),
        c(treatment, "stage", "context_probability")
      ))
    }))
  })
  hidden <- vapply(seq_len(nrow(ctx)), function(i) {
    here <- which(ai$context == ai$context[ai$arm == 1][i])
    s <- stgs[here]
    sum(unique(s[duplicated(s)]) %in% ignore)
  }, 1L)

  out <- do.call(rbind, res)
  if (is.null(out)) {
    out <- cbind(ctx[0, , drop = FALSE], stats::setNames(
      data.frame(character(0), character(0), numeric(0)),
      c(treatment, "stage", "context_probability")
    ))
  }
  rownames(out) <- NULL
  attr(out, "n_ignored") <- sum(hidden)
  attr(out, "ignore") <- ignore
  class(out) <- c("sevt.armsep", "data.frame")
  out
}

#' @rdname arm_separation
#' @param x an object of class \code{sevt.armsep}.
#' @param ... additional arguments passed to \code{print.data.frame}.
#' @export
print.sevt.armsep <- function(x, ...) {
  n <- attr(x, "n_ignored")
  ig <- attr(x, "ignore")
  if (nrow(x) == 0) {
    cat("The treatment arms are separated in every context.\n")
  } else {
    print(as.data.frame(x), ...)
  }
  if (isTRUE(n > 0)) {
    cli::cli_alert_info(
      "{n} context{?s} sharing the stage {.val {ig}} {cli::qty(n)}{?is/are}
       not shown. Use {.code ignore = NULL} to include {cli::qty(n)}{?it/them}."
    )
  }
  invisible(x)
}

#' Separate the treatment arms of a staging
#'
#' Refine the staging of an outcome variable so that no stage holds two
#' situations which differ only in the value taken by the treatment.
#'
#' @param object an object of class \code{sevt} with probabilities.
#' @param treatment the treatment variable.
#' @param outcome the outcome variable, which must follow
#'                 \code{treatment} in the order of \code{object}.
#' @param ignore name of the stages of \code{outcome} which are left as
#'                they are, by default the stage of the situations with no
#'                observations.
#' @return an object of class \code{sevt}, equal to \code{object} except
#'         for the staging, and the fitted probabilities, of
#'         \code{outcome}.
#' @details
#' A stage holding two situations of one context, one per arm, is split by
#' the value of \code{treatment}; every other stage is left alone. Only
#' those stages state that the treatment has no effect somewhere, which
#' \code{\link{arm_separation}} explains, and a stage holding different
#' arms of different contexts says nothing of the sort, so splitting it
#' would spend observations for nothing.
#'
#' A staging which is already separated is therefore returned unchanged,
#' stage names included.
#'
#' The refinement is applied after the search, and so splits stages the
#' search chose to pool: the probabilities are then estimated on fewer
#' observations each. A search which never merges across arms in the first
#' place would spend its degrees of freedom better, at the cost of not
#' being the search the package implements.
#'
#' @examples
#' model <- stages_bhc(full(Titanic, lambda = 1))
#' arm_separation(model, treatment = "Age", outcome = "Survived")
#' model_sep <- separate_arms(model, treatment = "Age", outcome = "Survived")
#' arm_separation(model_sep, treatment = "Age", outcome = "Survived")
#' @seealso \code{\link{arm_separation}}, \code{\link{ps_stratify}}
#' @export
separate_arms <- function(object, treatment = NULL, outcome = NULL,
                          ignore = object$name_unobserved) {
  check_sevt_prob(object)
  defaults <- default_treatment_outcome(treatment, outcome, object)
  treatment <- defaults$treatment
  outcome <- defaults$outcome
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
  ai <- arm_index(object, treatment, outcome)
  stgs <- stages(object)[[outcome]]
  ## only the stages holding two arms of one context need splitting: a stage
  ## may hold different arms of different contexts without stating anything
  ## about an effect, and splitting it would cost observations for nothing
  tied <- unique(unlist(lapply(unique(ai$context), function(cc) {
    s <- stgs[ai$context == cc]
    unique(s[duplicated(s)])
  })))
  split <- stgs %in% setdiff(tied, ignore)
  value <- stgs
  value[split] <- paste(stgs[split], lv[ai$arm][split], sep = ":")
  stages(object)[outcome] <- value
  object <- record_call(object, match.call())
  object
}
