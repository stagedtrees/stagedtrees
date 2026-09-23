#' Propensity-score stratification
#'
#' Rebuild a staged event tree so that the staging of an outcome variable
#' is inherited from the propensity-score stratification already induced
#' by a treatment variable.
#'
#' @param object a fitted object of class \code{sevt}.
#' @param treatment the treatment variable. Defaults to the variable
#'                   preceding \code{outcome}, or to the second-to-last
#'                   variable of \code{object} if \code{outcome} is not
#'                   given either.
#' @param outcome the outcome variable. It must be the variable
#'                 immediately following \code{treatment} in the order
#'                 of \code{object}. Defaults to the variable following
#'                 \code{treatment}, or to the last variable of
#'                 \code{object} if \code{treatment} is not given either.
#' @param ignore name of the stages of \code{outcome} which are left as
#'                they are, by default the stage of the situations with no
#'                observations. Note that these are stages of
#'                \code{outcome}, the variable this function re-stages,
#'                and not of \code{treatment}. Use \code{ignore = NULL} to
#'                re-stage every situation of \code{outcome}.
#' @return an object of class \code{sevt}, equal to \code{object} except
#'         for the staging (and, consequently, the fitted probabilities)
#'         of \code{outcome}.
#' @details
#' The staging of \code{treatment} already groups together the histories
#' that share the same probability of receiving each value of the
#' treatment, i.e. it is a propensity-score stratification of the
#' population. This function transfers that stratification to
#' \code{outcome} by giving each of its situations a new stage label
#' built from the pair (propensity-score stage of its \code{treatment}
#' history, value taken by \code{treatment}). Two situations of
#' \code{outcome} therefore share a stage exactly when they share both a
#' treatment stage and a treatment value.
#'
#' Situations of \code{outcome} whose stage is listed in \code{ignore},
#' by default the unobserved ones pooled by
#' \code{\link{join_unobserved}}, keep their stage. Such a situation has
#' no observations of its own, so re-staging it would hand it the
#' distribution of the stratum it lands in, estimated from the other
#' contexts in that stratum. That is an extrapolation, and it is exactly
#' what a stratification is for, but it is not something the data shows
#' for that context; leaving these situations alone keeps their outcome
#' distribution undefined, and \code{ignore = NULL} asks for the
#' extrapolation instead. Which contexts these are, and how much of the
#' population they account for, is reported by \code{\link{positivity}}.
#'
#' The new staging is assigned through the replacement method for
#' \code{\link{stages}}, which detects that \code{object} is already
#' fitted and automatically refits \code{outcome} using the data and
#' \code{lambda} cached in \code{object}; no separate call to
#' \code{\link{sevt_fit}} is needed.
#'
#' Averaging the potential outcomes of the returned object over the
#' resulting strata, weighted by stratum size, is the staged-tree
#' analogue of propensity-score stratification for estimating an average
#' treatment effect. See \code{\link{potential_outcomes}} for the
#' complementary construction, based on standardization over the
#' treatment instead.
#'
#' @references
#' Varando G., Leonelli M., Cerdà-Bautista J., Sitokonstantinou V., Camps-Valls G.
#' _Staged Event Trees for Transparent Treatment Effect Estimation_, 2025,
#' <https://arxiv.org/abs/2509.26265>
#'
#' @examples
#' model <- stages_bhc(full(Titanic, order = c("Class", "Sex", "Age", "Survived")))
#' model_ps <- ps_stratify(model, treatment = "Age", outcome = "Survived")
#'
#' # equivalent, since treatment/outcome default to the last two variables
#' model_ps <- ps_stratify(model)
#' stages(model_ps)[["Survived"]]
#' @seealso \code{\link{potential_outcomes}}, \code{\link{positivity}},
#'          \code{\link{stages}}
#' @export
ps_stratify <- function(object, treatment = NULL, outcome = NULL,
                        ignore = object$name_unobserved) {
  check_sevt_fit(object)
  defaults <- default_treatment_outcome(treatment, outcome, object)
  treatment <- defaults$treatment
  outcome <- defaults$outcome
  check_scope(c(treatment, outcome), object)
  order <- sevt_varnames(object)
  it <- which(order == treatment)
  io <- which(order == outcome)
  if (io != it + 1) {
    cli::cli_abort(c(
      "{.arg outcome} must be the variable immediately following
      {.arg treatment} in the order of {.arg object}.",
      "x" = "{.val {treatment}} is at position {it} and {.val {outcome}}
             is at position {io} in {.val {order}}."
    ))
  }
  lv <- object$tree[[treatment]]
  # stages() is used instead of object$stages, since the latter is NULL
  # when `treatment` is the first variable in the order.
  stgs <- stages(object)
  st <- stgs[[treatment]]
  value <- paste(rep(st, each = length(lv)),
                 rep(lv, times = length(st)),
                 sep = ":")
  keep <- stgs[[outcome]] %in% ignore
  value[keep] <- stgs[[outcome]][keep]
  # stages<- detects that `object` is already fitted and refits `outcome`
  # on the spot, reusing the data and lambda cached in `object`.
  stages(object)[outcome] <- value
  ## the returned tree carries a staging built to estimate an effect, not
  ## one searched on the data, and the call is what says so
  object <- record_call(object, match.call())
  object
}
