#' Split randomly a stage
#'
#' Randomly assign some of the paths to a new stage.
#' @param object an object of class \code{sevt}.
#' @param var the variable name.
#' @param stage the name of the stage.
#' @param p probability to move a situation from the
#'          original stage into the new stage.
#'
#' @return an object of class \code{sevt}.
#' @details Splits randomly a given stage into two stages. More precisely,
#' it assigns each situation within the given stage into a new stage with
#' probability \code{p}.
#' @keywords internal
split_stage_random <- function(object, var, stage, p = 0.5) {
  check_sevt(object)
  check_var_in(var, object)
  # if the given stage is not present
  if (!(stage %in% object$stages[[var]])) {
    # return the same object
    return(object)
  }
  # obtain size of stages for given variable
  d <- length(object$stages[[var]])
  # get a new label
  label <- new_label(object$stages[[var]])
  # find where stage should be changed
  # changes should happen with probability p
  # and only for the given stage
  ix <-
    (object$stages[[var]] == stage) &
      sample(
        x = c(TRUE, FALSE),
        size = d,
        prob = c(p, 1 - p),
        replace = TRUE
      )
  # if there is a change to do
  if (any(ix)) {
    # set the new label
    object$stages[[var]][ix] <- label
    if (is_fitted_sevt(object)) {
      # re-fit the model
      object <- sevt_fit(object, scope = var)
    }
  }
  return(object)
}

#' Rename stage(s) in staged event tree
#'
#' Change the name of a stage in a staged event tree.
#' @param object an object of class \code{sevt}.
#' @param var name of a variable in \code{object}.
#' @param stage name of the stage to be renamed.
#' @param new new name for the stage.
#' @details Renames a stage without merging. If \code{new} already exists
#' as a stage for variable \code{var}, an error is raised because renaming
#' onto an existing stage would silently overwrite its probability vector
#' with incorrect values. To merge two stages use \code{\link{join_stages}}.
#'
#' @return a staged event tree object where stage \code{stage} has been
#' renamed to \code{new}.
#' @export
rename_stage <- function(object, var, stage, new) {
  check_sevt(object)
  check_var_in(var, object)
  if (!stage %in% object$stages[[var]]) {
    cli::cli_abort(c(
      "{.arg stage} must be a valid stage for variable {.arg var} in
      {.arg object}.",
      "x" = "You've supplied {.value {stage}} which is not a stage of
      variable {.value {var}} in {.arg object}."
    ))
  }
  if (new != stage && new %in% object$stages[[var]]) {
    cli::cli_abort(c(
      "Stage {.val {new}} already exists for variable {.val {var}}.",
      "i" = "Renaming onto an existing stage would silently overwrite its
      probability vector. Use {.fun stagedtrees::join_stages} to merge stages."
    ))
  }
  if (new == stage) return(object)
  # set new label
  object$stages[[var]][object$stages[[var]] %in% stage] <- new
  # if staged tree has prob move it to the new-label
  if (has_prob(object)) {
    object$prob[[var]][[new]] <- object$prob[[var]][[stage]]
    object$prob[[var]][[stage]] <- NULL
  }
  object$ll <- NULL
  return(object)
}
