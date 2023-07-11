#' Set stage to path
#'
#' @param object an object of class \code{sevt}.
#' @param path Vector of the path.
#' @param stage stage to be assigned.
#' @keywords internal
set_stage <- function(object, path, stage) {
  stage <- as.character(stage)
  warning("NOT YET IMPLEMENTED")
  ## TODO
  return(object)
}

#' Join stages
#'
#' Join two stages in a staged event tree object, updating
#' probabilities and log-likelihood accordingly.
#'
#' @param object an object of class \code{sevt}.
#' @param var variable.
#' @param s1 first stage.
#' @param s2 second stage.
#' @return the staged event tree where \code{s1} and \code{s2} are joined.
#' @details This function joins two stages associated to the
#'          same variable,
#'          updating probabilities and log-likelihood if
#'          the object was fitted.
#' @examples
#' model <- full(PhDArticles, lambda = 0)
#' model <- stages_fbhc(model)
#' model$stages$Kids
#' model <- join_stages(model, "Kids", "5", "6")
#' model$stages$Kids
#' @export
join_stages <- function(object, var, s1, s2) {
  check_sevt(object)
  check_var_in(var, object)
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  st <- stages(object, var)
  not_in <- c(s1, s2)[!(c(s1, s2) %in% st)]
  if (length(not_in) > 0) {
    cli::cli_abort(c(
      "{.arg s1} and {.arg s2} must be stages of {.arg var}
      in {.arg object}.",
      "x" = "You've supplied {.value {not_in}} which {?is/are} not
             {?a/} stage{?s} for variable {var} in {.arg object}."
    ))
  }
  if (s1 == s2) {
    cli::cli_abort(c(
      "{.arg s1} and {.arg s2} must be distinct stages in {.arg object}.",
      "x" = "You've supplied {.arg s1} and {.arg s2}
             both equal to {.value {s1}}."
    ))
  }
  k <- length(object$tree[[var]])
  object$stages[[var]][st == s2] <- s1
  if (has_prob(object)) {
    p1 <- object$prob[[var]][[s1]]
    p2 <- object$prob[[var]][[s2]]
    n2 <- attr(p2, "n")
    n1 <- attr(p1, "n")
    if (is.null(n1) || is.na(n1)) n1 <- 1
    if (is.null(n2) || is.na(n2)) n2 <- 1
    if (is.null(object$lambda)) {
      object$lambda <- 0
    }
    ct1 <-
      ifelse(is.na(p1), 0, p1) * (n1 + object$lambda * k) - object$lambda
    ct2 <-
      ifelse(is.na(p2), 0, p2) * (n2 + object$lambda * k) - object$lambda
    dll <-
      sum(ct2[ct2 > 0] * log(p2[ct2 > 0])) +
      sum(ct1[ct1 > 0] * log(p1[ct1 > 0]))
    object$prob[[var]][[s1]] <- ct2 + ct1 + object$lambda
    attr(object$prob[[var]][[s1]], "n") <- n1 + n2
    object$prob[[var]][[s1]] <-
      object$prob[[var]][[s1]] / sum(object$prob[[var]][[s1]])
    object$prob[[var]][[s2]] <- NULL ## delete one of the two
    if (!is.null(object$ll)) {
      ## update log likelihood
      ct1 <- ct1 + ct2
      object$ll <-
        object$ll - dll + sum(ct1[ct1 > 0] *
          log(object$prob[[var]][[s1]][ct1 > 0]))
      attr(object$ll, "df") <-
        attr(object$ll, "df") - length(object$prob[[var]][[s1]]) + 1
    }
  }
  return(object)
}

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
      object <- sevt_fit(object, lambda = object$lambda)
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
#' @details No internal checks are performed and as side effect
#' stages can be joined, if e.g. \code{new} is equal to the name
#' of a stage for variable \code{var}.
#'
#' @return a staged event tree object where stages \code{stage}
#' have been renamed to \code{new}.
#' @export
rename_stage <- function(object, var, stage, new) {
  check_sevt(object)
  check_var_in(var, object)
  if (!stage %in% stages(object, var)) {
    cli::cli_abort(c(
      "{.arg stage} must be a valid stage for variable {.arg var} in
      {.arg object}.",
      "x" = "You've supplied {.value {stage}} which is not a stage of
      variable {.value {var}} in {.arg object}."
    ))
  }
  # set new label
  object$stages[[var]][object$stages[[var]] %in% stage] <- new
  # if staged tree has prob move it to the new-label
  if (has_prob(object)) {
    object$prob[[var]][[new]] <- object$prob[[var]][[stage]]
    object$prob[[var]][[stage]] <- NULL
  }
  return(object)
}

#' Stages of a variable
#'
#' Obtain the stages of a given variable in a staged event tree object.
#' @param object an object of class \code{sevt}.
#' @param var name of one variable in \code{object}.
#' @return If \code{var} is specified, it returns a character vector with
#'         stage names for the given variable
#'         (that is \code{object$stages[[var]]}.
#'         Otherwise, If \code{var} is not specified, \code{stages}
#'         returns a list of character vectors containing the stages associated
#'         to each variable in the model (that is \code{object$stages}).
#' @export
stages <- function(object, var = NULL) {
  check_sevt(object)
  if (is.null(var)) {
    object$stages
  } else {
    check_var_in(var, object)
    object$stages[[var]]
  }
}
