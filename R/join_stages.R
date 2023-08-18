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
  st <- object$stages[[var]]
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
  join_stages_unsafe(object, var, s1, s2)
}


#' @rdname join_stages
#' @keywords internal
join_stages_unsafe <- function(object, var, s1, s2) {
  st <- object$stages[[var]]
  k <- length(object$tree[[var]])
  object$stages[[var]][st == s2] <- s1
  if (isFALSE(is.null(object$prob))) {
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
    if (isFALSE(is.null(object$ll))) {
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

#' @rdname join_stages
#' @param stages a vector of stage names for variable \code{var}.
#' @param ignore vector of stages which will be ignored and left untouched.
#' @keywords internal
join_all <- function(object, var, stages, ignore = NULL) {
  stages <- unique(stages)
  stages <- stages[!(stages %in% ignore)]
  if (length(stages) <= 1)
    return(object)
  for (i in seq_along(stages)[-1]) {
    object <- join_stages_unsafe(object, var, stages[1], stages[i])
  }
  return(object)
}
