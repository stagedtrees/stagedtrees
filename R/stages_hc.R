#' Hill-climbing
#'
#' Greedy search of staged event trees with
#' iterative moving of nodes between stages.
#'
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param score the score function to be maximized.
#' @param max_iter the maximum number of iterations per variable.
#' @param scope names of variables that should be considered for the optimization
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{message}).
#'
#' @details For each variable node-moves that best increases the
#' score are performed until no increase is possible.
#' A node-move is either changing the stage
#' associate to a node or move the node to a new stage.
#'
#' The `ignore` argument can be used to specify stages that should not
#' be affected during the search, that is left untouched.
#' This is useful for preserving structural zeroes and to speed-up
#' computations.
#'
#' Candidate moves are ranked by log-likelihood alone, which is done in
#' compiled code, and `score` is then evaluated only on the best candidate of
#' each kind of move. A move can dissolve its source stage, leave both stages
#' populated, or create a new stage, changing the degrees of freedom by
#' `-(k-1)`, `0` or `+(k-1)` respectively, where `k` is the number of levels
#' of the variable; candidates are therefore grouped by that change, within
#' which they differ only in log-likelihood. Any score that prefers a higher
#' log-likelihood at equal complexity ranks them identically, so at most three
#' score evaluations per step suffice. Scores outside that class -- ones that
#' inspect the stage structure itself rather than the fitted log-likelihood
#' and dimension -- may select a different model than an exhaustive search
#' over the score would.
#'
#' @return The final staged event tree obtained.
#'
#' @examples
#' start <- indep(PhDArticles[, 1:5], join_unobserved = TRUE)
#' model <- stages_hc(start)
#' @export
stages_hc <- function(object,
                      score = function(x) {
                        return(-BIC(x))
                      },
                      max_iter = Inf,
                      scope = NULL,
                      ignore = object$name_unobserved,
                      trace = 0) {
  check_sevt_fit(object)
  if (is.null(scope)) {
    scope <- sevt_varnames(object)[-1]
  }
  check_scope(scope, object)
  now_score <- score(object)
  for (v in scope) {
    done <- FALSE
    iter <- 0
    lambda <- object$lambda
    if (is.null(lambda)) lambda <- 0
    while (!done & iter < max_iter) {
      iter <- iter + 1
      done <- TRUE
      stages <- object$stages[[v]]
      ustages <- unique(stages)
      newname <- new_label(c(ustages, ignore))
      ustages <- ustages[!(ustages %in% ignore)]
      if (length(ustages) < 1) break
      ## Candidates are ranked by log-likelihood in compiled code, but grouped
      ## by their change in degrees of freedom first: a move to a new stage
      ## always fits better, so log-likelihood alone would always pick one.
      ## Within a group the degrees of freedom are fixed, so the score cannot
      ## reorder candidates and needs evaluating only on the group's best.
      ct <- as.matrix(object$ctables[[v]])
      storage.mode(ct) <- "double"
      asg <- match(stages, ustages) - 1L
      asg[is.na(asg)] <- -1L
      cand <- best_move_cpp(ct, as.integer(asg), length(ustages), lambda)
      ## every representative is scored against the UNMODIFIED object and only
      ## the best is applied; the candidate indices refer to the current stage
      ## structure and are stale the moment a move is taken
      temp <- NULL
      temp_score <- now_score
      for (r in seq_len(nrow(cand))) {
        i <- cand[r, 1]
        s2 <- if (cand[r, 2] == 0) newname else ustages[cand[r, 2]]
        try <- object
        try$stages[[v]][i] <- s2
        try <- sevt_fit(try, scope = v)
        try_score <- score(try)
        if (try_score > temp_score) {
          temp <- try
          temp_score <- try_score
          ia <- i
          s1a <- stages[i]
          s2a <- s2
        }
      }
      if (!is.null(temp)) {
        object <- temp
        now_score <- temp_score
        done <- FALSE
      }
      if ((trace > 1) && !done) {
        cli::cli_text("{v}: moved {ia} from stage {s1a} to stage {s2a}.")
      }
    } ## end while
    if (trace > 0) {
      cli::cli_text("HC over {v} done after {iter} iterations.")
    }
  } ## end for over variables
  if (trace > 0) {
    cli::cli_text("HC done")
  }
  object <- record_call(object, match.call())
  return(object)
}
