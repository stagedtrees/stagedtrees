#' Backward hill-climbing
#'
#' Greedy search of staged event trees with
#' iterative joining of stages.
#'
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param score the score to be maximized. Either a string naming one of the
#'              predefined scores, \code{"BIC"} (default) or \code{"AIC"},
#'              or a function taking a \code{sevt} object and returning a
#'              numeric value. See Details.
#' @param max_iter the maximum number of iterations per variable.
#' @param scope names of variables that should be considered for the optimization.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{cli::cli_text}).
#' @details For each variable the algorithm tries to join stages
#' and moves to the best model that increases the score. When no
#' increase is possible it moves to the next variable.
#'
#' When \code{score} is given as a string, candidate joins are scored from the
#' change they induce in log-likelihood and degrees of freedom, without
#' building the candidate model. This is equivalent to, and appreciably faster
#' than, evaluating the score on each candidate: on a model with 243
#' situations it is roughly seven times faster, and the saving grows with the
#' number of stages. Passing an equivalent function, for example
#' \code{function(x) -BIC(x)}, is still supported and yields the same result,
#' but requires fitting every candidate and is therefore slower.
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(p = 4, n = 100)
#' model <- stages_bhc(full(DD), trace = 2)
#' summary(model)
#'
#' ## a score can also be given as a function, at the cost of speed
#' model2 <- stages_bhc(full(DD), score = function(x) -BIC(x))
#' @importFrom stats  BIC
#' @export
stages_bhc <-
  function(object,
           score = "BIC",
           max_iter = Inf,
           scope = NULL,
           ignore = object$name_unobserved,
           trace = 0) {
    check_sevt_fit(object)
    sc <- resolve_score(score)
    score_fun <- if (is.null(sc)) score else sc$full
    now_score <- score_fun(object)
    if (is.null(scope)) {
      scope <- sevt_varnames(object)[-1]
    }
    check_scope(scope, object)
    nobs <- attr(object$ll, "nobs")
    for (v in scope) {
      iter <- 0
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object # clone the object
        temp_score <- now_score
        done <- TRUE
        stages <- unique(object$stages[[v]])
        stages <- stages[!(stages %in% ignore)]
        if (length(stages) > 1) {
          if (is.null(sc)) {
            ## general score: must be evaluated on a complete object
            for (i in 2:length(stages)) {
              ## try all stages pair
              s1 <- stages[i]
              for (j in 1:(i - 1)) {
                s2 <- stages[j]
                try <-
                  join_stages_unsafe(object, v, s1, s2) ## join the 2 stages
                try_score <- score_fun(try)
                if (try_score >= temp_score) {
                  temp <- try
                  temp_score <- try_score
                  s1a <- s1
                  s2a <- s2
                  done <- FALSE
                }
              }
            }
          } else {
            ## predefined score: evaluate the move from its effect on
            ## log-likelihood and degrees of freedom, and join only the winner
            pv <- object$prob[[v]]
            k <- length(object$tree[[v]])
            lambda <- object$lambda
            if (is.null(lambda)) lambda <- 0
            ddf <- -(k - 1)
            best <- 0
            for (i in 2:length(stages)) {
              s1 <- stages[i]
              for (j in 1:(i - 1)) {
                s2 <- stages[j]
                dscore <- sc$delta(
                  join_ll_delta(pv[[s1]], pv[[s2]], lambda, k), ddf, nobs
                )
                if (dscore >= best) {
                  best <- dscore
                  s1a <- s1
                  s2a <- s2
                  done <- FALSE
                }
              }
            }
            if (!done) {
              temp <- join_stages_unsafe(object, v, s1a, s2a)
              temp_score <- now_score + best
            }
          }
        } ## end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if ((trace > 1) && !done) {
          cli::cli_text("{v}: joined stages: {s1a} and {s2a}")
        }
      } ## end while
      if (trace > 0) {
        cli::cli_text("BHC over {v} done after {iter} iterations")
      }
    } ## end for over variables
    if (trace > 0) {
      cli::cli_text("BHC done")
    }
    object$call <- match.call()
    object$score <- list(value = now_score, f = score_fun)
    return(object)
  }
