#' Backward hill-climbing
#'
#' Greedy search of staged event trees with
#' iterative joining of stages.
#'
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param score the score function to be maximized.
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
#' The candidate merge is selected by log-likelihood alone, which is done in
#' compiled code, and \code{score} is then evaluated once on that candidate to
#' decide whether to accept it. This is possible because every pairwise merge
#' changes the degrees of freedom by the same amount, so candidates differ only
#' in their log-likelihood and any score that prefers a higher log-likelihood at
#' equal complexity ranks them identically. Scores outside that class -- ones
#' that inspect the stage structure itself rather than the fitted
#' log-likelihood and dimension -- may select a different model than an
#' exhaustive search over the score would.
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(p = 4, n = 100)
#' model <- stages_bhc(full(DD), trace = 2)
#' summary(model)
#'
#' ## any score function can be used, at no cost to speed
#' model2 <- stages_bhc(full(DD), score = function(x) -AIC(x))
#' @importFrom stats  BIC
#' @export
stages_bhc <-
  function(object,
           score = function(x) {
             return(-BIC(x))
           },
           max_iter = Inf,
           scope = NULL,
           ignore = object$name_unobserved,
           trace = 0) {
    check_sevt_fit(object)
    now_score <- score(object)
    if (is.null(scope)) {
      scope <- sevt_varnames(object)[-1]
    }
    check_scope(scope, object)
    for (v in scope) {
      iter <- 0
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        done <- TRUE
        stages <- unique(object$stages[[v]])
        stages <- stages[!(stages %in% ignore)]
        if (length(stages) > 1) {
          k <- length(object$tree[[v]])
          lambda <- object$lambda
          if (is.null(lambda)) lambda <- 0
          pv <- object$prob[[v]][stages]
          pm <- do.call(rbind, lapply(pv, as.numeric))
          nv <- vapply(pv, function(p) {
            n <- attr(p, "n")
            if (is.null(n)) NA_real_ else as.numeric(n)
          }, FUN.VALUE = 1.0)
          ## pick the merge by log-likelihood (compiled, score-independent)
          bm <- best_merge_cpp(pm, nv, lambda, k)
          s1a <- stages[bm[1]]
          s2a <- stages[bm[2]]
          ## and let the score decide whether to take it
          try <- join_stages_unsafe(object, v, s1a, s2a)
          try_score <- score(try)
          if (try_score >= now_score) {
            object <- try
            now_score <- try_score
            done <- FALSE
          }
        } ## end if there are more than 1 stage
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
    object$score <- list(value = now_score, f = score)
    return(object)
  }
