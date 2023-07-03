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
#' is printed (via \code{message}).
#' @details For each variable the algorithm tries to join stages
#' and moves to the best model that increases the score. When no
#' increase is possible it moves to the next variable.
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(p = 4, n = 100)
#' model <- stages_bhc(full(DD), trace = 2)
#' summary(model)
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
      r <- 1
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
          for (i in 2:length(stages)) {
            ## try all stages pair
            s1 <- stages[i]
            for (j in 1:(i - 1)) {
              s2 <- stages[j]
              try <-
                join_stages(object, v, s1, s2) ## join the 2 stages
              try_score <- score(try)
              if (try_score >= temp_score) {
                temp <- try
                temp_score <- try_score
                s1a <- s1
                s2a <- s2
                done <- FALSE
              }
            }
          }
        } ## end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if ((trace > 1) && !done) {
          message(v, " joined stages: ", s1a, " and ", s2a)
        }
      } ## end while
      if (trace > 0) {
        message("BHC over ", v, " done after ", iter, " iterations")
      }
    } ## end for over variables
    if (trace > 0) {
      message("BHC done")
    }
    object$call <- match.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }
