#' Fast backward hill-climbing
#'
#' Greedy search of staged event trees with
#' iterative joining of stages.
#'
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param score the score function to be maximized.
#' @param max_iter the maximum number of iteration.
#' @param scope names of variables that should be considered for the optimization.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{message}).
#' @details For each variable the algorithm tries to join stages
#' and moves to the first model that increases the score. When no
#' increase is possible it moves to the next variable.
#'
#'
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(p = 5, n = 100)
#' model <- stages_fbhc(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @export
stages_fbhc <-
  function(object,
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
      iter <- 0
      r <- 1
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object # clone the object
        temp_score <- now_score # clone the score
        s1_select <- NULL
        s2_select <- NULL
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
                s1_select <- s1 # just to message it if verbose
                s2_select <- s2 # just to message it if verose
                done <- FALSE
                break
              }
            }
            if (!done) {
              break
            }
          }
        } ## end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if ((trace > 1) && !done) {
          cli::cli_text("{v}: joined stages {c(s1_select, s2_select)}.")
        }
      } ## end while
      if (trace > 0) {
        cli::cli_text("fast BHC over {v} done after {iter} iterations.")
      }
    } ## end for over variables
    if (trace > 0) {
      cli::cli_text("fast HC done")
    }
    object$call <- match.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }
