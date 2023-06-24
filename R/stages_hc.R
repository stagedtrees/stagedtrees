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
#' @return The final staged event tree obtained.
#'
#' @examples
#' start <- indep(PhDArticles[,1:5], join_unobserved = TRUE)
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
  if (is.null(scope)){
    scope <- sevt_varnames(object)[-1]
  }
  stopifnot(all(scope %in% sevt_varnames(object)[-1]))
  now_score <- score(object)
  for (v in scope) {
    done <- FALSE
    iter <- 0
    while (!done & iter < max_iter) {
      iter <- iter + 1
      temp <- object # clone the object
      temp_score <- now_score # clone the score
      stages <- object$stages[[v]]
      ustages <- unique(stages)
      newname <- new_label(c(ustages, ignore))
      ustages <- ustages[!(ustages %in% ignore)]
      done <- TRUE
      for (j in seq_along(ustages)) {
        s1 <- ustages[j]
        idx <- (seq_along(stages))[stages == s1]
        for (i in idx) {
          try <- object
          for (s2 in c(ustages[-j], newname)) {
            try$stages[[v]][i] <- s2
            try <- sevt_fit(try, scope = v)
            try_score <- score(try)
            if (try_score > temp_score) {
              temp <- try
              temp_score <- try_score
              ia <- i # just to message it if verbose
              s1a <- s1
              s2a <- s2
              done <- FALSE
            }
          }
        }
      } ## end for over stages
      object <- temp
      now_score <- temp_score
      if ((trace > 1) && !done) {
        message(
          v, " moved ", ia, " from stage ", s1a, " to stage ",
          s2a
        )
      }
    } ## end while
    if (trace > 0) {
      message(v, " HC done")
    }
  } ## end for over variables
  if (trace > 0) {
    message(
      "HC over ",
      v,
      " done after ",
      iter,
      " iterations."
    )
  }
  object$call <- sys.call()
  return(object)
}
