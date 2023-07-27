#' Context-specific Backward hill-climbing
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
#' @details For each variable the algorithm tries to join stages
#' , by adding context specific independences,
#' and moves to the best model that increases the score. When no
#' increase is possible it moves to the next variable.
#' @return The final staged event tree obtained.
#' @examples
#' model <- stages_csbhc(full(Titanic))
#' summary(model)
#' @importFrom stats  BIC
#' @export
stages_csbhc <- function(object,
                         score = function(x) {
                           return(-BIC(x$ll))
                         },
                         max_iter = Inf,
                         scope = NULL,
                         ignore = object$name_unobserved) {
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
        mats <- ci_matrices(object, v)
        ## try all matrices
        for (i in 1:length(mats)) {
          ## for each column
          if (nrow(mats[[i]]) > 1) {
            for (j in seq(ncol(mats[[i]]))) {
              ## join together stages in column j of mat i
              try <- join_all(object, v, c(mats[[i]][, j]), ignore = ignore)
              try_score <- score(try)
              if (try_score > temp_score) {
                temp <- try
                temp_score <- try_score
                done <- FALSE
              }
            }
          }
        }
      } ## end if there are more than 1 stage
      object <- temp
      now_score <- temp_score
    } ## end while
  } ## end for over variables
  object$score <- list(value = now_score, score = score)
  return(object)
}
