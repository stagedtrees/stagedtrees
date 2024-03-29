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
  vars <- names(object$tree)
  for (v in scope) {
    r <- 1
    iter <- 0
    done <- FALSE
    while (!done && iter < max_iter) {
      iter <- iter + 1
      temp <- object # clone the object
      temp_score <- now_score
      done <- TRUE
      sss <- object$stages[[v]]
      stages <- unique(sss)
      stages <- stages[!(stages %in% ignore)]
      if (length(stages) > 1) {
        ## mats <- ci_matrices(object, v)
        ## try all matrices
        ix <- which(vars == v)
        for (i in rev(seq_len(ix - 1))) {
          mat <- matrix(sss,
                        nrow = length(object$tree[[vars[i]]]))
          if (sum(duplicated(mat)) == (nrow(mat) - 1)) {
            mat <- mat[1, , drop =  FALSE]
          }
          ## for each column
          if (nrow(mat) > 1) {
            ix <- !duplicated(t(mat))
            mat2 <- mat[, ix, drop = FALSE]
            for (j in seq_len(ncol(mat2))) {
              ## join together stages in column j of mat i
              if (length(unique(c(mat2[,j]))) > 1){
                try <- join_all(object, v, c(mat2[, j]), ignore = ignore)
                try_score <- score(try)
                if (try_score > temp_score) {
                  temp <- try
                  temp_score <- try_score
                  done <- FALSE
                }
              }
            }
          }
          sss <- c(t(mat))
        }
      } ## end if there are more than 1 stage
      object <- temp
      now_score <- temp_score
    } ## end while
  } ## end for over variables
  object$score <- list(value = now_score, score = score)
  return(object)
}
