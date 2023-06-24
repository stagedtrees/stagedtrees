#' Backward random hill-climbing
#'
#' Randomly try to join stages.
#' This is a pretty-useless function, used for comparisons.
#'
#' @param object an object of class \code{sevt}.
#' @param score the score function to be maximized.
#' @param max_iter the maximum number of iteration.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{message}).
#'
#' @details At each iteration a variable and
#' two of its stages are randomly selected.
#' If joining the stages increases the score, the model is
#' updated. The procedure is repeated until the
#' number of iterations reaches \code{max_iter}.
#'
#' @return an object of class \code{sevt}.
#' @export
#' @examples
#' DD <- generate_xor_dataset(p = 4, n = 100)
#' model <- stages_bhcr(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
stages_bhcr <-
  function(object,
           score = function(x) {
             return(-BIC(x))
           },
           max_iter = 100,
           trace = 0) {
    check_sevt_fit(object)
    now_score <- score(object)
    r <- 1
    iter <- 0
    while (iter < max_iter) {
      ## chose randomly one of the variable and try to perform a stage-merging
      iter <- iter + 1
      v <- sample(names(object$tree)[-1], size = 1)
      if (length(unique(object$stages[[v]])) > 1) {
        stgs <-
          sample(unique(object$stages[[v]]),
            size = 2,
            replace = FALSE
          ) ## select randomly two stages
        try <-
          join_stages(object, v, stgs[1], stgs[2]) ## join the 2 stages
        try_score <- score(try)
        if (try_score >= now_score) {
          object <- try
          r <-
            abs((try_score - now_score) / now_score) ## compute relative score increase
          now_score <- try_score
          if (trace > 1) {
            message(paste(v, "joined stages: ", stgs[1], "and", stgs[2]))
          }
        }
      }
    }
    if (trace > 0) {
      message(paste("backward HC random done after", iter, "iteration"))
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }
