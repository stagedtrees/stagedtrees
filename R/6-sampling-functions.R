
#' Sample from a staged event tree
#' 
#' @param n number of observations
#' @param object the staged event tree object
#' @details TO WRITE
#' 
#' @return A data frame containing a sample of size \code{n} 
#' @examples 
#' 
#' #########
#' data("Titanic")
#' mod <- naive.sevt(full(Titanic, lambda = 1))
#' sample.sevt(mod, 10)
#' @export
sample.sevt <- function(object, n = 1){
  stopifnot(n > 0)
  stopifnot(is(object, "sevt"))
  stopifnot(is_fitted.sevt(object))
  p <- length(object$tree)
  vars <- names(object$tree)
  S <- array(dim = c(n, p), data = NA, 
             dimnames = list(NULL, vars))
  S[, vars[1]] <- sample(object$tree[[ vars[1] ]], replace = TRUE, size = n,
                  prob = object$prob[[ vars[1] ]][[ 1 ]])
  for (i in 2:p){
     for (j in 1:n){
       stage <- find_stage(object, S[j, 1 : (i - 1)])
       S[j, i] <- sample(object$tree[[ vars[i] ]], size = 1, 
                         prob = object$prob[[ vars[i] ]][[ stage ]]) 
     }
  }
  return(as.data.frame(S))
}