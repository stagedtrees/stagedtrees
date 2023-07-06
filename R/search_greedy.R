#' Greedy Order Search
#'
#' Search the optimal staged event tree
#' with a greedy heuristic.
#' @param data either a data.frame or a table containing the data.
#' @param alg a function that performs stages structure estimation. Similar to
#'            \code{\link{stages_bhc}} or \code{\link{stages_hclust}}.
#'            The function \code{alg} must accept the argument
#'            \code{scope}.
#' @param search_criterion the criterion minimized in the order search.
#' @param lambda numerical value passed to \code{\link{full}}.
#' @param join_unobserved logical, passed to \code{\link{full}}.
#' @param ... additional arguments, passed to \code{alg}.
#' @return The estimated staged event tree model.
#' @details The greedy approach implemented in this function
#'          iteratively adds variables to the staged tree that
#'          better improve the \code{search_criterion}.
#' @examples
#' model <- search_greedy(Titanic, alg = stages_fbhc)
#' print(model)
#' @export
search_greedy <- function(data, alg = stages_bhc, search_criterion = BIC, lambda = 0,
                          join_unobserved = TRUE, ...){
  if (is.data.frame(data)){
    vs <- colnames(data)
  }else if (is.table(data)){
    vs <- names(dimnames(data))
  }else{
    cli::cli_abort(c(
      "{.arg data} must be a data.frame or a table object.",
      "x" = "You've supplied {.arg data} which is {.type {data}}."
    ))
  }
  ## initialize best
  best <- full(data, order = vs[1], lambda = lambda, join_unobserved = join_unobserved)
  ## check all other possible first variable
  if (length(vs) < 2) return(best)
  for (v in vs){
    tmp <- full(data, order = v, lambda = lambda, join_unobserved = join_unobserved)
    #print(score(tmp))
    if (search_criterion(tmp) < search_criterion(best)){
      best <- tmp
    }
  }
  object <- best
  ## add the best one by one
  svs <- vs[!(vs %in% names(object$tree))]
  for (i in seq_along(vs)[-1]){
    #done <- FALSE
    best <- alg(sevt_add(object, svs[1], data, join_unobserved = join_unobserved),
                scope = svs[1], ...)
    for (v in svs[-1]){
      tmp <- alg(sevt_add(object, v, data, join_unobserved = join_unobserved),
                 scope = v, ...)
      if (search_criterion(tmp) < search_criterion(best)){
        best <- tmp
        #done <- TRUE
      }
    }
    #if (!done) break
    object <- best
    svs <- vs[!(vs %in% names(object$tree))]
  }
  return(object)
}
