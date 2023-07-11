
bls <- function(data, left, new, alg, search_criterion = BIC, lambda, join_unobserved, ...){
  m <- full(data = data, order = left,
            join_unobserved = join_unobserved,
            lambda = lambda)
  s <- search_criterion(m)
  m <- alg(sevt_add(m, new, data, join_unobserved = join_unobserved), scope = new, ...)
  return(search_criterion(m) - s)
}

#' Optimal Order Search
#'
#' Find the optimal staged event tree
#' with a dynamic programming approach.
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
#' @details This function is an implementation of the
#'          dynamic programming approach
#'          of Silander and Leong (2013).
#'          If the \code{search_criterion} is decomposable
#'          the returned model attains the best value
#'          among all possible orders.
#' @references
#' Silander T., Leong TY.
#' A Dynamic Programming Algorithm for Learning Chain Event Graphs.
#' In: Fürnkranz J., Hüllermeier E., Higuchi T. (eds)
#' Discovery Science. DS 2013. _Lecture Notes in Computer Science_,
#' vol 8140. Springer, Berlin, Heidelberg. 2013.
#'
#' Cowell R and Smith J.
#' Causal discovery through MAP selection of stratified chain event graphs.
#' _Electronic Journal of Statistics_, 8(1):965–997, 2014.
#' @examples
#' ## default search using BIC score
#' model <- search_best(Titanic, alg = stages_kmeans)
#'
#' ## use df as search_criterion
#' model1 <- search_best(Titanic, alg = stages_bhc,
#'                       search_criterion = function(m) attr(logLik(m), "df"))
#' @importFrom utils combn
#' @export
search_best <- function(data, alg = stages_bhc, search_criterion = BIC, lambda = 0,
                           join_unobserved = TRUE, ...){
  if (is.data.frame(data)){
    vs <- colnames(data)
  }else if (is.table(data)){
    vs <- names(dimnames(data))
  }else{
    cli::cli_abort(c(
      "{.arg data} must be a {.cls data.frame} or a {.cls table} object.",
      "x" = "You've supplied {.arg data} which is {.type {data}}."
    ))
  }
  ## initialize scores with 1 variables
  scores <- sapply(vs, FUN = function(vv){
    search_criterion(full(data, order = vv, join_unobserved = join_unobserved,
                      lambda = lambda))
  }, USE.NAMES = TRUE )
  sinks <- vs
  names(sinks) <- vs
  for (i in seq_along(vs)[-1]){
    sets <- combn(vs,i)
    tmp <- apply(sets, MARGIN = 2, FUN = function(W){
      sapply(W, function(v){
        scores[paste(W[W!=v],collapse="-")] + bls(data, W[W!=v], v, alg,
                                                  search_criterion,
                                                  lambda = lambda,
                                                  join_unobserved = join_unobserved,
                                                  ...)
      })
    })
    new_sinks <- sapply(seq(ncol(sets)), function(i) sets[which.min(tmp[,i]),i])
    nam <- apply(sets, MARGIN = 2, FUN = paste, collapse = "-")
    sinks[nam] <- new_sinks
    scores[nam] <- apply(tmp, 2, min)
  }
  left <- paste(vs, collapse = "-")
  order <- c()
  for (i in length(vs):1){
    order[i] <- sinks[left]
    left <- paste(vs[!(vs %in% order)], collapse = "-")
  }
  object <- alg(full(data, order = order, join_unobserved = join_unobserved,
                     lambda = lambda), ...)
  return(object)
}
