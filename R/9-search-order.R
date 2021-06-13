#' Add a variable to a staged event tree
#' 
#' Return an updated staged event tree with one additional 
#' variable at the end of the tree. 
#' @param object an object of class \code{sevt}.
#' @param var character, the name of the new variable to be added.
#' @param data either a \code{data.frame} or a \code{table} containing 
#'             the data from the variables in \code{object} plus \code{var}.
#' @param join_unobserved logical, passed to \code{\link{full}}.
#' @details This function update a staged event tree object with
#'          an additional variable. The stages structure of the 
#'          new variable is initialized as in the saturated model.
#' @return An object of class \code{sevt} representing a 
#' staged event tree model with \code{var} added as last variable.
#' @examples 
#' model <- full(Titanic, order = c("Age", "Class"))
#' print(model)
#' model <- sevt_add(model, "Survived", Titanic)
#' print(model)
#' @export
sevt_add <- function(object, var, data, join_unobserved = TRUE){
  if (is.data.frame(data)) {
    data <- table(data[, c(names(object$tree), var)])
  }
  if (!is.table(data)){
    stop("Invalid data argument. Data must be a data.frame or a table obejct.")
  }
  path <- names(object$tree)
  tt <- apply(data, MARGIN = c(path, var), sum)
  ctable <- ftable(tt, col.vars = var, row.vars = path)
  tmp <- sevt(data, full = TRUE, order = c(path, var))
  object$tree <- tmp$tree
  object$stages[[var]] <- tmp$stages[[var]]
  object$ctables[[var]] <- ctable
  object$prob[[var]] <- lapply(seq_along(object$stages[[var]]), function(ix){
    tt <- ctable[ix, ]
    names(tt) <- object$tree[[var]]
    n <- sum(tt)
    tt <- (tt + object$lambda)
    tt <- tt / sum(tt)
    tt[is.nan(tt)] <- NA
    attr(tt, "n") <- n
    return(tt)
  })
  names(object$prob[[var]]) <- object$stages[[var]]
  if (join_unobserved){
    ix <- rowSums(ctable) == 0
    if (any(ix)){
      object$stages[[var]][ix] <- object$name_unobserved[1]
      p.unobserved <- object$prob[[var]][ix][[1]]
      object$prob[[var]][ix] <- NULL
      object$prob[[var]][[object$name_unobserved[1]]] <- p.unobserved
    }  
  }
  object$ll <- NULL ## force recompute log-likelihood
  object$ll <- logLik(object)
  return(object)
}

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
    stop("Invalid data argument. Data must be a data.frame or a table obejct.")
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
    stop("Invalid data argument. Data must be a data.frame or a table obejct.")
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