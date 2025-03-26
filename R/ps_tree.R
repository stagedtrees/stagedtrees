#' Ps-tree
#'
#' Build the ps-tree associated to a given staged event tree.
#' @param object a fitted object of class \code{sevt}.
#' @param outcome the outcome variable.
#' @param treatment the treatment variable.
#' @param ignore ignored stages.
#' @export
ps_tree <- function(object, outcome, treatment, ignore = object$name_unobserved){
  check_sevt(object)
  check_scope(c(outcome, treatment), object)
  was_fitted <- is_fitted_sevt(object)
  stgs <- unique(object$stages[[treatment]])
  stgs <- stgs[!(stgs %in% ignore)]
  strata <- sapply(stgs, function(stage){
    get_path(object, treatment, stage)
  }, simplify = FALSE)

  tmp <- object$stages[[outcome]]
  #kk <- sum(! (tmp %in% ignore))
  #object$stages[[outcome]][! (tmp %in% ignore)] <- paste0(1:kk)
  for (i in 1:length(strata)){
    nn <- names(strata)[i]
    for (j in 1:nrow(strata[[i]])){
      todo <-  stages(object)[outcome, strata[[i]][j,,drop = FALSE]]
      stages(object)[outcome, strata[[i]][j,,drop = FALSE], fit = FALSE] <- paste(nn, 1:length(todo), sep = "-")
    }
  }

  if (was_fitted){
    sevt_fit(object)
  }else{
    return(object)
  }
}
