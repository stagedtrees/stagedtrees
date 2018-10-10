




forward_select.staged_ev_treee <- function(object=NULL, data=NULL, lambda = 1,
                                           score = function(x) return(-BIC(x))){
  if (is.null(object)){
    if (is.null(data)){
      warning("Provide something fitted staged event tree or data")
      return(NULL)
    }
    object <- staged_ev_tree(data, fit =TRUE, lambda = lambda)
  }
  if (is.null(data)){
    data <- object$data
    if (is.null(data)){
      warning("Provide data")
      return(object)
    }
  }
  now <- score(object)

}

#' backword naive random hill-climbing
#'
#' Randomly select a model and selct it if it increase the score
#'
#' @param object a staged event tree model (optional)
#' @param data the data (can be not specified if it is attached to \code{object})
#' @param order the order of the tree (optional)
#' @param lambda the laplace smoothing factor
#' @param score the score function to be maximized
#' @param eps the stopping criteria for the relative score increase
#' @param max_iter the maximum number of iteration
#' @param verbose If info should be printed
#' @export
backward_hill_climb_random <- function(object = NULL, data = NULL, order = NULL
                                           , lambda=1
                                           , score = function(x) return( - BIC(x) )
                                           , eps = 0.0001, max_iter = 100
                                           , verbose = FALSE){
  if (is.null(object)){
    if (is.null(data)){
      warning("Provide something: the fitted staged event tree or data")
      return(NULL)
    }
    ## if the staged event tree is not provided initialize it to the full model
    object <- staged_ev_tree(strt_ev_tree(data, fit = TRUE,
                                          order = order, lambda = lambda))
  }
  if (is.null(data)){
    data <- sevt$data
    if (is.null(data)){
      warning("Provide data")
      return(object)
    }
  }
  now_score <- score(object)
  r <- 1
  iter <- 0
  while (r > eps && iter < max_iter){ ## chose randomly one of the variable and try to perform a stage-merging
    iter <- iter + 1
    v <- sample(names(object$tree)[ - 1 ],size = 1)
    if (length(object$stages[[ v ]]) > 1 ){
      stgs <- sample(object$stages[[ v ]], size = 2, replace = FALSE) ##select randomly two stages
      try <- join_stages(object, v, stgs[1], stgs[2]) ## join the 2 stages
      try_score <- score(try)
      if (try_score > now_score){
        object <- try
        r <- abs((try_score - now_score) / now_score) ##compute relative score increase
        now_score <- try_score
        if (verbose){
           print(paste("joined for", v, "stages: ", stgs[1], "and", stgs[2]))
        }
      }
    }
  }
  if (verbose){ print(paste("Exit after", iter, "iteration"))}
  return(object)
}



#' backword hill-climbing
#'
#' Each iter move to the best model, from more complicated to simpler model
#'
#' @param object a staged event tree model (optional)
#' @param data the data (can be not specified if it is attached to \code{object})
#' @param order the order of the tree (optional)
#' @param lambda the laplace smoothing factor
#' @param score the score function to be maximized
#' @param eps the stopping criteria for the relative score increase
#' @param max_iter the maximum number of iteration
#' @param verbose If info should be printed
backward_hill_climb <- function(object = NULL, data = NULL, order = NULL
                                                      , lambda=1
                                                      , score = function(x) return( - BIC(x) )
                                                      , eps = 0.0001, max_iter = 100
                                                      , verbose = FALSE){
  if (is.null(object)){
    if (is.null(data)){
      warning("Provide something: the fitted staged event tree or data")
      return(NULL)
    }
    ## if the staged event tree is not provided initialize it to the full model
    object <- staged_ev_tree(strt_ev_tree(data, fit = TRUE,
                                          order = order, lambda = lambda))
  }
  if (is.null(data)){
    data <- sevt$data
    if (is.null(data)){
      warning("Provide data")
      return(object)
    }
  }
  now_score <- score(object)
  r <- 1
  iter <- 0
  while (r > eps && iter < max_iter){ ## chose randomly one of the variable and try to perform a stage-merging
    iter <- iter + 1
    temp <- object #clone the object
    temp_score <- now_score
    for (v in names(object$tree)[-1]){
      if (length(object$stages[[ v ]]) > 1 ){
        for (i in 2:length(object$stages[[ v ]])){ ##try all stages pair
          s1 <- object$stages[[ v ]][i]
          for (j in 1:(i-1)){
             s2 <- object$stages[[ v ]][j]
             try <- join_stages(object, v, s1, s2) ## join the 2 stages
             try_score <- score(try)
             if (try_score > temp_score){
               temp <- try
               temp_score <- try_score
             }
          }
        }
      } ##end if there are more than 1 stage
    } ## end for over variables now in temp there is the best possible incremented model
    r <- abs((temp_score - now_score) / now_score) ##compute relative score increase
    object <- temp
    now_score <- temp_score
    object$score <- now_score
    if (verbose){
      print(paste("joined stages:"))
    }
  } ## end while
  if (verbose){ print(paste("Exit after", iter, "iteration"))}
  object$call <- NULL #todo
  return(object)
}


#' Fast backword hill-climbing
#'
#' Each iter move to the best model, from more complicated to simpler model
#' It's fast because we iterate over the variable just once and not at every while iteration
#'
#' @param object a staged event tree model (optional)
#' @param data the data (can be not specified if it is attached to \code{object})
#' @param order the order of the tree (optional)
#' @param lambda the laplace smoothing factor
#' @param score the score function to be maximized
#' @param eps the stopping criteria for the relative score increase
#' @param max_iter the maximum number of iteration
#' @param verbose If info should be printed
fast_backward_hill_climb <- function(object = NULL, data = NULL, order = NULL
                                , lambda=1
                                , score = function(x) return( - BIC(x) )
                                , eps = 0, max_iter = 1000
                                , verbose = FALSE){
  if (is.null(object)){
    if (is.null(data)){
      warning("Provide something: the fitted staged event tree or data")
      return(NULL)
    }
    ## if the staged event tree is not provided initialize it to the full model
    object <- staged_ev_tree(strt_ev_tree(data, fit = TRUE,
                                          order = order, lambda = lambda))
  }
  if (is.null(data)){
    data <- sevt$data
    if (is.null(data)){
      warning("Provide data")
      return(object)
    }
  }
  now_score <- score(object)
  for (v in names(object$tree)[-1]){
    iter <- 0
    r <- 1
    while (r > eps && iter < max_iter){ ## chose randomly one of the variable and try to perform a stage-merging
      iter <- iter + 1
      temp <- object #clone the object
      temp_score <- now_score #clone the score
      if (length(object$stages[[ v ]]) > 1 ){
        for (i in 2:length(object$stages[[ v ]])){ ##try all stages pair
          s1 <- object$stages[[ v ]][i]
          for (j in 1:(i-1)){
            s2 <- object$stages[[ v ]][j]
            try <- join_stages(object, v, s1, s2) ## join the 2 stages
            try_score <- score(try)
            if (try_score > temp_score){
              temp <- try
              temp_score <- try_score
            }
          }
        }
      } ##end if there are more than 1 stage
      r <- abs((temp_score - now_score) / now_score) ##compute relative score increase
      object <- temp
      now_score <- temp_score
      object$score <- now_score
    } ## end while

  } ## end for over variables
  if (verbose){ print(paste("Exit after", iter, "iteration"))}
  object$call <- NULL #todo
  return(object)
}


