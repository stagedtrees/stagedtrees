#' Join situations with no observations
#' 
#' @param object a fitted staged event tree
#' @param fit if the probability should be re-computed
#' @param trace if \code{>0} print information to console
#' 
#' @return a staged event tree with situations with 0 
#' observations merged
#' 
#' @details This function takes as input a (fitted) staged event tree object 
#' and looking at the \code{ctables} join all the situations with zero 
#' recorded observations in the same stage. Since such joining does not change
#' the log-likelihood of the model, it is a useful (time-wise) 
#' pre-processing before
#' others model selection algorithms.
#' If \code{fit=TRUE} the model will be then re-fitted, if user sets 
#' \code{fit = FALSE} the returned model will have no probabilities. 
#'  
#' @importFrom  methods is
#' @export
#' 
#' @examples 
#' DD <- generate_random_dataset(n = 5, N = 1000)
#' model_full <- staged_ev_tree(DD, fit = TRUE, full = TRUE, lambda = 1)
#' model <- join_zero_counts(model_full, fit = TRUE)
#' logLik(model_full)
#' logLik(model)
#' BIC(model_full, model)
join_zero_counts <- function(object, fit = TRUE, trace = 0){
  stopifnot(is(object, "staged_ev_tree"))
  stopifnot(is_fitted.staged_ev_tree(object))
  tot <- 0
  for (v in names(object$tree)[-1]){
    new <- new_label(unique(object$stages[[v]]))
    ix <- rowSums(object$ctables[[ v ]]) == 0
    object$stages[[v]][ ix ] <- new 
    tot <- tot + sum(ix)
    if (trace > 1){
      message(v," joined ", sum(ix), " situations")
    }
  }
  if (trace > 0){
    message("joined a total of ", tot, " situations")
  }
  object$prob <- NULL
  object$ll <- NULL
  if (fit){
    object <- fit.staged_ev_tree(object, lambda = object$lambda)
    if (trace > 0){
      message("object fitted using lambda = ", object$lambda)
    }
  }
  return(object)
}


#' Naive staged event tree
#' 
#' Build a stage event tree with two stages for each variable
#' @param object a staged event tree object with ctables 
#' @return A staged event tree with two stages per variable
#' @export
#' @examples 
#' DD <- generate_xor_dataset(n = 4, N = 1000)[,5:1]
#' model_0 <- staged_ev_tree(DD[1:500,], fit = TRUE, lambda = 1)
#' naive_model <- naive_staged_ev_tree(model_0)
#' pr <- predict(naive_model, newdata = DD[501:1000,])
#' table(pr,DD$C[501:1000])
naive_staged_ev_tree <- function(object){
  stopifnot(is_fitted.staged_ev_tree(object))
  for (v in names(object$tree)[-1]){
    M <- KL_mat_prob(object$ctables[[v]] + object$lambda)
    groups <- simple_clustering(M)
    ### compute probabilitites and assign stages
    object$prob[[ v ]] <- list()
    for (s in c("1", "2")){
      object$stages[[v]][ groups[[ s ]] ] <- s
    }
  }
  return(fit.staged_ev_tree(object, lambda = object$lambda))
}

#' Backword random hill-climbing
#'
#' Randomly try to join stages
#'
#' @param object a staged event tree model 
#' @param score the score function to be maximized
#' @param max_iter the maximum number of iteration
#' @param trace if >0 increasingly amount of info 
#' is printed (via \code{message})
#' 
#' @details At each iteration a variable and 
#' two of its stages are randomly selected. 
#' If joining the stages increase the score, the model is 
#' updated. The procedure is repeated until the 
#' number of iterations reach \code{max_iter}.
#' @return The final staged event tree object
#' @export
#' @importFrom stats  BIC
#' @importFrom  methods is
backward_hill_climb_random <-
  function(object,
           score = function(x)
             return(-BIC(x))
           , max_iter = 100
           , trace = 0) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    now_score <- score(object)
    r <- 1
    iter <- 0
    while (iter < max_iter) {
      ## chose randomly one of the variable and try to perform a stage-merging
      iter <- iter + 1
      v <- sample(names(object$tree)[-1], size = 1)
      if (length(unique(object$stages[[v]])) > 1) {
        stgs <-
          sample(unique(object$stages[[v]]), size = 2, replace = FALSE) ##select randomly two stages
        try <-
          join_stages(object, v, stgs[1], stgs[2]) ## join the 2 stages
        try_score <- score(try)
        if (try_score >= now_score) {
          object <- try
          r <-
            abs((try_score - now_score) / now_score) ##compute relative score increase
          now_score <- try_score
          if (trace > 1) {
            message(paste(v, "joined stages: ", stgs[1], "and", stgs[2]))
          }
        }
      }
    }
    if (trace > 0) {
      message(paste("backword HC random done after", iter, "iteration"))
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }



#' Backword hill-climbing
#'
#' Hill-climbing search of staged event trees with 
#' iterative joining of stages
#' 
#' @param object a staged event tree model
#' @param score the score function to be maximized
#' @param max_iter the maximum number of iterations per variable
#' @param trace if >0 increasingly amount of info 
#' is printed (via \code{message})
#' @details For each variable the algorithm try to join stages 
#' and move to the best model that increase the score. When no 
#' increase is possible it moves to the next variable.
#' @return The final staged event tree object
#' @examples 
#' DD <- generate_random_dataset(n = 4, N = 1000)
#' model_full <- staged_ev_tree(DD, fit = TRUE, full = TRUE, lambda = 1)
#' model <- backward_hill_climb(model_full, trace = 2)
#' BIC(model_full, model)
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
backward_hill_climb <-
  function(object,
           score = function(x)
             return(-BIC(x))
           ,
           max_iter = Inf,
           trace = 0) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    now_score <- score(object)
 
    for (v in names(object$tree)[-1]) {
      r <- 1
      iter <- 0
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object #clone the object
        temp_score <- now_score
        done <- TRUE
        stages <- unique(object$stages[[v]])
        if (length(stages) > 1) {
          for (i in 2:length(stages)) {
            ##try all stages pair
            s1 <- stages[i]
            for (j in 1:(i - 1)) {
              s2 <- stages[j]
              try <-
                join_stages(object, v, s1, s2) ## join the 2 stages
              try_score <- score(try)
              if (try_score >= temp_score) {
                temp <- try
                temp_score <- try_score
                s1a <- s1
                s2a <- s2
                done <- FALSE
              }
            }
          }
        } ##end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if ( (trace > 1) && !done) {
          message(v, " joined stages: ", s1a, " and ", s2a)
        }
      } ## end while
      if (trace > 0){
        message("HC over ", v, " done after ", iter, " iterations")
      }
    } ## end for over variables
    if (trace > 0) {
      message("Backword HC done")
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }


#' Fast backword hill-climbing
#'
#' Fast hill-climbing search of staged event trees with 
#' iterative joining of stages.
#'
#' @param object a staged event tree model
#' @param score the score function to be maximized
#' @param max_iter the maximum number of iteration
#' @param trace if >0 increasingly amount of info 
#' is printed (via \code{message})
#' @details For each variable the algorithm try to join stages 
#' and move to the first model that increase the score. When no 
#' increase is possible it moves to the next variable.
#' @return The final staged event tree obtained
#' @examples 
#' DD <- generate_random_dataset(n = 5, N = 1000)
#' model_full <- staged_ev_tree(DD, fit = TRUE, full = TRUE, lambda = 1)
#' model <- fast_backward_hill_climb(model_full, trace = 2)
#' BIC(model_full, model)
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
fast_backward_hill_climb <-
  function(object = NULL,
           score = function(x)
             return(-BIC(x)),
           max_iter = Inf,
           trace = 0) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    now_score <- score(object)
    for (v in names(object$tree)[-1]) {
      iter <- 0
      r <- 1
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object #clone the object
        temp_score <- now_score #clone the score
        s1_select <- NULL
        s2_select <- NULL
        done <- TRUE
        stages <- unique(object$stages[[v]])
        if (length(stages) > 1) {
          for (i in 2:length(stages)) {
            ##try all stages pair
            s1 <- stages[i]
            for (j in 1:(i - 1)) {
              s2 <- stages[j]
              try <-
                join_stages(object, v, s1, s2) ## join the 2 stages
              try_score <- score(try)
              if (try_score >= temp_score) {
                temp <- try
                temp_score <- try_score
                s1_select <- s1 #just to message it if verbose
                s2_select <- s2 #just to message it if verose
                done <- FALSE
                break
              }
            }
            if (!done)
              break
          }
        } ##end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if ((trace > 1) && !done)
          message(v, " joined stages: ",
                        s1_select, " and ", s2_select)
      } ## end while
      if (trace > 0 ) {
        message(
          "fast HC over ",
          v ,
          " done after ",
          iter,
          " iterations."
        )
      }
    } ## end for over variables
    if (trace > 0) {
      message("fast HC done")
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }


#' Backword joining of stages
#'
#' Join stages from more complex to simpler models
#' using entropy do decide if join or not a stage
#'
#' @param object the staged event tree from where to start
#' @param thr the threshold for joining stages
#' @param trace if >0 increasingly amount of info 
#' is printed (via \code{message})
#' @return the learned staged event tree
#' 
#' @examples 
#' DD <- generate_random_dataset(n = 5, N = 1000)
#' model_full <- staged_ev_tree(DD, fit = TRUE, full = TRUE, lambda = 1)
#' model <- backward_joining_KL(model_full, trace = 2)
#' BIC(model_full, model)
#' @importFrom  methods is
#' @export
backward_joining_KL <-
  function(object = NULL,
           thr = 0.01,
           trace = 0) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    for (v in names(object$tree)[-1]) {
      finish <- FALSE
      while (!finish) {
        finish <- TRUE
        stages <- unique(object$stages[[v]])
        if (length(stages) > 1) {
          M <- KL_mat_stages(object$prob[[v]]) #compute KL matrix
          diag(M) <- Inf
          idx <- which.min(M)
          i <- ceiling(idx / dim(M)[1])
          j <- idx - (i - 1) * dim(M)[1]
          s1 <- stages[i]
          s2 <- stages[j]
          if (abs(M[i, j]) < thr) {
            object <-
              join_stages(object, v, s1, s2) ## join the 2 stages
            finish <- FALSE #if joined the stages we are not finish
            if (trace > 1){
              message(v, " joined stages: ", s1,
                      " and ", s2)
            }
          }
        } ## end if there are more than 1 stage
      } ## end while
      if (trace > 0 ) {
        message("KL join over ", v ," done")
      }
    } ## end for over variables
    if (trace > 0) {
      message("KL join done")
    }
    object$call <- sys.call()
    return(object)
  }
