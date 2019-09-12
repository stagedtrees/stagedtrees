#' Join situations with no observations
#'
#' @param object a fitted staged event tree
#' @param fit if the probability should be re-computed
#' @param name string with a name for the new stage
#' @param trace if \code{> 0} print information to console
#'
#' @return a staged event tree with situations with 0
#' observations merged in a single stage
#'
#' @details It takes as input a (fitted) staged event tree object
#' and looking at the \code{ctables} it joins, in the same stage, all the situations with zero
#' recorded observations. Since such joining does not change
#' the log-likelihood of the model, it is a useful (time-wise)
#' pre-processing before others model selection algorithms.
#' If \code{fit=TRUE} the model will be then re-fitted, if user sets
#' \code{fit=FALSE} the returned model will have no probabilities.
#'
#' @importFrom  methods is
#' @export
#'
#' @examples
#' DD <- generate_xor_dataset(n = 5, N = 1000)
#' model_full <- full(DD, lambda = 1)
#' model <- join_zero_counts(model_full, fit = TRUE)
#' logLik(model_full)
#' logLik(model)
#' BIC(model_full, model)
join_zero_counts <-
  function(object,
           fit = TRUE,
           trace = 0,
           name = NULL) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted.sevt(object))
    tot <- 0
    for (v in names(object$tree)[-1]) {
      if (is.null(name))
        new <- new_label(unique(object$stages[[v]]))
      else
        new <- name
      ix <- rowSums(object$ctables[[v]]) == 0
      object$stages[[v]][ix] <- new
      tot <- tot + sum(ix)
      if (trace > 1) {
        message(v, " joined ", sum(ix), " situations")
      }
    }
    if (trace > 0) {
      message("joined a total of ", tot, " situations")
    }
    object$prob <- NULL
    object$ll <- NULL
    if (fit) {
      object <- sevt.fit(object, lambda = object$lambda)
      if (trace > 0) {
        message("object fitted using lambda = ", object$lambda)
      }
    }
    return(object)
  }


#' Naive staged event tree
#'
#' Build a stage event tree with two stages for each variable
#' @param object a full staged event tree
#' @param distance a distance between probabilities
#' @param k the maximum number of variable to consider
#' @return A staged event tree with two stages per variable
#' @export
#' @examples
#' DD <- generate_xor_dataset(n = 4, N = 1000)[,5:1]
#' naive_model <- naive.sevt(full(DD, lambda = 1))
#' pr <- predict(naive_model, newdata = DD[501:1000,])
#' table(pr,DD$C[501:1000])
naive.sevt <-
  function(object,
           distance = kl,
           k = length(object$tree)) {
    stopifnot(is_fitted.sevt(object))
    for (v in names(object$tree)[2:k]) {
      M <- distance_mat_stages(object$prob[[v]])
      groups <- simple_clustering(M)
      ### compute probabilitites and assign stages
      object$prob[[v]] <- list()
      for (s in c("1", "2")) {
        object$stages[[v]][groups[[s]]] <- s
      }
    }
    object$call <- sys.call()
    return(sevt.fit(object, lambda = object$lambda))
  }

#' Backward Random Hill-Climbing
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
#'
#' @return The final staged event tree object
#' @export
#' @examples
#' DD <- generate_xor_dataset(n = 4, N = 100)
#' model <- bhcr.sevt(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @importFrom  methods is
bhcr.sevt <-
  function(object,
           score = function(x)
             return(-BIC(x))
           ,
           max_iter = 100
           ,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted.sevt(object))
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
                 replace = FALSE) ##select randomly two stages
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
      message(paste("backward HC random done after", iter, "iteration"))
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }



#' Backward hill-climbing
#'
#' Backward Hill-climbing search of staged event trees with
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
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(n = 4, N = 100)
#' model <- bhc.sevt(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
bhc.sevt <-
  function(object,
           score = function(x)
             return(-BIC(x))
           ,
           max_iter = Inf,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted.sevt(object))
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
        if ((trace > 1) && !done) {
          message(v, " joined stages: ", s1a, " and ", s2a)
        }
      } ## end while
      if (trace > 0) {
        message("BHC over ", v, " done after ", iter, " iterations")
      }
    } ## end for over variables
    if (trace > 0) {
      message("BHC done")
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }


#' Fast backward hill-climbing
#'
#' Fast backward hill-climbing search of staged event trees with
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
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(n = 5, N = 100)
#' model <- fbhc.sevt(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
fbhc.sevt <-
  function(object = NULL,
           score = function(x)
             return(-BIC(x)),
           max_iter = Inf,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted.sevt(object))
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
      if (trace > 0) {
        message("fast HC over ",
                v ,
                " done after ",
                iter,
                " iterations.")
      }
    } ## end for over variables
    if (trace > 0) {
      message("fast HC done")
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }


#' Backward joining of stages
#'
#' Join stages from more complex to simpler models
#' using a distance and a threshold value
#'
#' @param object the staged event tree from where to start
#' @param distance the distance between probabilities to use
#' @param thr the threshold for joining stages
#' @param trace if >0 increasingly amount of info
#' @param ... additional parameters to be passed to the distance function
#' is printed (via \code{message})
#' 
#' @details For each variable in the model stages are joined iteratively. 
#' At each iteration the two stages with minimum distance are selected and
#' joined if their distance is less than \code{thr}. 
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(n = 5, N = 1000)
#' model <- bj.sevt(full(DD, lambda = 1), trace = 2)
#' summary(model)
#' @importFrom  methods is
#' @export
bj.sevt <-
  function(object = NULL,
           distance = kl,
           thr = 0.1,
           trace = 0,
           ...) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted.sevt(object))
    stopifnot(is(distance, "function"))
    for (v in names(object$tree)[-1]) {
      finish <- FALSE
      while (!finish) {
        finish <- TRUE
        stages <- unique(object$stages[[v]])
        if (length(stages) > 1) {
          M <- distance_mat_stages(object$prob[[v]], distance, ...)
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
            if (trace > 1) {
              message(v, " joined stages: ", s1,
                      " and ", s2)
            }
          }
        } ## end if there are more than 1 stage
      } ## end while
      if (trace > 0) {
        message("backward join over ", v , " done")
      }
    } ## end for over variables
    if (trace > 0) {
      message("backward join done")
    }
    object$call <- sys.call()
    return(object)
  }



#' Hill-Climb Score optimization
#'
#' Hill-climbing search of staged event trees with
#' iterative moving of nodes between stages.
#' 
#' @param object a staged event tree object
#' @param score a function that score staged event tree objects
#' @param max_iter the maximum number of iterations per variable
#' @param trace integer, if positive information on the progress is
#'              printed to console
#'              
#' @details For each variable the node-move that best increase the 
#' score is performed. A node-move is either changing the stage 
#' associate to a node or move the node to a new stage. 
#' 
#' @return The final staged event tree obtained.
#'
#' @examples
#' model <- hc.sevt(full(PhDArticles[,1:4], lambda = 1))
#' summary(model)
#' @export
hc.sevt <- function(object,
                    score = function(x)
                      return(-BIC(x)),
                    max_iter = Inf,
                    trace = 0) {
  stopifnot(is(object, "sevt"))
  stopifnot(is_fitted.sevt(object))
  stopifnot(!is.null(object$ctables))
  now_score <- score(object)
  for (v in names(object$tree)[-1]) {
    done <- FALSE
    iter <- 0
    while (!done & iter < max_iter) {
      iter <- iter + 1
      temp <- object #clone the object
      temp_score <- now_score #clone the score
      stages <- object$stages[[v]]
      ustages <- unique(stages)
      newname <- new_label(ustages)
      done <- TRUE
      for (j in 1:length(ustages)) {
        s1 <- ustages[j]
        idx <- (1:length(stages))[stages == s1]
        for (i in idx) {
          try <- object
          for (s2 in c(ustages[-j], newname)) {
            try$stages[[v]][i] <- s2
            try <- sevt.fit(try, lambda = object$lambda)
            try_score <- score(try)
            if (try_score > temp_score) {
              temp <- try
              temp_score <- try_score
              ia <- i #just to message it if verbose
              s1a <- s1
              s2a <- s2
              done <- FALSE
            }
          }
        }
      }##end for over stages
      object <- temp
      now_score <- temp_score
      if ((trace > 1) && !done) {
        message(v, " moved ", ia, " from stage ", s1a, " to stage ",
                s2a)
      }
    }##end while
    if (trace > 0) {
      message(v, " HC done")
    }
  }##end for over variables
  if (trace > 0) {
    message("HC over ",
            v ,
            " done after ",
            iter,
            " iterations.")
  }
  object$call <- sys.call()
  return(object)
}
