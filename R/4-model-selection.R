


forward_select.staged_ev_treee <-
  function(object = NULL,
           data = NULL,
           lambda = 1,
           score = function(x)
             return(-BIC(x))) {
    if (is.null(object)) {
      if (is.null(data)) {
        warning("Provide something fitted staged event tree or data")
        return(NULL)
      }
      object <- staged_ev_tree(data, fit = TRUE, lambda = lambda)
    }
    if (is.null(data)) {
      data <- object$data
      if (is.null(data)) {
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
#' @param object a staged event tree model 
#' @param score the score function to be maximized
#' @param eps the stopping criteria for the relative score increase
#' @param max_iter the maximum number of iteration
#' @param verbose If info should be printed (via \code{message})
#' @export
#' @importFrom stats  BIC
#' @importFrom  methods is
backward_hill_climb_random <-
  function(object = NULL,
           score = function(x)
             return(-BIC(x))
           ,
           eps = 0.0001,
           max_iter = 100
           ,
           verbose = FALSE) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    now_score <- score(object)
    r <- 1
    iter <- 0
    while (r > eps &&
           iter < max_iter) {
      ## chose randomly one of the variable and try to perform a stage-merging
      iter <- iter + 1
      v <- sample(names(object$tree)[-1], size = 1)
      if (length(object$stages[[v]]) > 1) {
        stgs <-
          sample(object$stages[[v]], size = 2, replace = FALSE) ##select randomly two stages
        try <-
          join_stages(object, v, stgs[1], stgs[2]) ## join the 2 stages
        try_score <- score(try)
        if (try_score >= now_score) {
          object <- try
          r <-
            abs((try_score - now_score) / now_score) ##compute relative score increase
          now_score <- try_score
          if (verbose) {
            message(paste("joined for", v, "stages: ", stgs[1], "and", stgs[2]))
          }
        }
      }
    }
    if (verbose) {
      message(paste("Exit after", iter, "iteration"))
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }



#' backword hill-climbing
#'
#' Each iter move to the best model, from more complicated to simpler model
#'
#' @param object a staged event tree model
#' @param score the score function to be maximized
#' @param max_iter the maximum number of iteration
#' @param verbose If info should be printed (via \code{message})
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
backward_hill_climb <-
  function(object = NULL,
           score = function(x)
             return(-BIC(x))
           ,
           max_iter = Inf,
           verbose = FALSE) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    now_score <- score(object)
    iter <- 0
    for (v in names(object$tree)[-1]) {
      r <- 1
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object #clone the object
        temp_score <- now_score
        v_sel <- NULL
        done <- TRUE
        if (length(object$stages[[v]]) > 1) {
          for (i in 2:length(object$stages[[v]])) {
            ##try all stages pair
            s1 <- object$stages[[v]][i]
            for (j in 1:(i - 1)) {
              s2 <- object$stages[[v]][j]
              try <-
                join_stages(object, v, s1, s2) ## join the 2 stages
              try_score <- score(try)
              if (try_score >= temp_score) {
                temp <- try
                temp_score <- try_score
                s1a <- s1
                s2a <- s2
                v_sel <- v
                done <- FALSE
              }
            }
          }
        } ##end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if (verbose && !done) {
          message(paste("Variable", v_sel, "joined stages:", s1a, "and", s2a))
        }
      } ## end while
    } ## end for over variables
    if (verbose) {
      message(paste("Exit after", iter, "iteration"))
    }
    object$call <- sys.call()
    object$score <- list(value = now_score, f = score)
    return(object)
  }


#' Fast backword hill-climbing
#'
#' Move to the first model that increase the score
#'
#' @param object a staged event tree model
#' @param score the score function to be maximized
#' @param max_iter the maximum number of iteration
#' @param verbose If info should be printed (via \code{message})
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
fast_backward_hill_climb <-
  function(object = NULL,
           score = function(x)
             return(-BIC(x)),
           max_iter = Inf,
           verbose = FALSE) {
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
        if (length(object$stages[[v]]) > 1) {
          for (i in 2:length(object$stages[[v]])) {
            ##try all stages pair
            s1 <- object$stages[[v]][i]
            for (j in 1:(i - 1)) {
              s2 <- object$stages[[v]][j]
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
        if (verbose && !done)
          message(paste("Joined stage:",
                        s1_select, "and", s2_select))
      } ## end while
      if (verbose) {
        message(paste(
          "Hill-Climb over variable",
          v ,
          "done after",
          iter,
          "iterations."
        ))
      }
    } ## end for over variables
    
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
#' @param lambda the laplace smoothing
#' @param thr the threshold for joining stages
#' @param verbose logical
#' @return the learned staged event tree
#' @importFrom  methods is
backward_joining <-
  function(object = NULL,
           lambda = 1,
           thr = 0.01,
           verbose = FALSE) {
    stopifnot(is(object, "staged_ev_tree"))
    stopifnot(is_fitted.staged_ev_tree(object))
    for (v in names(object$tree)[-1]) {
      finish <- FALSE
      while (!finish) {
        finish <- TRUE
        if (length(object$stages[[v]]) > 1) {
          M <- KL_mat_stages(object$prob[[v]]) #compute KL matrix
          diag(M) <- Inf
          idx <- which.min(M)
          i <- ceiling(idx / dim(M)[1])
          j <- idx - (i - 1) * dim(M)[1]
          s1 <- object$stages[[v]][i]
          s2 <- object$stages[[v]][j]
          if (abs(M[i, j]) < thr) {
            if (verbose)
              message("Variable ", v , " Joined stages ", s1,
                      " and ", s2)
            object <-
              join_stages(object, v, s1, s2) ## join the 2 stages
            finish <- FALSE #if joined the stages we are not finish
          }
        } ## end if there are more than 1 stage
      } ## end while
    } ## end for over variables
    object$call <- sys.call()
    return(object)
  }
