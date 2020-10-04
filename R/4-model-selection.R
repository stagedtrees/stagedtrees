#' Join situations with no observations
#'
#' @param object a fitted staged event tree.
#' @param fit if the model's probability should be computed.
#' @param name string with a name for the new stage.
#' @param scope list of variable in \code{object}.
#' @param trace if \code{> 0} print information to console.
#' @param lambda smoothing parameter for the fitting.
#'
#' @return a staged event tree with situations with 0
#' observations merged in a single stage.
#'
#' @details It takes as input a (fitted) staged event tree object
#' and looking at the \code{ctables} it joins, in the same stage, all the situations with zero
#' recorded observations. Since such joining does not change
#' the log-likelihood of the model, it is a useful (time-wise)
#' pre-processing before others model selection algorithms.
#' If \code{fit=TRUE} the model will be then re-fitted, if user sets
#' \code{fit=FALSE} the returned model will have no probabilities.
#' 
#' Users can decide to join unobserved situations directly in 
#' \code{\link{full}} or \code{\link{indep}}, setting 
#' \code{join_unobserved = TRUE}.
#'
#' @importFrom methods is
#' @export
#'
#' @examples
#' DD <- generate_xor_dataset(n = 5, N = 10)
#' model_full <- full(DD, lambda = 1)
#' model <- join_unobserved(model_full)
#' logLik(model_full)
#' logLik(model)
#' BIC(model_full, model)
join_unobserved <-
  function(object,
           fit = TRUE,
           trace = 0,
           name = 'UNOBSERVED',
           scope = sevt_varnames(object)[-1],
           lambda = object$lambda) {
    stopifnot(is(object, "sevt"))
    stopifnot(!is.null(object$ctables))
    tot <- 0
    ## make scope valid 
    scope <- scope[scope %in% sevt_varnames(object)[-1]]
    for (v in scope) {
      if (is.null(name)) {
        new <- new_label(unique(object$stages[[v]]))
      } else {
        new <- paste0(name)
      }
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
      object <- sevt_fit(object, lambda = lambda)
      if (trace > 0) {
        message("object fitted using lambda = ", lambda)
      }
    }
    object$name_unobserved <- name
    return(object)
  }

#' Full and independent staged event tree
#' 
#' @name full_indep
#' @details  Functions to create full or independent staged tree models.
#'           The full (or saturated) staged tree is the model where every 
#'           situation is in a different stage, and thus the model has the 
#'           maximum number of parameters. 
#'           Conversely, the independent staged tree (`indep`) assign
#'           all the situation related to the same variable to the same 
#'           stage, thus it is equivalent to the independence factorization
#'           \eqn{P(X1, X2, X3, ...) = P(X1)P(X2)P(X3)...} .
NULL

#' @rdname full_indep
#' @param data data to create the model, data.frame or table. 
#' @param order character vector, order of variables.
#' @param join_unobserved logical, if situations with zero observations should 
#'                           be joined.
#' @param name_unobserved name to pass to \code{\link{join_unobserved}}.
#' @param lambda smoothing coefficient.
#' @examples
#'
#' ######### full model
#'
#' DD <- generate_xor_dataset(4, 100)
#' model_full <- full(DD, lambda = 1)
#' @export
full <- function(data, order = NULL, 
                 join_unobserved = FALSE,
                 lambda = 0,
                 name_unobserved = "UNOBSERVED") {
  UseMethod("full", data)
}

#' @rdname full_indep
#' @export
full.table <- function(data, order = names(dimnames(data)), 
                       join_unobserved = FALSE,
                       lambda = 0,
                       name_unobserved = "UNOBSERVED"){
  object <- sevt(data, full = TRUE, order = order)
  object$ctables <- make_ctables(object, data)
  if (join_unobserved){
    join_unobserved(object, 
              fit = TRUE, name = name_unobserved, lambda = lambda)
  }else{
    sevt_fit(object, lambda = lambda)
  }
}

#' @rdname full_indep
#' @export
full.data.frame <- function(data, order = colnames(data),
                            join_unobserved = FALSE,
                            lambda = 0,
                            name_unobserved = "UNOBSERVED"){
  object <- sevt(data, full = TRUE, order = order)
  object$ctables <- make_ctables(object, data)
  if (join_unobserved){
    join_unobserved(object, 
              fit = TRUE, name = name_unobserved, lambda = lambda)
  }else{
    sevt_fit(object, lambda = lambda)
  }
}


#' @rdname full_indep
#' @export
indep <- function(data, order = NULL,
                  join_unobserved = FALSE,
                  lambda = 0,
                  name_unobserved = "UNOBSERVED") {
  UseMethod("indep", data)
}

#' @rdname full_indep
#' @export
indep.table <- function(data, order = names(dimnames(data)),
                        join_unobserved = FALSE, lambda = 0, 
                        name_unobserved = "UNOBSERVED") {
  object <- sevt(data, full = FALSE, order = order)
  object$ctables <- make_ctables(object, data)
  if (join_unobserved){
    join_unobserved(object, 
              fit = TRUE, name = name_unobserved, lambda = lambda)
  }else{
    sevt_fit(object, lambda = lambda)
  }
}

#' @rdname full_indep
#' @examples
#'
#' ######### independence model (data.frame)
#' DD <- generate_xor_dataset(4, 100)
#' system.time(model <- indep(DD, lambda = 1))
#' model
#' @export
indep.data.frame <- function(data, order = colnames(data),
                             join_unobserved = FALSE, lambda = 0, 
                             name_unobserved = "UNOBSERVED") {
  # create the staged tree object
  model <- sevt(data, full = FALSE, order = order)
  # create empty probability list
  model$prob <- list()
  # extract names of variables
  var <- names(model$tree)
  # store lambda value
  model$lambda <- lambda
  # initialize loglik to 0
  model$ll <- 0
  # iterate for each variable 
  for (v in var) {
    # extract the table of the given variable
    ctab <- table(data[[v]])
    # obtain sums of cases
    n <- sum(ctab)
    # compute probability table prob = (ctab + lambda)/sum(ctab + lambda)
    model$prob[[v]] <- list("1" = ctab + lambda)
    model$prob[[v]][["1"]] <-
      model$prob[[v]][["1"]] / sum(model$prob[[v]][["1"]])
    # store sample size
    attr(model$prob[[v]][["1"]], "n") <- n
    # compute where prob > 0
    ix <- ctab > 0
    # set appropriate class (get rid of table formatting)
    class(model$prob[[v]][["1"]]) <- "numeric"
    # update loglik
    model$ll <-
      model$ll + sum(ctab[ix] * log(model$prob[[v]][["1"]][ix]))
  }
  # finish setting up loglik
  # store degrees of freedom
  attr(model$ll, "df") <-
    sum(vapply(model$tree, length, FUN.VALUE = 1) - 1)
  # store number of obs
  attr(model$ll, "nobs") <- nrow(data)
  # set logLik class
  class(model$ll) <- "logLik"
  # store contingency tables
  model$ctables <- make_ctables(model, data)
  if (join_unobserved){
    join_unobserved(model, fit = TRUE, name = name_unobserved)
  }else{
    model
  }
}



#' Backward Random Hill-Climbing
#'
#' Randomly try to join stages. 
#' This is a pretty-useless function, used for comparisons. 
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
#' model <- stages_bhcr(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @importFrom  methods is
stages_bhcr <-
  function(object,
           score = function(x) {
             return(-BIC(x))
           },
           max_iter = 100,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted_sevt(object))
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



#' Backward hill-climbing
#'
#' Backward Hill-climbing search of staged event trees with
#' iterative joining of stages
#'
#' @param object a staged event tree model.
#' @param score the score function to be maximized.
#' @param max_iter the maximum number of iterations per variable.
#' @param scope names of variables that should be considered for the optimization.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{message}).
#' @details For each variable the algorithm try to join stages
#' and move to the best model that increase the score. When no
#' increase is possible it moves to the next variable.
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(n = 4, N = 100)
#' model <- stages_bhc(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
stages_bhc <-
  function(object,
           score = function(x) {
             return(-BIC(x))
           },
           max_iter = Inf,
           scope = NULL,
           ignore = object$name_unobserved,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted_sevt(object))
    now_score <- score(object)
    if (is.null(scope)){
      scope <- sevt_varnames(object)[-1]
    }
    stopifnot(all(scope %in% sevt_varnames(object)[-1]))
    for (v in scope) {
      r <- 1
      iter <- 0
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object # clone the object
        temp_score <- now_score
        done <- TRUE
        stages <- unique(object$stages[[v]])
        stages <- stages[!(stages %in% ignore)]
        if (length(stages) > 1) {
          for (i in 2:length(stages)) {
            ## try all stages pair
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
        } ## end if there are more than 1 stage
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
#' @param object a staged event tree model.
#' @param score the score function to be maximized.
#' @param max_iter the maximum number of iteration.
#' @param scope names of variables that should be considered for the optimization.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{message})
#' @details For each variable the algorithm try to join stages
#' and move to the first model that increase the score. When no
#' increase is possible it moves to the next variable.
#' 
#' 
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(n = 5, N = 100)
#' model <- stages_fbhc(full(DD), trace = 2)
#' summary(model)
#' @importFrom stats  BIC
#' @importFrom  methods is
#' @export
stages_fbhc <-
  function(object = NULL,
           score = function(x) {
             return(-BIC(x))
           },
           max_iter = Inf,
           scope = NULL,
           ignore = object$name_unobserved,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted_sevt(object))
    if (is.null(scope)){
      scope <- sevt_varnames(object)[-1]
    }
    stopifnot(all(scope %in% sevt_varnames(object)[-1]))
    now_score <- score(object)
    for (v in scope) {
      iter <- 0
      r <- 1
      done <- FALSE
      while (!done && iter < max_iter) {
        iter <- iter + 1
        temp <- object # clone the object
        temp_score <- now_score # clone the score
        s1_select <- NULL
        s2_select <- NULL
        done <- TRUE
        stages <- unique(object$stages[[v]])
        stages <- stages[!(stages %in% ignore)]
        if (length(stages) > 1) {
          for (i in 2:length(stages)) {
            ## try all stages pair
            s1 <- stages[i]
            for (j in 1:(i - 1)) {
              s2 <- stages[j]
              try <-
                join_stages(object, v, s1, s2) ## join the 2 stages
              try_score <- score(try)
              if (try_score >= temp_score) {
                temp <- try
                temp_score <- try_score
                s1_select <- s1 # just to message it if verbose
                s2_select <- s2 # just to message it if verose
                done <- FALSE
                break
              }
            }
            if (!done) {
              break
            }
          }
        } ## end if there are more than 1 stage
        object <- temp
        now_score <- temp_score
        if ((trace > 1) && !done) {
          message(
            v, " joined stages: ",
            s1_select, " and ", s2_select
          )
        }
      } ## end while
      if (trace > 0) {
        message(
          "fast HC over ",
          v,
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


#' Backward joining of stages
#'
#' Join stages from more complex to simpler models
#' using a distance and a threshold value.
#'
#' @param object the staged event tree from where to start.
#' @param distance string, see details.
#' @param thr the threshold for joining stages
#' @param scope names of variables that should be considered 
#'              for the optimization.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info.
#' is printed (via \code{message})
#'
#' @details For each variable in the model stages are joined iteratively.
#' At each iteration the two stages with minimum distance are selected and
#' joined if their distance is less than \code{thr}.
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(n = 5, N = 1000)
#' model <- stages_bj(full(DD, lambda = 1), trace = 2)
#' summary(model)
#' @importFrom  methods is
#' @export
stages_bj <-
  function(object = NULL,
           distance = "kl",
           thr = 0.1,
           scope = NULL,
           ignore = object$name_unobserved,
           trace = 0) {
    stopifnot(is(object, "sevt"))
    stopifnot(is_fitted_sevt(object))
    stopifnot(is(distance, "character"))
    stopifnot(distance %in% c("l1", "l2", "ry", "kl", "tv", "hl",
                              "bh", "cd"))
    dist_fun <- switch(distance, 
                       l1 = probdist.l1,
                       l2 = probdist.l2,
                       ry = probdist.ry,
                       kl = probdist.kl,
                       tv = probdist.tv,
                       hl = probdist.hl,
                       bh = probdist.bh,
                       cd = probdist.cd,
                       )
    if (is.null(scope)){
      scope <- sevt_varnames(object)[-1]
    }
    stopifnot(all(scope %in% sevt_varnames(object)[-1]))
    for (v in scope) {
      finish <- FALSE
      while (!finish) {
        finish <- TRUE
        stages <- unique(object$stages[[v]])
        stages <- stages[!(stages %in% ignore)]
        if (length(stages) > 1) {
          M <- as.matrix(distance_mat_stages(object$prob[[v]][stages], dist_fun))
          diag(M) <- Inf
          idx <- which.min(M)
          i <- ceiling(idx / dim(M)[1])
          j <- idx - (i - 1) * dim(M)[1]
          s1 <- stages[i]
          s2 <- stages[j]
          if (abs(M[i, j]) < thr) {
            object <-
              join_stages(object, v, s1, s2) ## join the 2 stages
            finish <- FALSE # if joined the stages we are not finish
            if (trace > 1) {
              message(
                v, " joined stages: ", s1,
                " and ", s2
              )
            }
          }
        } ## end if there are more than 1 stage
      } ## end while
      if (trace > 0) {
        message("backward join over ", v, " done")
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
#' @param object a staged event tree object.
#' @param score a function that score staged event tree objects.
#' @param max_iter the maximum number of iterations per variable.
#' @param scope names of variables that should be considered for the optimization
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace integer, if positive information on the progress is
#'              printed to console
#'
#' @details For each variable node-moves that best increase the
#' score are performed until no increase is possible. 
#' A node-move is either changing the stage
#' associate to a node or move the node to a new stage.
#' 
#' The `ignore` argument can be used to specify stages that should not 
#' be affected during the search, that is left untouched. 
#' This is usefull for preserving structural zeroes and to speed-up 
#' computations. 
#'
#' @return The final staged event tree obtained.
#'
#' @examples
#' model <- stages_hc(full(PhDArticles[, 1:3], lambda = 1))
#' summary(model)
#' 
#' ## preserve zero stages
#' start <- indep(PhDArticles[,1:5], join_unobserved = TRUE)
#' model <- stages_hc(start)
#' @export
stages_hc <- function(object,
                    score = function(x) {
                      return(-BIC(x))
                    },
                    max_iter = Inf,
                    scope = NULL,
                    ignore = object$name_unobserved,
                    trace = 0) {
  stopifnot(is(object, "sevt"))
  stopifnot(is_fitted_sevt(object))
  stopifnot(!is.null(object$ctables))
  if (is.null(scope)){
    scope <- sevt_varnames(object)[-1]
  }
  stopifnot(all(scope %in% sevt_varnames(object)[-1]))
  now_score <- score(object)
  for (v in scope) {
    done <- FALSE
    iter <- 0
    while (!done & iter < max_iter) {
      iter <- iter + 1
      temp <- object # clone the object
      temp_score <- now_score # clone the score
      stages <- object$stages[[v]]
      ustages <- unique(stages)
      newname <- new_label(c(ustages, ignore))
      ustages <- ustages[!(ustages %in% ignore)]
      done <- TRUE
      for (j in seq_along(ustages)) {
        s1 <- ustages[j]
        idx <- (seq_along(stages))[stages == s1]
        for (i in idx) {
          try <- object
          for (s2 in c(ustages[-j], newname)) {
            try$stages[[v]][i] <- s2
            try <- sevt_fit(try, lambda = object$lambda)
            try_score <- score(try)
            if (try_score > temp_score) {
              temp <- try
              temp_score <- try_score
              ia <- i # just to message it if verbose
              s1a <- s1
              s2a <- s2
              done <- FALSE
            }
          }
        }
      } ## end for over stages
      object <- temp
      now_score <- temp_score
      if ((trace > 1) && !done) {
        message(
          v, " moved ", ia, " from stage ", s1a, " to stage ",
          s2a
        )
      }
    } ## end while
    if (trace > 0) {
      message(v, " HC done")
    }
  } ## end for over variables
  if (trace > 0) {
    message(
      "HC over ",
      v,
      " done after ",
      iter,
      " iterations."
    )
  }
  object$call <- sys.call()
  return(object)
}

#' Learn a staged tree with hierarchical clustering
#' 
#' Build a stage event tree with \code{k} stages for each variable by
#' clustering stage probabilities with hierarchical clustering.
#' @param object a staged event tree object.
#' @param distance string, the distance measure to be used, either 
#'                 a possible `method` for \code{\link{dist}} or 
#'                 one of the following: \code{"totvar", "hellinger"}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param k integer or (named) vector: number of clusters, that is stages per variable. 
#'        Values will be recycled if needed. 
#' @param method the agglomeration method to be used in \code{\link{hclust}}.
#' @param limit the maximum number of variables to consider.
#' @param scope names of the variables to consider.
#' @details \code{hclust_sevt} performs hierarchical clustering 
#'          of the initial stage probabilities in \code{object} 
#'          and it aggregates them into the specified number
#'          of stages (\code{k}).
#'          A different number of stages for the different variables 
#'          in the model can be specified by supplying a (named) vector 
#'          via the argument \code{k}.
#' @return A staged event tree object.
#' @importFrom stats dist hclust cutree
#' @examples 
#' data("Titanic")
#' model <- stages_hclust(full(Titanic, join_unobserved = TRUE, lambda = 1), k = 2)
#' summary(model)
#' @export
stages_hclust <-
  function(object,
           distance = "totvar",
           k = length(object$tree[[1]]),
           method = "complete",
           ignore = object$name_unobserved,
           limit = length(object$tree),
           scope = NULL) {
    stopifnot(is_fitted_sevt(object))
    if (is.null(scope)) scope <- sevt_varnames(object)[2:limit]
    stopifnot(all(scope %in% sevt_varnames(object)[-1]))
    if (is.null(names(k))){
      k <- rep(k, length(scope))[seq_along(scope)]
      names(k) <- scope
    }
    for (v in scope) {
      wch <- names(object$prob[[v]])
      wch <- wch[!(wch %in% ignore)]
      pp <- t(as.matrix(as.data.frame(object$prob[[v]][wch])))
      rownames(pp) <- wch
      M <- switch(distance, 
                  "totvar" = 0.5*dist(pp, method = "manhattan"),
                  "hellinger" = dist(sqrt(pp), method = "euclidean") / sqrt(2),
                  dist(pp, method = distance))
      groups <- cutree(hclust(M, method = method), k = min(k[v], attr(M, "Size")))
      ### remove probabilitites and assign stages
      object$prob[[v]] <- list()
      old <- object$stages[[v]]
      for (s in 1:k[v]) {
        object$stages[[v]][old %in% names(which(groups == s))] <- paste0(s)
      }
    }
    object$call <- sys.call()
    return(sevt_fit(object, lambda = object$lambda))
  }

#' Learn a staged tree with k-means clustering
#' 
#' Build a stage event tree with \code{k} stages for each variable
#' by clustering (transformed) probabilities with k-means. 
#' @param object a staged event tree object.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param k integer or (named) vector: number of clusters, that is stages per variable. 
#'          Values will be recycled if needed.
#' @param algorithm character: as in \code{\link{kmeans}}.
#' @param transform function applied to the probabilities before clustering.
#' @param limit the maximum number of variables to consider.
#' @param scope names of the variables to consider.
#' @param nstart as in \code{\link{kmeans}}
#' @details \code{kmenas_sevt} performs k-means clustering 
#' to aggregate the stage probabilities of the initial 
#' staged tree \code{object}. 
#' Different values for k can be specified by supplying a 
#' (named) vector to \code{k}. 
#' \code{\link{kmeans}} from the \code{stats} package is used
#' internally and arguments \code{algorithm} and \code{nstart} 
#' refer to the same arguments as \code{\link{kmeans}}. 
#' @return A staged event tree.
#' @importFrom stats kmeans
#' @examples 
#' data("Titanic")
#' model <- stages_kmeans(full(Titanic, join_unobserved = TRUE, lambda = 1), k = 2)
#' summary(model)
#' @export
stages_kmeans <- function(object,
                        k = length(object$tree[[1]]),
                        algorithm = "Hartigan-Wong",
                        transform = sqrt,
                        ignore = object$name_unobserved,
                        limit = length(object$tree),
                        scope = NULL,
                        nstart = 1){
  stopifnot(is_fitted_sevt(object))
  stopifnot(is.function(transform) || is.null(transform))
  if (is.null(transform)) transform <- function(x) return(x)
  if (is.null(scope)) scope <- sevt_varnames(object)[2:limit]
  stopifnot(all(scope %in% sevt_varnames(object)[-1]))
  if (is.null(names(k))){
    k <- rep(k, length(scope))[seq_along(scope)]
    names(k) <- scope
  }
  for (v in scope) {
    wch <- names(object$prob[[v]])
    wch <- wch[!(wch %in% ignore)]
    pp <- transform(t(as.matrix(as.data.frame(object$prob[[v]][wch]))))
    rownames(pp) <- wch
    if (nrow(pp) > k[v]){
      groups <- kmeans(pp, centers = min(k[v], nrow(pp) - 1), 
                       algorithm = algorithm, nstart = nstart)$cluster
      ### remove probabilitites and assign stages
      object$prob[[v]] <- list()
      old <- object$stages[[v]]
      for (s in 1:k[v]) {
        object$stages[[v]][old %in% names(which(groups == s))] <- paste0(s)
      }
    }
  }
  object$call <- sys.call()
  return(sevt_fit(object, lambda = object$lambda))
}
