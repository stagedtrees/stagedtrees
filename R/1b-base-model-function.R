#' Coerce to sevt
#' 
#' Convert to an equivalent object of class \code{\link{sevt}}.
#' 
#' @param x   an R object.
#' @param ... additional parmaeters to be used by specific methods.
#' @return the equivalent object of class \code{\link{sevt}}.
#' @export
as_sevt <- function(x, order = NULL, ...){
  UseMethod("as_sevt", x)
} 

#' @rdname as_sevt
#' @param order order of the variables.
#' @details In \code{as_sevt.bn.fit} the \code{order} 
#' argument, if provided, must be a topological order of the 
#' \code{bn.fit} object (no check is performed). If the order is not provided 
#' a topological order will be used (the one returned by 
#' \code{bnlearn::node.ordering}).
#' @details A method for objects of class \code{bn.fit} 
#'          (\code{bnlearn} package).
#' @export
as_sevt.bn.fit <- function(x, order = NULL, ...) {
  bn <- bnlearn::bn.net(x)
  # build the ordered list of levels
  tree <- lapply(
    x,
    function(tt) {
      if (length(tt$parents) == 0) {
        names(tt$prob)
      } else {
        rownames(tt$prob)
      }
    }
  )
  # if no order is provided from the user
  # then a topological order is used
  if (is.null(order)){
    order <- bnlearn::node.ordering(bn)
  }
  # order the list of levels
  tree <- tree[order]
  # create staged tree from list
  object <- sevt(tree)
  # extract parents
  parents <- lapply(bn$nodes[order], function(n) {
    if (identical(n$parents, character(0))) {
      return(NULL)
    } else {
      return(n$parents)
    }
  })
  # build stages info respecting conditional 
  # independences depicted in the bayesian network
  for (i in 2:length(order)) {
    if (i <= 2) {
      if (order[i - 1] %in% parents[[i]]) {
        object$stages[[i - 1]] <- seq_along(tree[[i - 1]])
      }
    } else {
      grid <- expand.grid(tree[(i - 1):1])[, (i - 1):1]
      grid$stages <- 1:nrow(grid)
      if (length(parents[[i]]) > 0) {
        ind <- match(parents[[i]], colnames(grid))
        grid <- data.frame(grid[, c(ind, ncol(grid))])
        colnames(grid) <- c(parents[[i]], "stages")
        for (j in 1:(nrow(grid) - 1))
        {
          for (k in (j + 1):(nrow(grid)))
          {
            if (sum(grid[j, -ncol(grid)] == grid[k, -ncol(grid)]) ==
              length(grid[j, -ncol(grid)])) {
              grid$stages[k] <- grid$stages[j]
            }
          }
        }
        values <- unique(grid$stages)
        unique_values <- seq_along(values)
        for (l in seq_along(values))
        {
          for (m in seq_along(grid$stages))
          {
            if (grid$stages[m] == values[l]) {
              grid$stages[m] <- unique_values[l]
            }
          }
        }
      } else if (length(parents[[i]]) == 0) {
        grid$stages <- rep(1, nrow(grid))
      }
      object$stages[[i - 1]] <- grid$stages
    }
  }
  object$stages <- lapply(object$stages, as.character)
  return(object)
}


#' Fit a staged event tree
#'
#' Estimate transition probabilities in a staged event tree from data.
#' Probabilities are estimated with the relative frequencies plus,
#' eventually, an additive (Laplace) smoothing.
#' @param object an object of class \code{sevt}.
#' @param data data.frame or contingency table with observations of 
#'             the variables in \code{object}.
#' @param lambda smoothing parameter or pseudocount.
#' @return A fitted staged event tree, that is an object of class `sevt`
#'         with `ctables`, `prob` and `ll` components.
#' @details The data in form of contingency tables and the 
#'          log-likelihood of the model is 
#'          stored in the returned staged event tree.
#' @export
#' @examples
#'
#' #########
#' model <- sevt(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
#' D <- data.frame(
#'   X = c("good", "good", "bad"),
#'   Y = c("high", "low", "low")
#' )
#' model.fit <- sevt_fit(model, data = D, lambda = 1)
sevt_fit <- function(object,
                     data = NULL,
                     lambda = object$lambda) {
  if (is.null(data)) {
    if (!has_ctables(object)) {
      warning("Data must be provided or included in the model object as ctbales")
      return(object)
    }
  }else{
    object$ctables <- make_ctables(object, data)
  }
  if (is.null(lambda)){
    warning("lambda not provided nor available in the sevt object, lambda = 0 used")
    lambda <- 0
  }
  # store lambda 
  object$lambda <- lambda
  # extract order of variables 
  order <- names(object$tree)
  # 
  dims <- vapply(object$tree, length, FUN.VALUE = 1)
  object$prob <- list()
  n <- sum(object$ctables[[order[1]]])
  pp <- object$ctables[[order[1]]] + lambda
  pp <- pp / sum(pp)
  attr(pp, "n") <- n
  object$prob[[order[1]]] <- list("1" = pp)
  for (i in 2:length(order)) {
    stages <- unique(object$stages[[order[i]]])
    object$prob[[order[i]]] <-
      lapply(stages, function(s) {
        ix <- object$stages[[order[i]]] == s
        if (sum(ix) > 1) {
          tt <- apply(object$ctables[[order[i]]][ix, ], MARGIN = 2, sum)
        } else {
          tt <- object$ctables[[order[i]]][ix, ]
        }

        names(tt) <- object$tree[[order[i]]]
        n <- sum(tt) ## compute sample size
        tt <- (tt + lambda) ## smoothing
        tt <- tt / sum(tt) ## normalize
        tt[is.nan(tt)] <- NA  ## replace NaN with NA
        attr(tt, "n") <- n ## save sample size
        
        return(tt) # return normalized prob
      })
    names(object$prob[[order[i]]]) <- stages
  }
  object$ll <- NULL ## force recompute log-likelihood
  object$ll <- logLik(object)
  return(object)
}




#' Set stage to path
#'
#' @param object an object of class \code{sevt}.
#' @param path Vector of the path.
#' @param stage stage to be assigned.
#' @keywords internal
set_stage <- function(object, path, stage) {
  stage <- as.character(stage)
  warning("NOT YET IMPLEMENTED")
  ## TO DO
  return(object)
}

#' Join stages
#'
#' Join two stages in a staged event tree object, updating
#' probabilities and log-likelihood accordingly.
#'
#' @param object an object of class \code{sevt}.
#' @param v variable.
#' @param s1 first stage.
#' @param s2 second stage.
#' @return the staged event tree where \code{s1} and \code{s2} are joined.
#' @details This function joins two stages associated to the 
#'          same variable, 
#'          updating probabilities and log-likelihood if 
#'          the object was fitted.
#' @examples
#' model <- full(PhDArticles, lambda = 0)
#' model <- stages_fbhc(model)
#' model$stages$Kids
#' model <- join_stages(model, "Kids", "5", "6")
#' model$stages$Kids
#' @export
join_stages <- function(object, v, s1, s2) {
  check_sevt(object)
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  if (!all(c(s1, s2) %in% stages(object, var = v))) {
    stop("Stages are not present")
  }
  if (s1 == s2) stop("Join the same stage")
  k <- length(object$tree[[v]])
  st <- object$stages[[v]]
  object$stages[[v]][st == s2] <- s1
  if (!is.null(object$prob)) {
    p1 <- object$prob[[v]][[s1]]
    p2 <- object$prob[[v]][[s2]]
    n2 <- attr(p2, "n")
    n1 <- attr(p1, "n")
    if (is.null(n1)) n1 <- 1
    if (is.null(n2)) n2 <- 1
    if (is.null(object$lambda)){
      object$lambda <- 0
    }
    ct1 <-
      ifelse(is.na(p1), 0, p1) * (n1 + object$lambda * k) - object$lambda
    ct2 <-
      ifelse(is.na(p2), 0, p2) * (n2 + object$lambda * k) - object$lambda
    dll <-
      sum(ct2[ct2 > 0] * log(p2[ct2 > 0])) +
      sum(ct1[ct1 > 0] * log(p1[ct1 > 0]))
    object$prob[[v]][[s1]] <- ct2 + ct1 + object$lambda
    attr(object$prob[[v]][[s1]], "n") <- n1 + n2
    object$prob[[v]][[s1]] <-
      object$prob[[v]][[s1]] / sum(object$prob[[v]][[s1]])
    object$prob[[v]][[s2]] <- NULL ## delete one of the two
    if (!is.null(object$ll)) {
      ## update log likelihood
      ct1 <- ct1 + ct2
      object$ll <-
        object$ll - dll + sum(ct1[ct1 > 0] *
          log(object$prob[[v]][[s1]][ct1 > 0]))
      attr(object$ll, "df") <-
        attr(object$ll, "df") - length(object$prob[[v]][[s1]]) + 1
    }
  }
  return(object)
}


#' Split randomly a stage
#'
#' Randomly assign some of the paths to a new stage.
#' @param object an object of class \code{sevt}.
#' @param var the variable name.
#' @param stage the name of the stage.
#' @param p probability to move a situation from the 
#'          original stage into the new stage.
#'
#' @return an object of class \code{sevt}.
#' @details Splits randomly a given stage into two stages. More precisely,
#' it assigns each situation within the given stage into a new stage with
#' probability \code{p}.
#' @keywords internal
split_stage_random <- function(object, var, stage, p = 0.5) {
  check_sevt(object)
  # if the given stage is not present 
  if (!(stage %in% object$stages[[var]])) {
    # return the same object
    return(object)
  }
  # obtain size of stages for given variable
  d <- length(object$stages[[var]])
  # get a new label 
  label <- new_label(object$stages[[var]])
  # find where stage should be changed
  # changes should happen with probability p 
  # and only for the given stage
  ix <-
    (object$stages[[var]] == stage) &
      sample(
        x = c(TRUE, FALSE),
        size = d,
        prob = c(p, 1 - p),
        replace = TRUE
      )
  # if there is a change to do 
  if (any(ix)) {
    # set the new label 
    object$stages[[var]][ix] <- label
    if (is_fitted_sevt(object)) {
      # re-fit the model
      object <- sevt_fit(object, lambda = object$lambda)
    }
  }
  return(object)
}





#' Inclusions of stages
#'  
#' @description Display the relationship between two staged tree models over the 
#' same variables.
#' @param object1 an object of class \code{sevt}.
#' @param object2 an object of class \code{sevt}.
#' @return a list with inclusion relations between stage
#' structures for each variables in the models.
#' @details Computes the 
#'  inclusion/exclusion/equality/diversity between 
#'  the stages structures of the two models.
#' @examples
#' mod1 <- stages_bhc(full(PhDArticles[, 1:5], lambda = 1))
#' mod2 <- stages_fbhc(full(PhDArticles[, 1:5], lambda = 1))
#' inclusions_stages(mod1, mod2)
#' @export
inclusions_stages <- function(object1, object2) {
  check_sevt(object1)
  check_sevt(object2)
  stopifnot(sevt_nvar(object1) == sevt_nvar(object2))
  stopifnot(all(sevt_varnames(object1) == sevt_varnames(object2)))
  out <- rep(list(c()), length(object1$stages))
  attr(out, "names") <- attr(object1$stages, "names")
  out2 <- out

  for (k in seq_along(object1$stages)) {
    a <- object1$stages[[k]]
    b <- object2$stages[[k]]
    unique_a <- unique(a)
    unique_b <- unique(b)
    out_a <- out_b <- rep(0, length(a))
    for (i in seq_along(unique_a)) {
      ifelse((length(unique(b[which(a == unique_a[i])])) == 1),
        out_a[which(a == unique_a[i])] <- 1,
        out_a[which(a == unique_a[i])] <- 0
      )
    }
    for (i in seq_along(unique_b)) {
      ifelse((length(unique(a[which(b == unique_b[i])])) == 1),
        out_b[which(b == unique_b[i])] <- 1,
        out_b[which(b == unique_b[i])] <- 0
      )
    }

    out[[k]] <- ifelse((out_a + out_b) == 2, 0, 1)
    out2[[k]] <- data.frame("A" = rep(NA, length(out[[k]])),
                            "B" =  rep(NA, length(out[[k]])),
                            "C" = rep(NA, length(out[[k]])))

    ord1 <- ord2 <- c()

    for (i in seq_along(out[[k]])) {
      out2[[k]][i, 1] <- object1$stages[[k]][i]
      out2[[k]][i, 3] <- object2$stages[[k]][i]
      if (out[[k]][i] == 0) {
        out2[[k]][i, 2] <- "="
        ord1 <- c(ord1, object1$stages[[k]][i])
        ord2 <- c(ord2, object2$stages[[k]][i])
      }
      else if (out[[k]][i] == 1) {
        if (out_a[i] == 1 & out_b[i] == 0) {
          out2[[k]][i, 2] <- "<"
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 1) {
          out2[[k]][i, 2] <- ">"
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 0) {
          out2[[k]][i, 2] <- "!="
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
      }
    }

    # nice print
    out2[[k]] <- unique(noquote(out2[[k]]))
    colnames(out2[[k]]) <- c(deparse(substitute(object1)), " - ", deparse(substitute(object2)))
    out2[[k]] <- format(out2[[k]], justify = "centre")
  }
  return(out2)
}




#' Print a staged event tree
#'
#' @param x an object of class \code{sevt}.
#' @param ... additional parameters (compatibility).
#'
#' @return An invisible copy of \code{x}.
#' @details The order of the variables in the staged tree
#'  is printed (from root). In addition the number of levels of each
#'  variable is shown in square brackets.
#'  If available the log-likelihood of the model is printed.
#' @export
#' @examples
#' DD <- generate_xor_dataset(5, 100)
#' model <- full(DD, lambda  = 1)
#' print(model)
print.sevt <- function(x, ...) {
  cat(
    "Staged event tree",
    ifelse(is_fitted_sevt(x), "(fitted) \n", "\n")
  )
  ls <- vapply(x$tree, length, 1)
  cat(paste(paste0(names(x$tree), "[", ls, "] "), collapse = "-> "), "\n")
  if (!is.null(x$ll)) {
    print(x$ll)
  }
  invisible(x)
}



#' Stages of a variable
#'
#' Obtain the stages of a given variable in a staged event tree object.
#' @param object an object of class \code{sevt}.
#' @param var name of one variable in \code{object}.
#' @return If \code{var} is specified, it returns a character vector with
#'         stage names for the given variable
#'         (that is \code{object$stages[[var]]}.
#'         Otherwise, If \code{var} is not specified, \code{stages}
#'         returns a list of character vectors containing the stages associated
#'         to each variable in the model (that is \code{object$stages}).
#' @export
stages <- function(object, var = NULL) {
  check_sevt(object)
  if (is.null(var)) {
    object$stages
  } else {
    object$stages[[var]]
  }
}


#' Summarizing staged event trees
#'
#' Summary method for class \code{sevt}.
#'
#' @param object an object of class \code{sevt}.
#' @param ... arguments for compatibility.
#' @details Print model information and summary of stages.
#' @return An object of class \code{summary.sevt}
#'         for which a \code{print}
#'         method exist.
#' @examples
#' model <- stages_fbhc(full(PhDArticles, lambda = 1))
#' summary(model)
#' @export
summary.sevt <- function(object, ...) {
  check_sevt(object)
  vns <- sevt_varnames(object)
  nv <- sevt_nvar(object)
  out <- list()
  out[[vns[1]]] <- data.frame(
    stage = "1",
    npaths = 0, stringsAsFactors = FALSE
  )
  if (is_fitted_sevt(object)) {
    out[[vns[1]]][["sample.size"]] <- attr(object$prob[[vns[1]]][[1]], "n")
    out[[vns[1]]] <- cbind(
      out[[vns[1]]],
      t(as.data.frame(object$prob[[vns[1]]]))
    )
  }
  for (i in 2:nv) {
    v <- vns[i]
    D <- data.frame(
      stage = unique(object$stages[[v]]),
      stringsAsFactors = FALSE,
      row.names = make.names(unique(object$stages[[v]]))
    )
    D$npaths <- vapply(D$stage, function(s) {
      sum(object$stages[[v]] == s)
    }, FUN.VALUE = 1)
    if (is_fitted_sevt(object)) {
      D[["sample.size"]] <- vapply(D$stage, function(s) {
        attr(object$prob[[v]][[s]], "n")
      }, FUN.VALUE = 1)
      if (nrow(D) <= 1){
        D <- cbind(D, t(as.data.frame(object$prob[[v]])))
      }else{
        D <- cbind(D, t(as.data.frame(object$prob[[v]]))[rownames(D),])
      }
    }
    out[[vns[i]]] <- D
  }
  out <- list(stages.info = out)
  out$call <- object$call
  out$ll <- object$ll
  out$lambda <- object$lambda
  class(out) <- "summary.sevt"
  return(out)
}

#' @rdname summary.sevt
#' @param x an object of class \code{summary.sevt}.
#' @param max the maximum number of variables for which
#'            information is printed.
#' @export
print.summary.sevt <- function(x, max = 10, ...) {
  if (!is.null(x$call)) {
    cat("Call: \n")
    print(x$call)
  }
  if (!is.null(x$lambda)) cat("lambda: ", x$lambda, "\n")
  cat("Stages: \n")
  for (i in 1:min(length(x$stages.info), max)) {
    cat("  Variable: ", names(x$stages.info)[i], "\n")
    print.data.frame(x$stages.info[[i]], row.names = FALSE)
    cat("  ------------ \n")
  }
  if (max < length(x$stages.info)) {
    cat("  only the first ", max, " variables are shown \n")
  }
}


#' Extract subtree
#'
#' @param object an object of class \code{sevt}.
#' @param path the path from root after which extract the subtree.
#' @details Returns the subtree of the staged event tree, starting from 
#' \code{path}.
#' @return A staged event tree object corresponding to the subtree.
#' @examples
#' DD <- generate_random_dataset(4, 100)
#' model <- sevt(DD, full = TRUE)
#' plot(model)
#' model1 <- subtree(model, path = c("-1", "1"))
#' plot(model1)
#' @export
subtree <- function(object, path) {
  m <- 1
  idx <- tree_idx(path, object$tree)
  stage <- find_stage(object, path)
  varout <- sevt_varnames(object)[seq_along(path)]
  object$tree[varout] <- NULL ## remove previous variables
  object$stages[varout] <- NULL ## remove stages info
  object$ctables[varout] <- NULL
  var <- sevt_varnames(object)
  object$stages[[var[1]]] <-
    c(stage) ## keep stage name for first variable
  if (is_fitted_sevt(object)) {
    object$ctables[[var[1]]] <- object$ctables[[var[1]]][idx, ]
    attr(object$ctables[[var[1]]], "names") <- object$tree[[var[1]]]
  }
  for (i in 2:length(object$tree)) {
    m <- m * length(object$tree[[var[i - 1]]])
    tmpidx <- ((idx - 1) * m):(idx * m - 1) + 1
    object$stages[[var[i]]] <-
      object$stages[[var[i]]][tmpidx]
    if (is_fitted_sevt(object)) { # update ctables
      object$ctables[[var[i]]] <- ftable(object$ctables[[var[i]]][tmpidx, ])
      attr(object$ctables[[var[i]]], "row.vars") <- object$tree[1:(i - 1)]
      attr(object$ctables[[var[i]]], "col.vars") <- object$tree[i]
    }
  }
  if (is_fitted_sevt(object)) {
    object$prob[varout] <- NULL
    object$prob[[var[1]]] <- object$prob[[var[1]]][stage]
    for (i in 2:length(object$tree)) {
      object$prob[[var[i]]] <-
        object$prob[[var[i]]][unique(object$stages[[var[i]]])]
    }
  }
  object$ll <- NULL
  return(object)
}


#'  Standard renaming of stages
#'
#' Rename all stages in a staged event tree.
#' @param object an object of class \code{sevt}.
#' @param uniq logical, if stage numbers should be unique over all tree.
#' @param prefix logical, if stage names should be prefixed with variable name.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @return a staged event tree object with stages named with
#' consecutive integers.
#' @examples
#' model <- stages_fbhc(full(PhDArticles, join_unobserved = TRUE))
#' model$stages
#' model1 <- stndnaming(model)
#' model1$stages
#' 
#' ### unique stage names in all tree
#' model2 <- stndnaming(model, uniq = TRUE)
#' model2$stages
#' 
#' ### prefix stage names with variable name 
#' model3 <- stndnaming(model, prefix = TRUE)
#' model3$stages
#' 
#' ### manuallty select stage names left untouched
#' model4 <- stndnaming(model, ignore = c("2", "6"), prefix = TRUE)
#' model4$stages
#' @export
stndnaming <- function(object, uniq = FALSE, 
                            prefix = FALSE, 
                       ignore = object$name_unobserved) {
  var <- names(object$tree)
  start <- 0
  for (i in 2:length(var)) {
    v <- var[i]
    old <- unique(object$stages[[v]])
    old <- old[!(old %in% ignore)]
    if (length(old) > 0){
      new <- as.character(start + (seq_along(old)))
      if (prefix) new <- paste0(v, new)
      if (uniq) start <- start + length(old)
      object$stages[[v]] <- vapply(object$stages[[v]], function(s) {
        if (s %in% ignore) return(s)
        new[which(old %in% s, useNames = FALSE)]
      }, FUN.VALUE = "a", USE.NAMES = FALSE)
      if (is_fitted_sevt(object)) {
        object$prob[[v]][new] <- object$prob[[v]][old]
        object$prob[[v]][old[!(old %in% new)]] <-
          NULL ## remove old prob
      } 
    }
  }
  if (is_fitted_sevt(object)) {
    object$prob[[var[1]]] <- list("1" = object$prob[[var[1]]][[1]])
  }
  return(object)
}

#' Compare two staged event tree
#'
#' Compare two staged event trees, return the differences of the stages
#' structure and plot the difference tree. Three different methods to
#' compute the difference tree are available (see Details).
#' @param object1 an object of class \code{sevt}.
#' @param object2 an object of class \code{sevt}.
#' @param method character, method to compare staged event trees. 
#' One of: \code{"naive"}, 
#' \code{"hamming"} or \code{"stages"}.
#' @param return_tree logical, if \code{TRUE} the difference tree is returned.
#' @param plot logical.
#' @param ... additional parameters to be passed to \code{\link{plot.sevt}}.
#' @details \code{compare_stages} tests if the stage structure of two \code{sevt}
#' objects
#' is the same.
#' Three methods are available:
#' * \code{naive} first applies \code{\link{stndnaming}} to both
#' objects and then simply compares the resulting stage names.
#' * \code{hamming} uses the \code{hamming_stages} function that finds
#' a minimal subset of nodes which stages 
#' must be changed to obtain the same structure.
#' * \code{stages} uses the \code{diff_stages} function that compares
#' stages to check whether the same stage structure is present in both models.
#'
#' Setting \code{return_tree = TRUE} will return the stages
#' difference obtained with the selected method.
#' The stages difference is a list of numerical vectors with same 
#' lengths and structure as \code{stages(object1)} or \code{stages(object2)}, 
#' where values are 1 if the corresponding node has different 
#' (with respect to the selected \code{method}) associated stage, and
#' 0 otherwise. 
#'
#' With \code{plot = TRUE} the plot of the difference tree is displayed.
#'
#' If \code{return_tree = FALSE} and \code{plot = FALSE} 
#' the logical output is the same for the
#' three methods and thus the \code{naive} method should be used
#' since it is computationally faster.
#' @return 
#' \code{compare_stages}: if \code{return_tree = FALSE}, logical: \code{TRUE} if the two
#' models are exactly equal, otherwise \code{FALSE}.
#' Else if \code{return_tree = TRUE}, the differences between
#' the two trees, according to the selected \code{method}.
#' @export
#' @examples
#' data("Asym")
#' mod1 <- stages_bhc(full(Asym, lambda = 1))
#' mod2 <- stages_fbhc(full(Asym, lambda = 1))
#' compare_stages(mod1, mod2)
compare_stages <-
  function(object1,
           object2,
           method = "naive",
           return_tree = FALSE,
           plot = FALSE,
           ...) {
    # check and rename stages
    check_sevt(object1)
    check_sevt(object2)
    stopifnot(sevt_nvar(object1) == sevt_nvar(object2))
    stopifnot(all(names(object1$tree) == names(object2$tree)))
    object1 <- stndnaming(object1)
    object2 <- stndnaming(object2)
    # use the appropriate method
    difftree <- switch(
      method,
      naive = sapply(names(object1$tree)[-1],
                     function(v) {
                       as.numeric(object1$stages[[v]] != object2$stages[[v]])
                     },
        USE.NAMES = TRUE
      ),
      hamming = hamming_stages(object1, object2, return_tree = TRUE),
      stages = diff_stages(object1, object2),
      sapply(names(object1$tree)[-1],
        function(v) {
          as.numeric(object1$stages[[v]] != object2$stages[[v]])
        },
        USE.NAMES = TRUE
      )
    )
    # root is always ok
    tmp <- list()
    tmp[[names(object1$tree)[1]]] <- 0
    difftree <- c(tmp, difftree)
    # plot if required
    if (plot) {
      toplot <- list(tree = object1$tree)
      class(toplot) <- "sevt"
      toplot$stages <- difftree
      for (v in names(toplot$tree)[-1]){
        toplot$stages[[v]][object1$stages[[v]] %in% object1$name_unobserved & 
                             object2$stages[[v]] %in% object2$name_unobserved] <- "UNOBSERVED"
      }
      toplot$name_unobserved <- "UNOBSERVED"
      plot(
        toplot,
        col = function(x) {
          c("1" = "red",
          "0" = 0)
        },
        pch = 16,
        ...
      )
    }
    if (return_tree) {
      return(difftree)
    } else {
      return(all(sapply(difftree, function(x) {
        all(x == 0)
      })))
    }
  }


#' @rdname compare_stages
#' @details 
#' \code{hamming_stages} finds a minimal set of nodes for which the associated stages
#' should be changed to obtain equivalent structures. To do that, a maximum-weight bipartite 
#' matching problem between the stages of the two staged trees is solved using the 
#' Hungarian method implemented in the \code{solve_LSAP} function of the \code{clue}
#' package. 
#' \code{hamming_stages} requires the package \code{clue}.
#' @return \code{hamming_stages}: if \code{return_tree = FALSE}, integer, the minimum
#' number of situations where the stage should be changed to obtain the same 
#' models. If \code{return_tree = TRUE} a stages-like structure showing which 
#' situations should be modified to obtain the same models.
#' @export
hamming_stages <- function(object1, object2, return_tree = FALSE) {
  check_sevt(object1)
  check_sevt(object2)
  # check if models are over the same variables, and same order
  stopifnot(sevt_nvar(object1) == sevt_nvar(object2))
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("Package \"clue\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # rename stages with increasing integers
  object1 <- stndnaming(object1)
  object2 <- stndnaming(object2)
  # for each variable match stages to obtain hamming distance
  for (v in names(object1$tree)[-1]) {
    # extract stages vectors
    ss1 <- object1$stages[[v]]
    ss2 <- object2$stages[[v]]
    # and unique stages
    u1 <- unique(ss1)
    u2 <- unique(ss2)
    # build matrix describing bipartite matching problem
    M <- matrix(
      nrow = length(u1),
      ncol = length(u2),
      dimnames = list(u1, u2)
    )
    for (s1 in u1) {
      for (s2 in u2) {
        M[s1, s2] <- sum(ss1 == s1 & ss2 == s2)
      }
    }
    ## solve bipartite matching problem using function in clue package
    if (length(u1) < length(u2)) {
      res <- clue::solve_LSAP(M, maximum = TRUE)
      map <- cbind(u1[seq_along(res)], u2[res]) ## u1 -> u2
      new <- vapply(ss1, function(s) {
        map[map[, 1] == s, 2]
      }, FUN.VALUE = "a")
      object1$stages[[v]] <- new
    } else {
      res <- clue::solve_LSAP(t(M), maximum = TRUE)
      map <- cbind(u2[seq_along(res)], u1[res]) ## u2 -> u1
      new <- vapply(ss2, function(xs) {
        map[map[, 1] == xs, 2]
      }, FUN.VALUE = "a")
      object2$stages[[v]] <- new
    }
  }
  # build the tree of differences
  difftree <- sapply(names(object1$tree)[-1], function(v) {
    as.numeric(object1$stages[[v]] != object2$stages[[v]])
  }, USE.NAMES = TRUE)
  # return tree if required or simply the hamming distance
  if (return_tree) {
    return(difftree)
  } else {
    sum(sapply(difftree, function(x) {
      sum(as.numeric(x), na.rm = TRUE)
    }))
  }
}


#' @rdname compare_stages
#' @return \code{diff_stages}: a stages-like structure marking the situations belonging 
#' to stages which are not the exactly equal.
#' @export
#' @examples
#'
#' ##########
#' m0 <- full(PhDArticles[, 1:4], lambda = 0)
#' m1 <- stages_bhc(m0)
#' m2 <- stages_bj(m0, distance = "totvar", thr = 0.25)
#' diff_stages(m1, m2)
diff_stages <- function(object1, object2) {
  check_sevt(object1)
  check_sevt(object2)
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  out <- rep(list(c()), length(object1$stages))
  attr(out, "names") <- attr(object1$stages, "names")
  for (k in seq_along(object1$stages)) {
    a <- object1$stages[[k]]
    b <- object2$stages[[k]]
    unique_a <- unique(a)
    unique_b <- unique(b)
    out_a <- out_b <- rep(0, length(a))
    for (i in seq_along(unique_a)) {
      ifelse((length(unique(b[which(a == unique_a[i])])) == 1),
        out_a[which(a == unique_a[i])] <- 1,
        out_a[which(a == unique_a[i])] <- 0
      )
    }
    for (i in seq_along(unique_b)) {
      ifelse((length(unique(a[which(b == unique_b[i])])) == 1),
        out_b[which(b == unique_b[i])] <- 1,
        out_b[which(b == unique_b[i])] <- 0
      )
    }
    # stages exactly equal have sign(out_a) + sign(out_b) == 2.
    out[[k]] <- ifelse((sign(out_a) + sign(out_b)) == 2, 0, 1)
  }
  return(out)
}

#' Variable names
#'
#' Utility returning variable-names in a staged event tree
#' model.
#' @param object an object of class \code{sevt}.
#' @return A character vector.
#' @keywords internal
sevt_varnames <- function(object) {
  names(object$tree)
}


#' Number of variables
#'
#' Utility returning the number of variables 
#' in a staged event tree model.
#' @param object An object of class \code{sevt}.
#' @return integer, the number of variables.
#' @keywords internal
sevt_nvar <- function(object) {
  length(names(object$tree))
}


#' Number of parameters of a staged event tree
#'
#' Return the number of parameters of the model.
#' @param x An object of class \code{sevt}.
#' @return integer, degrees of freedom of the staged event tree.
#' @keywords internal
sevt_df <- function(x) {
  sum(c(1, vapply(
    x$stages,
    FUN = function(x) {
      length(unique(x))
    },
    FUN.VALUE = 1
  )) *
    (vapply(
      x$tree,
      FUN = length, FUN.VALUE = 1
    ) - 1))
}




#' Get stage or path
#'
#' Utility functions to obtain stages from paths and
#' paths from stages.
#'
#' @param object an object of class \code{sevt}.
#' @param path character vector, the path fromroot or
#' a two dimensional array where each row is a path
#' from root.
#' @return \code{get_stage} returns
#' the stage name(s)  for given path(s).
#' @examples
#' model <- stages_fbhc(full(PhDArticles))
#' get_stage(model, c("0", "male"))
#' paths <- expand.grid(model$tree[2:1])[, 2:1]
#' get_stage(model, paths)
#' @export
get_stage <- function(object, path) {
  check_sevt(object)
  if (is.null(object$stages)) {
    stop("object is not a staged tree (no stages found)")
  }
  if (is.null(dim(path))) {
    find_stage(object, path)
  } else {
    apply(path,
      MARGIN = 1,
      function(x) find_stage(object, x)
    )
  }
}


#' @rdname get_stage
#'
#' @param var character, one of the variable in
#'            the staged tree.
#' @param stage character vector, the name
#' of the stages for which the paths should be
#' returned.
#' @return  \code{get_path} returns a
#'         data.frame containing the paths
#'         corresponding to the given stage(s).
#' @examples
#' get_path(model, "Kids", "5")
#' get_path(model, "Gender", "2")
#' get_path(model, "Kids", c("5", "6"))
#' @export
get_path <- function(object, var, stage) {
  check_sevt(object)
  if (!var %in% names(object$tree)) {
    stop(var, " is not a variable in the model")
  }
  
  # list all paths
  paths <- expand.grid(object$tree[(which(var == sevt_varnames(object)) - 1):1],
    stringsAsFactors = FALSE
  )
  # extract paths for given stage
  paths <- paths[object$stages[[var]] %in% stage, ncol(paths):1]
  # format to data.frame if var is not the first
  if (var %in% sevt_varnames(object)[2]) {
    paths <- data.frame(paths)
    colnames(paths) <- sevt_varnames(object)[1]
  }
  return(paths)
}

#' Rename stage(s) in staged event tree
#' 
#' Change the name of a stage in a staged event tree.
#' @param object an object of class \code{sevt}.
#' @param var name of a variable in \code{object}.
#' @param stage name of the stage to be renamed.
#' @param new new name for the stage.
#' @details No internal checks are performed and as side effect 
#' stages can be joined, if e.g. \code{new} is equal to the name
#' of a stage for variable \code{var}. 
#' 
#' @return a staged event tree object where stages \code{stage} 
#' have been renamed to \code{new}.
#' @export 
rename_stage <- function(object, var, stage, new){
  check_sevt(object)
  if (length(var) > 1){
    stop("var argument has length > 1")
  }
  if (length(stage) > 1){
    stop("stage argument has length > 1")
  }
  if (length(new) > 1){
    stop("new argument has length > 1")
  }
  if (!var %in% names(object$tree)) {
    stop(var, " is not a variable in the model")
  }
  if (!stage %in% object$stages[[var]]){
    stop(stage, " is not a stage of variable ", var, " in the model")
  }
  # set new label
  object$stages[[var]][object$stages[[var]] %in% stage] <- new
  # if staged tree has prob move it to the new-label
  if (has_prob(object)){
    object$prob[[var]][[new]] <- object$prob[[var]][[stage]]
    object$prob[[var]][[stage]] <- NULL
  }
  return(object)
}