#' Staged (stratified) event tree
#'
#' Builds the staged event tree for a set of categorical variables,
#' the order can be specified.
#'
#' @param x data.frame, list, table, \code{bn.fit} object
#' @param order order of the variables
#' @param full logical, if \code{TRUE} the full model is created
#' @param fit logical, if  \code{TRUE} the model is fitted
#' @param lambda smoothing parameter
#' @param ... additional parameters to be passed
#'            to the appropiate method, see \link{sevt.fit}
#' @return A staged event tree object
#' @details A staged event tree object is a list with components:
#'\itemize{
#'          \item tree: A named list where for each variable,
#'                      the levels of such variable are listed.
#'                      The order of the variable is the
#'                      order of the event tree.
#'          \item stages: A named list where each component
#'                        stores the stages for the given variable.
#'          \item prob: The conditional probability tables for every
#'                      variable and every stage.
#'          \item ctables: The contingency tables of the data.
#'          }
#' @export
#' @examples
#'
#' ######### from data.frame
#' DD <- generate_random_dataset(n = 5, 1000)
#' indep <- staged_ev_tree(DD, fit = TRUE)
#' full <- staged_ev_tree(DD, full = TRUE, fit = TRUE, lambda = 1)
staged_ev_tree <- function(x, ...) {
  UseMethod("staged_ev_tree", object = x)
}

#' @rdname staged_ev_tree
#' @export
staged_ev_tree.default <- function(x, ...) {
  return(staged_ev_tree.data.frame(as.data.frame(x, ...)))
}




#' @rdname staged_ev_tree
#' @examples
#'
#' ######### from table
#' data("Titanic")
#' modT <- staged_ev_tree(Titanic, fit = TRUE, full = TRUE, lambda = 1)
#' plot(modT)
#' @export
staged_ev_tree.table <- function(x,
                                 order = names(dimnames(x)),
                                 full = FALSE,
                                 fit = TRUE,
                                 ...) {
  tree <- dimnames(x)[order]
  vars <- names(tree)
  k <- length(tree)
  stopifnot(!is.null(tree))
  model <- staged_ev_tree.list(tree, full = full, ...)
  if (fit) {
    model <- sevt.fit(model, data = x, ...)
  }
  return(model)
}


#' @rdname staged_ev_tree
#' @export
staged_ev_tree.data.frame <- function(x,
                                      order = colnames(x),
                                      full = FALSE,
                                      fit = TRUE,
                                      ...) {
  tree <- lapply(x, function(v)
    return(levels(as.factor(v))))[order]
  sevt <- staged_ev_tree.list(tree, full = full, ...)
  if (fit) {
    return(sevt.fit(sevt, data = x, ...))
  } else {
    return(sevt)
  }
}


#' @rdname staged_ev_tree
#' @examples
#'
#' ######### from list
#' model <- staged_ev_tree(list(X = c("good", "bad"),
#'                              Y = c("high", "low")))
#' @export
staged_ev_tree.list <- function(x, full = FALSE, ...) {
  if (is.null(names(x))) {
    #if there are no names of variables
    #we assign variables names V1,V2,...
    names(x) <- paste0("V", 1:length(x))
  }
  
  dims <- vapply(x, FUN = length, FUN.VALUE = 1)
  if (any(is.null(dims))) {
    #naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }
  
  evt <- list()
  evt$tree <- x
  if (full) {
    evt$stages <- lapply(2:length(x), function(i) {
      as.character(1:prod(dims[1:(i - 1)]))
    })
  } else{
    evt$stages <- lapply(2:length(x), function(i) {
      rep("1", prod(dims[1:(i - 1)]))
    })
  }
  names(evt$stages) <- names(x)[-1]
  class(evt) <- "sevt"
  return(evt)
}

#' @rdname staged_ev_tree
#' @export
staged_ev_tree.bn.fit <- function(x, ...) {
  bn <- bnlearn::bn.net(x)
  tree <- lapply(x,
                 function(tt)
                   if (length(tt$parents) == 0)
                     names(tt$prob)
                 else
                   rownames(tt$prob))
  order <- bnlearn::node.ordering(bn)
  tree <- tree[order]
  object <- staged_ev_tree(tree)
  parents <- lapply(bn$nodes[order], function(n) {
    if (identical(n$parents, character(0))) {
      return(NULL)
    } else{
      return(n$parents)
    }
  })
  for (i in 2:length(order)) {
    if (i <= 2) {
      if (order[i - 1] %in% parents[[i]]) {
        object$stages[[i - 1]] <- 1:length(tree[[i - 1]])
      }
    } else{
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
            if (sum(grid[j,-ncol(grid)] == grid[k,-ncol(grid)]) ==
                length(grid[j,-ncol(grid)]))
            {
              grid$stages[k] <- grid$stages[j]
            }
          }
        }
        values <- unique(grid$stages)
        unique_values <- 1:length(values)
        for (l in 1:length(values))
        {
          for (m in 1:length(grid$stages))
          {
            if (grid$stages[m] == values[l])
            {
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
#' @param sevt a staged event tree
#' @param data data.frame or stratified event tree
#' @param lambda smoothing parameter
#' @param ... additional parameters
#' @return a fitted staged event tree
#' @export
#' @examples
#'
#' #########
#' model <- staged_ev_tree(list(X = c("good", "bad"),
#'                         Y = c("high", "low")))
#' D <- data.frame(X = c("good", "good", "bad"),
#'                 Y = c("high", "low", "low"))
#' model.fit <- sevt.fit(model, data = D, lambda = 1)
sevt.fit <- function(sevt,
                     data = NULL,
                     lambda = 0,
                     ...) {
  if (is.null(data)) {
    if (is.null(sevt$ctables)) {
      warning("Data must be provided or included in the model object")
      return(sevt)
    }
  }
  if (is(data, "strt_ev_tree")) {
    sevt$ctables <- data$ctables
  } else{
    if (!is.null(data)) {
      sevt$ctables <-
        strt_ev_tree(data, order = names(sevt$tree), fit = TRUE)$ctables
    }
  }
  sevt$lambda <- lambda
  order <- names(sevt$tree)
  dims <- sapply(sevt$tree, length)
  sevt$prob <- list()
  n <- sum(sevt$ctables[[order[1]]])
  pp <- sevt$ctables[[order[1]]] + lambda
  pp <- pp / sum(pp)
  attr(pp, "n") <- n
  sevt$prob[[order[1]]] <- list("1" = pp)
  #lambda <- lambda / dims[1]
  for (i in 2:length(order)) {
    stages <- unique(sevt$stages[[order[i]]])
    sevt$prob[[order[i]]] <-
      lapply(stages, function(s) {
        ix <- sevt$stages[[order[i]]] == s
        if (sum(ix) > 1) {
          tt <- apply(sevt$ctables[[order[i]]][ix,], MARGIN = 2, sum)
        } else{
          tt <- sevt$ctables[[order[i]]][ix,]
        }
        names(tt) <- sevt$tree[[order[i]]]
        n <- sum(tt) ##compute sample size
        tt <- (tt + lambda) ##smoothing
        tt <- tt / sum(tt)  ##normalize
        attr(tt, "n") <- n ##save sample size
        return(tt) #return normalized prob
      })
    names(sevt$prob[[order[i]]]) <- stages
    #lambda <- lambda / dims[i]
  }
  sevt$ll <- NULL ##force recompute log-likelihood
  sevt$ll <- logLik(sevt)
  return(sevt)
}

#' @rdname staged_ev_tree
#' @export
staged_ev_tree.strt_ev_tree <- function(x, lambda = 0, ...) {
  obj <- staged_ev_tree.list(x$tree, full = TRUE)
  vars <- names(x$tree)
  dims <- sapply(x$tree, length)
  if (!is.null(x$ctables)) {
    obj$ctables <- x$ctables
    ## if the x is fitted we can just copy the probabilitites
    n <- sum(x$ctables[[vars[1]]])
    pp <- x$ctables[[vars[1]]] + lambda
    pp <- pp / sum(pp)
    obj$prob[[vars[1]]] <- list("1" = pp)
    attr(obj$prob[[vars[1]]][["1"]], "n") <- n
    #lambda <- lambda / dims[1]
    for (i in 2:length(x$tree)) {
      obj$prob[[vars[i]]] <-
        lapply(1:(dim(x$ctables[[vars[i]]])[1]), function(k) {
          n <- sum(x$ctables[[vars[i]]][k, ])
          pp <- (x$ctables[[vars[i]]][k, ] + lambda)
          pp <- pp / sum(pp)
          names(pp) <- x$tree[[vars[i]]]
          attr(pp, "n") <- n
          return(pp)
        })
      #lambda <- lambda / dims[i]
      names(obj$prob[[vars[i]]]) <-
        as.character(1:length(obj$prob[[vars[i]]]))
    }
    names(obj$prob) <- names(x$tree)
  }
  return(obj)
}


#'@rdname sevt.fit
#'@details \code{fit.sevt} is just a wrapper of \code{sevt.fit},
#'          it is provided for compatibilities. (DEPRECATED)
#'@export
fit.sevt <- function(sevt,
                     data = NULL,
                     lambda = 0,
                     ...){
  sevt.fit(sevt, data, lambda, ...)
}

#' Set stage to path
#'
#' Set the given stage to the path for the stage event tree
#' @param sevt Staged event tree
#' @param path Vector of the path
#' @param stage stage to be assigned
set_stage <- function(sevt, path, stage) {
  stage <- as.character(stage)
  ## TO DO
  return(sevt)
}

#' Join two stages
#'
#' Probabilities are recomputed
#'
#' @param sevt staged event tree
#' @param v variable
#' @param s1 first stage
#' @param s2 second stage
#' @return the staged event tree where \code{s1} and \code{s2} are joined
#' @export
join_stages <- function(sevt, v,  s1, s2) {
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  d <- dim(sevt$paths[[v]])[2]
  k <- length(sevt$tree[[v]])
  st <- sevt$stages[[v]]
  sevt$stages[[v]][st == s2] <- s1
  if (!is.null(sevt$prob)) {
    n2 <- attr(sevt$prob[[v]][[s2]], "n")
    n1 <- attr(sevt$prob[[v]][[s1]], "n")
    ct1 <-
      sevt$prob[[v]][[s1]] * (n1 + sevt$lambda * k) - sevt$lambda
    ct2 <-
      sevt$prob[[v]][[s2]] * (n2 + sevt$lambda * k) - sevt$lambda
    dll <-
      sum(ct2 * log(sevt$prob[[v]][[s2]])) +
      sum(ct1 * log(sevt$prob[[v]][[s1]]))
    sevt$prob[[v]][[s1]] <-  ct2 + ct1 + sevt$lambda
    attr(sevt$prob[[v]][[s1]], "n") <- n1 + n2
    sevt$prob[[v]][[s1]] <-
      sevt$prob[[v]][[s1]] / sum(sevt$prob[[v]][[s1]])
    sevt$prob[[v]][[s2]] <- NULL ##delete one of the two
    if (!is.null(sevt$ll)) {
      ## update log likelihood
      sevt$ll <-
        sevt$ll - dll +  sum((ct1 + ct2) *
                               log(sevt$prob[[v]][[s1]]))
      attr(sevt$ll, "df") <-
        attr(sevt$ll, "df") - length(sevt$prob[[v]][[s1]]) + 1
    }
  }
  return(sevt)
}


#' split randomly a stage
#'
#' Randomly assign some of the path to a new stage
#'
#' @param object a staged event tree object
#' @param var the variable where to split the stage
#' @param stage the name of the stage
#' @param p probability
#'
#' @return a staged event tree object
#' @examples
#' DD <- generate_random_dataset(2, 100)
#' model <- staged_ev_tree(DD, fit = TRUE, full = TRUE, lambda = 1)
#' @export
split_stage_random <- function(object, var,  stage, p = 0.5) {
  if (!(stage %in% object$stages[[var]])) {
    return(object)
  }
  d <- length(object$stages[[var]])
  label <- new_label(object$stages[[var]])
  ix <-
    (object$stages[[var]] == stage) &
    sample(
      x = c(TRUE, FALSE),
      size = d,
      prob = c(p, 1 - p),
      replace = TRUE
    )
  if (any(ix)) {
    object$stages[[var]][ix] <- label
    if (is_fitted.sevt(object)) {
      ### we should do better than this
      object <- sevt.fit(object, lambda = object$lambda)
    }
  }
  return(object)
}

#' Check if a staged event tree is fitted
#'
#' @param x a staged event tree object
#' @return logical
#'
#' @export
is_fitted.sevt <- function(x) {
  return(!is.null(x$prob) && !is.null(x$ctables))
}

#' @rdname staged_ev_tree
#' @examples 
#' 
#' ######### full model 
#' 
#' DD <- generate_xor_dataset(7, 100)
#' modfull <- full(DD, lambda = 1)
#' @export
full <- function(x, ...) {
  staged_ev_tree(x, full = TRUE, ...)
}

#' @rdname staged_ev_tree
#' @examples
#'
#' ######### independence model
#' DD <- generate_xor_dataset(15, 100)
#' system.time(model1 <- staged_ev_tree(DD, fit = TRUE, lambda = 1))
#' system.time(model2 <- indep(DD, lambda = 1))
#' model1
#' model2
#' @export
indep <- function(x, lambda = 0, ...) {
  x <- as.data.frame(x)
  model <- staged_ev_tree(x, fit = FALSE, full = FALSE)
  model$prob <- list()
  var <- names(model$tree)
  model$lambda <- lambda
  model$ll <- 0
  for (v in var) {
    ctab <- table(x[[v]])
    n <- sum(ctab)
    model$prob[[v]] <- list("1" =  ctab + lambda)
    model$prob[[v]][[1]] <-
      model$prob[[v]][[1]] / sum(model$prob[[v]][[1]])
    attr(model$prob[[v]][[1]], "n") <- n
    ix <- ctab > 0
    model$ll <-
      model$ll + sum(ctab[ix] * log(model$prob[[v]][[1]][ix]))
  }
  attr(model$ll, "df") <-
    sum(vapply(model$tree, length, FUN.VALUE = 1) - 1)
  attr(model$ll, "nobs") <- nrow(x)
  class(model$ll) <- "logLik"
  model$ctables <- strt_ev_tree(x, fit = TRUE)$ctables
  return(model)
}

#' Print a staged event tree
#'
#' @param x the staged event tree obejct
#' @param ... additional parameters (compatibility)
#'
#' @return An invisible copy of \code{x}
#' @details The order of the variables in the stratified tree
#'  is printed (from root). In addition the number of levels of each
#'  variable is shown in square brackets.
#'  If available the log-likelihood of the model is printed.
#' @export
#' @examples
#' DD <- generate_xor_dataset(5, 100)
#' model <- staged_ev_tree(DD, fit = TRUE, lambda = 1)
#' print(model)
print.sevt <- function(x, ...) {
  cat("Staged event tree",
      ifelse(is_fitted.sevt(x), "(fitted) \n", "\n"))
  ls <- vapply(x$tree, length, 1)
  cat(paste(paste0(names(x$tree), "[", ls, "] ") , collapse = "-> "), "\n")
  #nstages <- vapply(x$stages, function(s) length(unique(s)), FUN.VALUE = 1)
  #cat("n.stages: \n")
  #print(nstages)
  if (!is.null(x$ll)) {
    print(x$ll)
  }
  invisible(x)
}



#' Print stages info
#'
#' @param object a staged event tree object
#' @param var name of one variable
#' @export
stages.sevt <- function(object, var = NULL) {
  stopifnot(is(object, "sevt"))
  if (is.null(var)) {
    object$stages
  } else{
    object$stages[[var]]
  }
}


#' Print stage(s) info
#'
#' @param object a staged event tree object
#' @param var name of one variable
#' @param stage name(s) of stage for variable \code{var}
#' @export
stageinfo.sevt <- function(object, var, stage = NULL) {
  stopifnot(is(object, "sevt"))
  if (var == names(object$tree)[1]) {
    cat("First variable ", var, "\n")
    if (is_fitted.sevt(object)) {
      cat("  ", "probabilities: ")
      cat(paste0(names(object$prob[[var]][[1]]), collapse = "      "), "\n")
      temp <- sapply(object$tree[[var]], nchar)
      temp <-
        sapply(sapply(temp, function(i)
          rep(" ", i)), paste0, collapse = "")
      temp <- paste0(round(object$prob[[var]][[1]], 3), temp)
      cat(character(18), temp, "\n")
      cat(character(3),
          "sample size:",
          attr(object$prob[[var]][[1]], "n"),
          "\n")
    }
  }
  if (is.null(stage)) {
    stage <- unique(object$stages[[var]])
  }
  invisible(sapply(stage, function(s) {
    s <- as.character(s)
    cat("Stage ", s, "for variable", var, "\n")
    cat("  ", sum(object$stages[[var]] == s), "nodes in the stage \n")
    if (is_fitted.sevt(object)) {
      cat("  ", "probabilities: ")
      cat(paste0(names(object$prob[[var]][[s]]), collapse = "      "), "\n")
      temp <- sapply(object$tree[[var]], nchar)
      temp <- sapply(sapply(temp, function(i)
        rep(" ", i)),
        paste0, collapse = "")
      temp <- paste0(round(object$prob[[var]][[s]], 3), temp)
      cat(character(18), temp, "\n")
      cat(character(3),
          "sample size:",
          attr(object$prob[[var]][[s]], "n"),
          "\n")
    }
    #cat("  ", "paths: TO DO \n")
    cat(rep("-", 32), "\n", sep = "")
  }))
}


#' Extract subtree
#'
#' @param object a staged event tree object
#' @param path, the path after which extract the subtree
#'
#' @return the staged event tree object corresponding to the subtree
#' @export
subtree.sevt <- function(object, path) {
  m <- 1
  idx <- tree_idx(path, object$tree)
  stage <- find_stage(object, path)
  object$tree[1:length(path)] <- NULL ##remove previous variables
  object$stages[1:length(path)] <- NULL ##remove stages info
  for (i in 2:length(object$tree)) {
    m <- m * length(object$tree[[i - 1]])
    object$stages[[i - 1]] <-
      object$stages[[i - 1]][((idx - 1)  * m):(idx  * m - 1) + 1]
  }
  if (is_fitted.sevt(object)) {
    object$prob[1:length(path)] <- NULL
    object$prob[[1]] <- object$prob[[1]][stage]
    for (i in 2:length(object$tree)) {
      ###to do: clean unused probabilities
    }
    #object$ctables <- NULL
    object$ll <- NULL
    #object$ll <- logLik(object)
  }
  return(object)
}


#'  Standard renaming of stages
#'
#' @param object a staged event tree object
#' @param rep logical, if stages name can be repeated for different
#'                variables
#' @return a staged event tree object with stages named with
#' consecutive integers.
#' @export
stndnaming.sevt <- function(object, rep = FALSE) {
  var <- names(object$tree)
  for (i in 2:length(var)) {
    v <- var[i]
    old <- unique(object$stages[[v]])
    new <- as.character(1:length(old))
    object$stages[[v]] <- vapply(object$stages[[v]], function(s) {
      as.character(which(old %in% s, useNames = FALSE))
    }, FUN.VALUE = "a", USE.NAMES = FALSE)
    if (is_fitted.sevt(object)) {
      object$prob[[v]][new] <- object$prob[[v]][old]
      object$prob[[v]][old[!(old %in% new)]] <-
        NULL ##remove old prob
    }
  }
  if (is_fitted.sevt(object)) {
    object$prob[[var[1]]] <- list("1" = object$prob[[var[1]]][[1]])
  }
  return(object)
}

#' Compare two staged event tree
#'
#' @param object1 a staged event tree
#' @param object2 a staged event tree
#' @param tree logical, if \code{TRUE} the difference tree is returned
#' @param plot logical
#' @param ... additional paraters to be passed to \code{\link{plot.sevt}}
#' @details \code{compare.sevt} test if the stage structure of two \code{sevt} 
#' objects
#' is the same by first applying \code{\link{stndnaming.sevt}} to both 
#' objects and then simply comparing the resulting \code{$stages} lists. 
#' Setting \code{tree = TRUE} will return the stages structure difference 
#' but at the moment is not completely correct, as the plot produced by
#' \code{plot = TRUE}.
#' 
#' @return Logical or a difference tree (if \code{tree = TRUE})
#' @export
#' @examples
#'
#' #########
#' data("Trump")
#' mod1 <- bhc.sevt(full(Trump, lambda = 1))
#' mod2 <- fbhc.sevt(full(Trump, lambda = 1))
#' compare.sevt(mod1, mod2)
compare.sevt <-
  function(object1,
           object2,
           tree = FALSE,
           plot = FALSE,
           ...) {
    stopifnot(is(object1, "sevt"))
    stopifnot(is(object2, "sevt"))
    stopifnot(all(names(object1$tree) == names(object2$tree)))
    object1 <- stndnaming.sevt(object1)
    object2 <- stndnaming.sevt(object2)
    difftree <- lapply(names(object1$tree)[-1], function(v)
    {
      sign(abs(
        as.numeric(object1$stages[[v]]) -
          as.numeric(object2$stages[[v]])
      ))
    })
    if (plot) {
      object1$stages <- difftree
      plot(
        object1,
        col = function(x)
          rep("red", length(x)),
        pch = 16,
        ...
      )
      
    }
    if (tree) {
      return(difftree)
    } else{
      return(all(sapply(difftree, function(x)
        all(x == 0))))
    }
  }

#' Get variable names
#' 
#' @param x a staged event tree obejct
#' @return Vector with variable names
#' @examples 
#' 
#' data("Titanic")
#' mod <-full(Titanic)
#' varnames.sevt(mod)
#' @export
varnames.sevt <- function(x){
  names(object$tree)
}

#' Get the number of variables 
#' 
#' @param x a staged event tree object
#' @return integer, the number of variables
#' @examples 
#' 
#' data("Titanic")
#' mod <- indep(Titanic)
#' nvar.sevt(mod)
#' @export
nvar.sevt <- function(x, ...){
  length(names(x))
}