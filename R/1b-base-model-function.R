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
#'            to the appropriate method, see \link{sevt.fit}
#' @details \code{staged_ev_tree} is the basic method to build a 
#'           full or independent staged tree. 
#'           The user will probably find more usefull the two 
#'           wrappers \code{full} and \code{indep}. Moreover 
#'           these last two methods have some advantages over 
#'           the basic \code{staged_ev_tree} function: 
#'           \itemize{
#'              \item \code{indep} is faster then the equivalent 
#'              \code{staged_ev_tree(full = FALSE)} when used from 
#'              a \code{data.frame}. 
#'              \item \code{full} allows for an additional parameter 
#'              \code{join_zero} which if set to \code{TRUE} will join
#'              stages with zero observations (using \code{\link{join_zero}}). If this option is used together 
#'              with \code{fit=TRUE} it will speed-up the computations 
#'              considerably. 
#'           }
#' @return A staged event tree object is a list with components:
#' \itemize{
#'          \item tree (required): A named list where for each variable,
#'                      the levels of such variable are listed.
#'                      The order of the variable is the
#'                      order of the event tree.
#'          \item stages (required): A named list where each component
#'                        stores the stages for the given variable.
#'          \item ctables: The contingency tables of the data.
#'          \item lambda: The smoothing parameter used to estimate the model.
#'          \item prob: The conditional probability tables for every
#'                      variable and every stage.
#'          \item ll: The log-likelihood of the \code{estimated} model.
#'        }
#' @export
#' @examples
#'
#' ######### from dataset
#' DD <- generate_random_dataset(n = 4, 1000)
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
#' modT <- staged_ev_tree(table(PhDArticles), fit = TRUE, full = TRUE, lambda = 1)
#' plot(modT)
#' @export
staged_ev_tree.table <- function(x,
                                 order = names(dimnames(x)),
                                 full = FALSE,
                                 fit = TRUE,
                                 ...) {
  # extract ordered list of levels
  tree <- dimnames(x)[order]
  # extract ordered names of variable 
  vars <- names(tree)
  # number of variables
  k <- length(tree)
  # check if tree exist
  stopifnot(!is.null(tree))
  # build staged tree from list
  model <- staged_ev_tree.list(tree, full = full, ...)
  # if required, fit the model
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
  # extract ordered list of levels
  tree <- lapply(x, function(v) {
    return(levels(as.factor(v)))
  })[order]
  # build staged tree from list 
  sevt <- staged_ev_tree.list(tree, full = full, ...)
  # if required fit the staged tree
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
#' model <- staged_ev_tree(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
#' @export
staged_ev_tree.list <- function(x, full = FALSE, ...) {
  if (is.null(names(x))) {
    # if there are no names of variables
    # we assign variables names V1,V2,...
    names(x) <- paste0("V", 1:length(x))
  }
  
  # extract number of levels for each variable
  dims <- vapply(x, FUN = length, FUN.VALUE = 1)
  if (any(is.null(dims))) {
    # naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }
  if (any(dims == 0)) {
    # naive check if levels are vector with lenght
    stop("Levels should be well defined")
  }

  # initialize empty object
  evt <- list()
  # store tree (ordered list of levels)
  evt$tree <- x
  # if a full staged tree is required 
  # build vector of different stages
  if (full) {
    evt$stages <- lapply(2:length(x), function(i) {
      as.character(1:prod(dims[1:(i - 1)]))
    })
  } else {
    # otherwise the independence model is built
    # using the same stage "1"
    evt$stages <- lapply(2:length(x), function(i) {
      rep("1", prod(dims[1:(i - 1)]))
    })
  }
  # stages should be a named list
  names(evt$stages) <- names(x)[-1]
  # assign class name 
  class(evt) <- "sevt"
  # return staged tree object
  return(evt)
}

#' @rdname staged_ev_tree
#' @export
staged_ev_tree.bn.fit <- function(x, order = NULL, ...) {
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
  # create staged tree from lsit
  object <- staged_ev_tree(tree)
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
        object$stages[[i - 1]] <- 1:length(tree[[i - 1]])
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
        unique_values <- 1:length(values)
        for (l in 1:length(values))
        {
          for (m in 1:length(grid$stages))
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
#' eventually,  an additive (Laplace) smoothing.
#' @param sevt a staged event tree
#' @param data data.frame, contingency table, or fitted
#'        stratified event tree
#' @param lambda smoothing parameter
#' @param ... additional parameters
#' @return a fitted staged event tree, that is an object of class `sevt`
#'         with `ctables` and `prob` arguments.
#' @details The log-likelihood of the model will be recomputed and
#'          stored in the returned object.
#' @export
#' @examples
#'
#' #########
#' model <- staged_ev_tree(list(
#'   X = c("good", "bad"),
#'   Y = c("high", "low")
#' ))
#' D <- data.frame(
#'   X = c("good", "good", "bad"),
#'   Y = c("high", "low", "low")
#' )
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
  } else {
    if (!is.null(data)) {
      sevt$ctables <-
        strt_ev_tree(data, order = names(sevt$tree), fit = TRUE)$ctables
    }
  }
  # store lambda 
  sevt$lambda <- lambda
  # extract order of variables 
  order <- names(sevt$tree)
  # 
  dims <- sapply(sevt$tree, length)
  sevt$prob <- list()
  n <- sum(sevt$ctables[[order[1]]])
  pp <- sevt$ctables[[order[1]]] + lambda
  pp <- pp / sum(pp)
  attr(pp, "n") <- n
  sevt$prob[[order[1]]] <- list("1" = pp)
  for (i in 2:length(order)) {
    stages <- unique(sevt$stages[[order[i]]])
    sevt$prob[[order[i]]] <-
      lapply(stages, function(s) {
        ix <- sevt$stages[[order[i]]] == s
        if (sum(ix) > 1) {
          tt <- apply(sevt$ctables[[order[i]]][ix, ], MARGIN = 2, sum)
        } else {
          tt <- sevt$ctables[[order[i]]][ix, ]
        }
        names(tt) <- sevt$tree[[order[i]]]
        n <- sum(tt) ## compute sample size
        tt <- (tt + lambda) ## smoothing
        tt <- tt / sum(tt) ## normalize
        attr(tt, "n") <- n ## save sample size
        return(tt) # return normalized prob
      })
    names(sevt$prob[[order[i]]]) <- stages
  }
  sevt$ll <- NULL ## force recompute log-likelihood
  sevt$ll <- logLik(sevt)
  return(sevt)
}

#' @rdname staged_ev_tree
#' @export
staged_ev_tree.strt_ev_tree <- function(x, fit = FALSE, lambda = 0, ...) {
  obj <- staged_ev_tree.list(x$tree, full = TRUE)
  vars <- names(x$tree)
  dims <- sapply(x$tree, length)
  obj$lambda <- lambda
  if (!is.null(x$ctables)) {
    obj$ctables <- x$ctables
    if (fit){
      ## if the x is fitted we can just copy the probabilitites
      n <- sum(x$ctables[[vars[1]]])
      pp <- x$ctables[[vars[1]]] + lambda
      pp <- pp / sum(pp)
      obj$prob[[vars[1]]] <- list("1" = pp)
      attr(obj$prob[[vars[1]]][["1"]], "n") <- n
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
        names(obj$prob[[vars[i]]]) <-
          as.character(1:length(obj$prob[[vars[i]]]))
      }
      names(obj$prob) <- names(x$tree)
    }
  }
  return(obj)
}


#' @rdname sevt.fit
#' @details \code{fit.sevt} is the same as \code{sevt.fit}
#'          and it will be probably removed in the future.
#' @export
fit.sevt <- function(sevt,
                     data = NULL,
                     lambda = 0,
                     ...) {
  sevt.fit(sevt, data, lambda, ...)
}

#' Set stage to path
#'
#' Set the given stage to the path for the stage event tree
#' @param sevt Staged event tree
#' @param path Vector of the path
#' @param stage stage to be assigned
#' @keywords internal
set_stage <- function(sevt, path, stage) {
  stage <- as.character(stage)
  ## TO DO
  return(sevt)
}

#' Join stages
#'
#' Join two stages in a staged event tree object, updating
#' probabilities and log-likelihood accordingly.
#'
#'
#' @param object staged event tree
#' @param v variable
#' @param s1 first stage
#' @param s2 second stage
#' @return the staged event tree where \code{s1} and \code{s2} are joined
#' @details this function joins together two stages associated to the same variable.
#' @examples
#' model <- full(PhDArticles, fit = TRUE, lambda = 0)
#' model <- fbhc.sevt(model)
#' model$stages$Kids
#' model <- join_stages(model, "Kids", "5", "6")
#' model$stages$Kids
#' @export
join_stages <- function(object, v, s1, s2) {
  stopifnot(is(object, "sevt"))
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  if (!all(c(s1, s2) %in% stages.sevt(object, var = v))) {
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
    ct1 <-
      ifelse(is.nan(p1), 0, p1) * (n1 + object$lambda * k) - object$lambda
    ct2 <-
      ifelse(is.nan(p2), 0, p2) * (n2 + object$lambda * k) - object$lambda
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


#' split randomly a stage
#'
#' Randomly assign some of the path to a new stage
#'
#' @param object a staged event tree object
#' @param var the variable where to split the stage
#' @param stage the name of the stage to split
#' @param p probability to move a situation from the original stage into the new stage
#'
#' @return a staged event tree object
#' @details It splits randomly a given stage into two stages. More precisely,
#' it assigns each situation within the given stage into a new stage with
#' probability \code{p}.
#' @examples
#' DD <- generate_random_dataset(5, 100)
#' model <- staged_ev_tree(DD, fit = TRUE, full = TRUE, lambda = 1)
#' model <- bhc.sevt(model)
#' model <- stndnaming.sevt(model)
#' model$stages$X5
#'
#' # no split
#' model1 <- split_stage_random(model, "X5", "1", p = 0)
#' model1$stages$X5
#'
#' # all situations in the new stage
#' model2 <- split_stage_random(model, "X5", "1", p = 1)
#' model2$stages$X5
#'
#' # randomly split with probability 0.5
#' model3 <- split_stage_random(model, "X5", "1", p = 0.5)
#' model3$stages$X5
#' @export
split_stage_random <- function(object, var, stage, p = 0.5) {
  stopifnot(is(object, "sevt"))
  # if the given stage is not presetn 
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
    if (is_fitted.sevt(object)) {
      # re-fit the model
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
#' @param join_zero logical, if situations with zero observations should 
#'                           be joined
#' @examples
#'
#' ######### full model
#'
#' DD <- generate_xor_dataset(4, 100)
#' modfull <- full(DD, lambda = 1)
#' @export
full <- function(x, join_zero = FALSE, fit = TRUE, ...) {
  UseMethod("full", x)
}

#' @rdname staged_ev_tree
#' @param name.join name to pass to \code{\link{join_zero}}
#' @export
full.default <- function(x, join_zero = FALSE, fit = TRUE, 
                         name.join = "NA", ...){
  strt <- strt_ev_tree(x, fit = TRUE, ...)
  if (join_zero){
    join_zero(staged_ev_tree(strt, fit = FALSE, ...), 
              fit = fit, name = name.join)
  }else{
    staged_ev_tree(strt, fit = fit, ...)
  }
}

#' @rdname staged_ev_tree
#' @param name.join name to pass to \code{\link{join_zero}}
#' @export
full.strt_ev_tree <- function(x, join_zero = FALSE, fit = TRUE, 
                              name.join = "NA", ...){
  if (join_zero){
    join_zero(staged_ev_tree(x, fit = FALSE, ...), fit = fit, 
              name = name.join)
  }else{
    staged_ev_tree(x, fit = fit, full = TRUE,  ...)
  }
}
#' @rdname staged_ev_tree
#' @export
indep <- function(x, ...) {
  UseMethod("indep", x)
}

#' @rdname staged_ev_tree
#' @export
indep.default <- function(x,  ...) {
  staged_ev_tree(x, full = FALSE, ...)
}

#' @rdname staged_ev_tree
#' @examples
#'
#' ######### independence model (data.frame)
#' DD <- generate_xor_dataset(4, 100)
#' system.time(model1 <- staged_ev_tree(DD, fit = TRUE, lambda = 1))
#' system.time(model2 <- indep(DD, lambda = 1))
#' model1
#' model2
#' @export
indep.data.frame <- function(x, fit = TRUE, lambda = 0, ...) {
  # create the staged tree object
  model <- staged_ev_tree(x, fit = FALSE, full = FALSE, ...)
  # create empty probability list
  model$prob <- list()
  # extract names of variables
  var <- names(model$tree)
  if (fit) {
    # store lambda value
    model$lambda <- lambda
    # initialize loglik to 0
    model$ll <- 0
    # iterate for each variable 
    for (v in var) {
      # extract the table of the given variable
      ctab <- table(x[[v]])
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
    attr(model$ll, "nobs") <- nrow(x)
    # set logLik class
    class(model$ll) <- "logLik"
    # store contingency tables
    model$ctables <- strt_ev_tree(x, fit = TRUE, ...)$ctables
  }
  return(model)
}


#' Inclusion relations between stage structures of two models estimated on the same dataset
#'
#' @param object1 first staged event tree to compare
#' @param object2 second staged event tree to compare
#'
#' @return a list with inclusion relations between stage
#' structures of each variable in the model
#' @details it computes the inclusion/exclusion/equality/diversity between the estimated stages
#' between the two given models, in \code{object1} and \code{object2}.
#' @examples
#' mod1 <- bhc.sevt(full(PhDArticles[, 1:5], lambda = 1))
#' mod2 <- fbhc.sevt(full(PhDArticles[, 1:5], lambda = 1))
#' inclusion.stages(mod1, mod2)
#' @export
inclusion.stages <- function(object1, object2) {
  stopifnot(is(object1, "sevt"))
  stopifnot(is(object2, "sevt"))
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  out <- rep(list(c()), length(object1$stages))
  attr(out, "names") <- attr(object1$stages, "names")
  out2 <- out

  for (k in 1:length(object1$stages)) {
    a <- object1$stages[[k]]
    b <- object2$stages[[k]]
    unique_a <- unique(a)
    unique_b <- unique(b)
    out_a <- out_b <- rep(0, length(a))
    for (i in 1:length(unique_a)) {
      ifelse((length(unique(b[which(a == unique_a[i])])) == 1),
        out_a[which(a == unique_a[i])] <- 1,
        out_a[which(a == unique_a[i])] <- 0
      )
    }
    for (i in 1:length(unique_b)) {
      ifelse((length(unique(a[which(b == unique_b[i])])) == 1),
        out_b[which(b == unique_b[i])] <- 1,
        out_b[which(b == unique_b[i])] <- 0
      )
    }

    out[[k]] <- ifelse((out_a + out_b) == 2, 0, 1)
    out2[[k]] <- character(length(out[[k]]))

    ord1 <- ord2 <- c()

    for (i in 1:length(out[[k]])) {
      if (out[[k]][i] == 0) {
        out2[[k]][i] <- paste(object1$stages[[k]][i], "  =  ", object2$stages[[k]][i])
        ord1 <- c(ord1, object1$stages[[k]][i])
        ord2 <- c(ord2, object2$stages[[k]][i])
      }
      else if (out[[k]][i] == 1) {
        if (out_a[i] == 1 & out_b[i] == 0) {
          out2[[k]][i] <- paste(object1$stages[[k]][i], "  <  ", object2$stages[[k]][i])
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 1) {
          out2[[k]][i] <- paste(object1$stages[[k]][i], "  >  ", object2$stages[[k]][i])
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 0) {
          out2[[k]][i] <- paste(object1$stages[[k]][i], "  !=  ", object2$stages[[k]][i])
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
      }
    }
    ord1 <- as.numeric(ord1)
    ord2 <- as.numeric(ord2)
    ordering <- data.frame(ord1, ord2)
    ordering <- unique(ordering[order(ordering$ord1, ordering$ord2), ])

    # nice print
    out2[[k]] <- data.frame(noquote(out2[[k]]))
    colnames(out2[[k]]) <- paste(deparse(substitute(object1)), " - ", deparse(substitute(object2)))
    name.width <- max(sapply(colnames(out2[[k]]), nchar))
    out2[[k]] <- format(out2[[k]], width = name.width, justify = "centre")
    out2[[k]] <- out2[[k]][noquote(rownames(ordering)), ]
    out2[[k]] <- data.frame(noquote(out2[[k]]))
    colnames(out2[[k]]) <- paste(deparse(substitute(object1)), " - ", deparse(substitute(object2)))
  }
  return(out2)
}


#' Print a staged event tree
#'
#' @param x the staged event tree object
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
  cat(
    "Staged event tree",
    ifelse(is_fitted.sevt(x), "(fitted) \n", "\n")
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
#' @param object a staged event tree object
#' @param var name of one variable
#' @return If \code{var} is specified, it returns a character vector with
#'         stage names for the given variable
#'         (that is \code{object$stages[[var]]}.
#'         Otherwise, If \code{var} is not specified, \code{stages.sevt}
#'         returns a list of character vectors containing the stages associated
#'         to each variable in the model (that is \code{object$stages}).
#' @examples
#' DD <- generate_xor_dataset(5, 100)
#' model <- staged_ev_tree(DD, fit = TRUE, lambda = 0)
#' stages.sevt(model, var = "X5")
#' @export
stages.sevt <- function(object, var = NULL) {
  stopifnot(is(object, "sevt"))
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
#' @param object a staged event tree object
#' @param ... arguments for compatibility
#' @details Print model information and summary of stages.
#' @return An object of class \code{summary.sevt}
#'         for which a \code{print}
#'         method exist.
#' @examples
#' model <- naive.sevt(full(PhDArticles, fit = TRUE, lambda = 1))
#' summary(model)
#' @export
summary.sevt <- function(object, ...) {
  stopifnot(is(object, "sevt"))
  vns <- varnames.sevt(object)
  nv <- nvar.sevt(object)
  out <- list()
  out[[vns[1]]] <- data.frame(
    stage = "1",
    npaths = 0, stringsAsFactors = FALSE
  )
  if (is_fitted.sevt(object)) {
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
    if (is_fitted.sevt(object)) {
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
#' @param x an object of class \code{summary.sevt}
#' @param max the maximum number of variables for which
#'            information is printed
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
#' @param object a staged event tree object
#' @param path, the path after which extract the subtree
#' @details it returns the subtree of the staged event tree given in input as \code{object}.
#' The new root of the subtree is the given \code{path}.
#' @return the staged event tree object corresponding to the subtree
#' @examples
#' DD <- generate_random_dataset(4, 100)
#' model <- staged_ev_tree(DD, full = TRUE)
#' plot(model)
#' model1 <- subtree.sevt(model, path = c("-1", "1"))
#' plot(model1)
#' @export
subtree.sevt <- function(object, path) {
  m <- 1
  idx <- tree_idx(path, object$tree)
  stage <- find_stage(object, path)
  varout <- varnames.sevt(object)[1:length(path)]
  object$tree[varout] <- NULL ## remove previous variables
  object$stages[varout] <- NULL ## remove stages info
  object$ctables[varout] <- NULL
  var <- varnames.sevt(object)
  object$stages[[var[1]]] <-
    c(stage) ## keep stage name for first variable
  if (is_fitted.sevt(object)) {
    object$ctables[[var[1]]] <- object$ctables[[var[1]]][idx, ]
    attr(object$ctables[[var[1]]], "names") <- object$tree[[var[1]]]
  }
  for (i in 2:length(object$tree)) {
    m <- m * length(object$tree[[var[i - 1]]])
    tmpidx <- ((idx - 1) * m):(idx * m - 1) + 1
    object$stages[[var[i]]] <-
      object$stages[[var[i]]][tmpidx]
    if (is_fitted.sevt(object)) { # update ctables
      object$ctables[[var[i]]] <- ftable(object$ctables[[var[i]]][tmpidx, ])
      attr(object$ctables[[var[i]]], "row.vars") <- object$tree[1:(i - 1)]
      attr(object$ctables[[var[i]]], "col.vars") <- object$tree[i]
    }
  }
  if (is_fitted.sevt(object)) {
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
#' @param object a staged event tree object
#' @param uniq logical, if stage numbers shuold be unique over all tree
#' @param prefix logical, if stage names should be prefixed with variable name
#' @param ignore stages name which should be left untouched
#' @return a staged event tree object with stages named with
#' consecutive integers.
#' @examples
#' DD <- generate_xor_dataset(4, 100)
#' model <- staged_ev_tree(DD, full = TRUE)
#' model <- fbhc.sevt(model)
#' model$stages
#' model1 <- stndnaming.sevt(model)
#' model1$stages
#' @export
stndnaming.sevt <- function(object, uniq = FALSE, 
                            prefix = FALSE, ignore = NULL) {
  var <- names(object$tree)
  start <- 0
  for (i in 2:length(var)) {
    v <- var[i]
    old <- unique(object$stages[[v]])
    old <- old[!(old %in% ignore)]
    new <- as.character(start + (1:length(old)))
    if (prefix) new <- paste0(v, new)
    if (uniq) start <- start + length(old)
    object$stages[[v]] <- vapply(object$stages[[v]], function(s) {
      if (s %in% ignore) return(s)
      new[which(old %in% s, useNames = FALSE)]
    }, FUN.VALUE = "a", USE.NAMES = FALSE)
    if (is_fitted.sevt(object)) {
      object$prob[[v]][new] <- object$prob[[v]][old]
      object$prob[[v]][old[!(old %in% new)]] <-
        NULL ## remove old prob
    }
  }
  if (is_fitted.sevt(object)) {
    object$prob[[var[1]]] <- list("1" = object$prob[[var[1]]][[1]])
  }
  return(object)
}

#' Compare two staged event tree
#'
#' Compare two stages event tree, return the differences of the stages
#' structure and plot the difference tree. Three different methods to
#' compute the difference tree are available.
#'
#' @param object1 a staged event tree
#' @param object2 a staged event tree
#' @param method method to compare staged event trees. It can be: \code{"naive"}, \code{"hamming"} or \code{"stages"}
#' @param return.tree logical, if \code{TRUE} the difference tree is returned
#' @param plot logical
#' @param ... additional parameters to be passed to \code{\link{plot.sevt}}
#' @details \code{compare.sevt} tests if the stage structure of two \code{sevt}
#' objects
#' is the same.
#' Three methods are available:
#' * \code{naive} first applies \code{\link{stndnaming.sevt}} to both
#' objects and then simply compares the resulting stages lists
#' (\code{stages.sevt(object1)} and \code{stages.sevt(object2)}).
#' * \code{hamming} uses the \code{hamming.sevt} function that try to map
#' stages in the different objects finding the few number of nodes that
#' must be changed to obtain the same structure.
#' * \code{stages} uses the \code{stagesdiff.sevt} function that compare
#' stages to check if the same stage structure is present in both models.
#'
#' Setting \code{return.tree = TRUE} will return the stages
#' structure difference obtained with the selected method.
#'
#' With \code{plot = TRUE} the plot of the difference tree is obtained.
#'
#' If \code{return.tree = FALSE} the logical output is the same for the
#' three methods and thus the \code{naive} method should be used
#' since it is computationally faster.
#'
#' To use the \code{hamming} method, the package \code{clue}
#' must be installed.
#'
#' Functions \code{\link{hamming.sevt}} and \code{\link{stagesdiff.sevt}}
#' can also be used directly.
#'
#' @return if \code{return.tree = FALSE}, logical: \code{TRUE} if the two
#' models are exactly equal, otherwise \code{FALSE}.
#' Else If \code{return.tree = TRUE}  it returns the differences between
#' the two trees, according to the selected \code{method}.
#' @export
#' @examples
#' data("PhDArticles")
#' mod1 <- bhc.sevt(full(PhDArticles[, 1:4], lambda = 1))
#' mod2 <- fbhc.sevt(full(PhDArticles[, 1:4], lambda = 1))
#' compare.sevt(mod1, mod2)
compare.sevt <-
  function(object1,
           object2,
           method = "naive",
           return.tree = FALSE,
           plot = FALSE,
           ...) {
    # check and rename stages
    stopifnot(is(object1, "sevt"))
    stopifnot(is(object2, "sevt"))
    stopifnot(all(names(object1$tree) == names(object2$tree)))
    object1 <- stndnaming.sevt(object1)
    object2 <- stndnaming.sevt(object2)
    # use the appropriate method
    difftree <- switch(
      method,
      naive = sapply(names(object1$tree)[-1],
        function(v) {
          sign(abs(
            as.numeric(object1$stages[[v]]) -
              as.numeric(object2$stages[[v]])
          ))
        },
        USE.NAMES = TRUE
      ),
      hamming = hamming.sevt(object1, object2, return.tree = TRUE),
      stages = stagesdiff.sevt(object1, object2),
      sapply(names(object1$tree)[-1],
        function(v) {
          sign(abs(
            as.numeric(object1$stages[[v]]) -
              as.numeric(object2$stages[[v]])
          ))
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
      object1$stages <- difftree
      plot(
        object1,
        col = function(x) {
          rep("red", length(x))
        },
        pch = 16,
        ...
      )
    }
    if (return.tree) {
      return(difftree)
    } else {
      return(all(sapply(difftree, function(x) {
        all(x == 0)
      })))
    }
  }


#' @rdname compare.sevt
#' @export
hamming.sevt <- function(object1, object2, return.tree = FALSE) {
  stopifnot(is(object1, "sevt"))
  stopifnot(is(object2, "sevt"))
  # check if models are over the same variables, and same order
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("Package \"clue\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # rename stages with increasing integers
  object1 <- stndnaming.sevt(object1)
  object2 <- stndnaming.sevt(object2)
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
    sign(abs(
      as.numeric(object1$stages[[v]]) -
        as.numeric(object2$stages[[v]])
    ))
  }, USE.NAMES = TRUE)
  # return tree if required or simply the hamming distance
  if (return.tree) {
    return(difftree)
  } else {
    sum(sapply(difftree, function(x) {
      sum(as.numeric(x))
    }))
  }
}


#' @rdname compare.sevt
#' @export
#' @examples
#'
#' ##########
#' m0 <- full(PhDArticles[, 1:4], fit = TRUE, lambda = 0)
#' m1 <- bhc.sevt(m0)
#' m2 <- bj.sevt(m0, distance = tv, thr = 0.25)
#' stagesdiff.sevt(m1, m2)
stagesdiff.sevt <- function(object1, object2) {
  stopifnot(is(object1, "sevt"))
  stopifnot(is(object2, "sevt"))
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  out <- rep(list(c()), length(object1$stages))
  attr(out, "names") <- attr(object1$stages, "names")
  for (k in 1:length(object1$stages)) {
    a <- object1$stages[[k]]
    b <- object2$stages[[k]]
    unique_a <- unique(a)
    unique_b <- unique(b)
    out_a <- out_b <- rep(0, length(a))
    for (i in 1:length(unique_a)) {
      ifelse((length(unique(b[which(a == unique_a[i])])) == 1),
        out_a[which(a == unique_a[i])] <- 1,
        out_a[which(a == unique_a[i])] <- 0
      )
    }
    for (i in 1:length(unique_b)) {
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

#' Get variable names
#'
#' @param x a staged event tree object
#' @return vector with variable names
#' @examples
#'
#' mod <- full(PhDArticles)
#' varnames.sevt(mod)
#' @export
varnames.sevt <- function(x) {
  names(x$tree)
}

#' Get the number of variables
#'
#' @param x a staged event tree object
#' @return integer, the number of variables
#' @examples
#'
#' mod <- indep(PhDArticles)
#' nvar.sevt(mod)
#' @export
nvar.sevt <- function(x) {
  stopifnot(is(x, "sevt"))
  length(names(x$tree))
}


#' Number of parameters of a staged event tree
#'
#' Return the number of parameters of the model.
#' @param x a staged event tree object
#' @return integer, degrees of freedom of the staged event tree
#' @examples
#'
#' #########
#' mod_f <- full(PhDArticles)
#' df.sevt(mod_f)
#'
#' mod_i <- indep(PhDArticles)
#' df.sevt(mod_i)
#' @export
df.sevt <- function(x) {
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

#' Retrieve stage or path
#'
#' Utility functions to obtain stages from paths and
#' paths from stages.
#'
#' @name getstagepath
NULL


#' @rdname getstagepath
#' @param object a staged event tree object
#' @param path a vector contating the path from root or
#' a two dimensional array where each row is a path
#' from root
#' @return \code{get_stage} returns
#' the name of the stage for a given path (or paths).
#' @examples
#' model <- fbhc.sevt(full(PhDArticles))
#' get_stage(model, c("0", "male"))
#' paths <- expand.grid(model$tree[2:1])[, 2:1]
#' get_stage(model, paths)
#' @export
get_stage <- function(object, path) {
  stopifnot(is(object, "sevt"))
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


#' @rdname getstagepath
#'
#' @param var string, one of the variable in
#'            the staged tree
#' @param stage string or vector, the name
#' of the stages for which the paths should be
#' returned
#' @return \code{get_paths} return a
#'         data.frame containing the paths
#'         corresponding to the given stage (or stages).
#' @examples
#' get_path(model, "Kids", "11")
#' get_path(model, "Gender", "2")
#' get_path(model, "Kids", c("5", "6"))
#' @export
get_path <- function(object, var, stage) {
  if (!var %in% names(object$tree)) {
    stop(var, " is not a variable in the model")
  }
  
  # list all paths
  paths <- expand.grid(object$tree[(which(var == varnames.sevt(object)) - 1):1],
    stringsAsFactors = FALSE
  )
  # extract paths for given stage
  paths <- paths[object$stages[[var]] %in% stage, ncol(paths):1]
  # format to data.frame if var is not the first
  if (var %in% varnames.sevt(object)[2]) {
    paths <- data.frame(paths)
    colnames(paths) <- varnames.sevt(object)[1]
  }
  return(paths)
}

#' rename stage(s) in staged even tree
#' 
#' @param object staged event tree
#' @param var string, one of the variable in
#'            the staged tree
#' @param stage string or vector, the name
#' of the stage(s) to be renamed
#' @param new.label new name for the stage(s)
#' @return a staged event tree object where stages \code{stage} 
#' have been renamed to \code{new.label}
#' @export 
rename_stage <- function(object, var, stage, new.label){
  stopifnot(is(object, "sevt"))
  if (!var %in% names(object$tree)) {
    stop(var, " is not a variable in the model")
  }
  # set new label
  object$stages[[var]][object$stages[[var]] %in% stage] <- new.label
  # if staged tree has probabilities re-fit the model
  # (TODO: faster version without refitting)
  if (is_fitted.sevt(object)){
    object <- sevt.fit(object)
  }
  return(object)
}
