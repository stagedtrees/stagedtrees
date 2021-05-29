#' Compute probability of a path from root
#'
#' Internal function to compute probability of a path. It does not
#' check the validity of the path.
#' @param object An object of class \code{sevt}.
#' @param x the path, expressed 
#'          as a character vector containing the sequence of the value of the variables.
#' @param log logical, if \code{TRUE} log-probability is returned.
#' @return The probability of the given path or its logarithm if \code{log=TRUE}.
#' @details Computes the probability of following a given path (\code{x}) starting from the root.
#' Can be a full path from the root to a leaf or a shorter path.
#' @keywords internal
path_probability <-
  function(object, x, log = FALSE) {
    check_sevt_prob(object)
    vs <- sevt_varnames(object)
    if (!is.null(names(x))) {
      # if it's a named vector just order it
      x <- x[vs]
    }
    # start computing the log probability with first variable
    l <- log(object$prob[[vs[1]]][[1]][x[1]])
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        # get corresponding stage
        s <- find_stage(object, x[1:(i - 1)])
        # and add log-prob
        l <- l + log(object$prob[[vs[i]]][[s]][x[i]])
      }
    }
    # return log prob or prob as requested
    if (log) {
      return(l)
    } else {
      return(exp(l))
    }
  }


#' Probabilities for a staged event tree
#' 
#' Compute (marginal and/or conditional) probabilities of elementary 
#' events with respect 
#' to the probability encoded in a staged event tree.
#' @param object an object of class \code{sevt} with probabilities.
#' @param x the vector or data.frame of observations.
#' @param conditional_on named vector, the conditioning event. 
#' @param log logical, if \code{TRUE} log-probabilities are returned.
#' @param na0 logical, if \code{NA} should be converted to 0.
#' @return the probabilities to observe each observation in \code{x}, possibly
#' conditional on the event(s) in \code{conditional_on}.
#'
#' @details Computes probabilities related to a vector or a 
#' data.frame of observations.
#' 
#' Optionally, conditional probabilities can be obtained by specifying 
#' the conditioning event in \code{conditional_on}. This can be done either
#' with a single named vector or with a data.frame object with the 
#' same number of rows of \code{x}. In the former, the same conditioning 
#' is used for all the computed probabilities (if \code{x} has multiple rows); 
#' while with the latter different conditioning events (but on the same variables)
#' can be specified for each row of \code{x}. 
#' 
#' @examples
#' data(Titanic)
#' model <- full(Titanic, lambda = 1)
#' samples <- expand.grid(model$tree[c(1, 4)])
#' pr <- prob(model, samples)
#' ## probabilities sum up to one 
#' sum(pr)
#' ## print observations with probabilities
#' print(cbind(samples, probability = pr))
#' 
#' ## compute one probability
#' prob(model, c(Class = "1st", Survived = "Yes"))
#' 
#' ## compute conditional probability 
#' prob(model, c(Survived = "Yes"), conditional_on = c(Class = "1st"))
#' 
#' ## compute conditional probabilities with different conditioning set
#' prob(model, data.frame(Age = rep("Adult", 8)), 
#'      conditional_on = expand.grid(model$tree[2:1])) 
#' ## the above should be the same as 
#' summary(model)$stages.info$Age
#' @export
prob <- function(object, x, conditional_on = NULL, log = FALSE, na0 = TRUE) {
  check_sevt_prob(object)
  if (is.null(dim(x))) {
    x <- as.data.frame(t(x))
  }
  p1 <- 1
  if (!is.null(conditional_on)){
    if (is.vector(conditional_on) && !is.null(names(conditional_on))){
      ## check if same names
      if (any(names(x) %in% names(conditional_on))){
        stop("invalid argument, x and conditional_on names should be disjoint")
      }
      x <- cbind(x, as.data.frame(t(conditional_on)))
      p1 <- prob(object, x = conditional_on, log = FALSE, na0 = na0)
    }else if (is.data.frame(conditional_on)){
      ## check if same names
      if (any(names(x) %in% names(conditional_on))){
        stop("invalid argument, x and conditional_on names should be disjoint")
      }
      x <- cbind(x, conditional_on)
      p1 <- prob(object, x = conditional_on, log = FALSE, na0 = na0)
      }else{
      stop("invalida argument, conditional_on must be NULL,
           a named vector or a data.frame")
    }
  }
  # get dimensions and variables
  n <- nrow(x)
  i <- ncol(x)
  # get variables in the model
  var <- names(object$tree)
  # variables of the model that are in x
  var1 <- var[var %in% colnames(x)]
  # index of last variable that appears in x
  k <- which(var %in% var1[length(var1)])
  res <- vapply(
    1:n,
    FUN.VALUE = 1,
    FUN = function(i) {
      ll <- object$tree[1:k]
      ll[var1] <- vapply(x[i, var1], as.character, FUN.VALUE = "aaa")
      sum(apply(
        expand.grid(ll),
        MARGIN = 1,
        FUN = function(xx) {
          path_probability(object, as.character(xx), log = FALSE)
        }
      ), na.rm = TRUE)
    }
  )
  res <- res / p1
  if (na0) res[is.na(res)] <- 0
  if (log) {
    return(log(res))
  } else {
    return(res)
  }
}


#' Log-Likelihood of a staged event tree
#'
#' Compute, or extract the log-likelihood of a staged event tree.
#' @param object an fitted object of class \code{sevt}.
#' @param ... additional parameters (compatibility).
#' @return An object of class \code{\link{logLik}}.
#' @importFrom stats logLik
#' @export
#' @examples
#' data("PhDArticles")
#' mod <- indep(PhDArticles)
#' logLik(mod)
logLik.sevt <- function(object, ...) {
  if (!is.null(object$ll)) {
    return(object$ll)
  }
  check_sevt_fit(object)
  vars <- names(object$tree)
  prob <- expand_prob(object)
  ll <- sum(vapply(
    seq_along(object$tree),
    FUN = function(i) {
      if (any(is.nan(prob[[vars[i]]]) &
              object$ctables[[vars[i]]] > 0)) {
        return(-Inf)
      }
      ix <-
        prob[[vars[i]]] > 0 & !is.na(prob[[vars[i]]]) &
        !is.nan(prob[[vars[i]]])
      sum(log(prob[[vars[i]]][ix]) *
            object$ctables[[vars[i]]][ix])
    },
    FUN.VALUE = 1
  ))
  attr(ll, "df") <-
    sum(c(1, vapply(
      object$stages[ vars[-1] ],
      FUN = function(x) {
        length(unique(x))
      },
      FUN.VALUE = 1
    )) *
      (vapply(
        object$tree,
        FUN = length, FUN.VALUE = 1
      ) - 1)) ## compute the degree of freedom
  attr(ll, "nobs") <- sum(object$ctables[[vars[1]]])
  class(ll) <- "logLik"
  return(ll)
}
