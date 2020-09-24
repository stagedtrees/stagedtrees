#' Compute probability of a path from root
#'
#' Internal function to compute probability of a path. It does not
#' check the validity of the path.
#' @param object a staged event tree object
#' @param x the path, expressed as a character vector containing the sequence of the assumed levels
#' @param log logical, if \code{TRUE} log-probability is returned
#' @return The probability of the given path or its logarithm if \code{log=TRUE}
#' @details it computes the probability of following a given path (\code{x}) starting from the root.
#' Can be a full path from the root to a leaf or a shorter path.
#' @keywords internal
path_probability <-
  function(object, x, log = FALSE) {
    stopifnot(is(object, "sevt"))
    if (!is.null(names(x))) {
      # if it's a named vector just order it
      x <- x[names(object$tree)]
    }
    # start computing the log probability with first variable
    l <- log(object$prob[[1]][[1]][x[1]])
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        # get corresponding stage
        s <- find_stage(object, x[1:(i - 1)])
        # and add log-prob
        l <- l + log(object$prob[[i]][[s]][x[i]])
      }
    }
    # return log prob or prob as requested
    if (log) {
      return(l)
    } else {
      return(exp(l))
    }
  }


#' Compute probabilities for a staged event tree
#'
#' @param object a (fitted) staged event tree object
#' @param x the vector or data.frame of observations
#' @param log logical, if \code{TRUE} log-probabilities are returned
#' @param nan0 logical, if \code{NaN} should be converted to 0
#' @return the probabilities to observe each observation given in \code{x}
#'
#' @details it computes probabilities related to a vector or a data.frame of observations.
#' They can be as an \code{expand.grid} data.frame or a simple subset of the dataset on which
#' the model is estimated.
#' @examples
#' DD <- generate_random_dataset(5, 100)
#' model <- full(DD, lambda = 1)
#' pr <- prob(model, expand.grid(model$tree[c(2, 3, 4)]))
#' sum(pr)
#' prob(model, DD[1:10, ])
#' @export
prob <- function(object, x, log = FALSE, nan0 = TRUE) {
  stopifnot(is(object, "sevt"))
  stopifnot(has_prob(object))
  if (is.null(dim(x))) {
    x <- as.data.frame(t(x))
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
      ))
    }
  )
  if (nan0) res[is.nan(res)] <- 0
  if (log) {
    return(log(res))
  } else {
    return(res)
  }
}


#' Log-Likelihood of a staged event tree
#'
#' Compute, or extract the log-likelihood of a staged event tree.
#' @param object the staged event tree object
#' @param ... additional parameters
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
  stopifnot(is(object, "sevt"))
  stopifnot(has_prob(object))
  stopifnot(!is.null(object$ctables))
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
