#' Generate a random (fitted) sevt
#'
#' Generate a random \code{sevt} from a DAG or a tree.
#' Probabilities are also randomly generated.
#'
#' @param x a \code{sevt} object, a \code{parentslist} object or a
#'        \code{list}.
#' @param ... additional parameters passed to specific methods.
#' @return A randomly generated fitted \code{sevt} object.
#' @details The generated staged tree is obtained by randomly
#' joining stages with probability \code{q}.
#' @export
random_sevt <- function(x, ...) {
  UseMethod(generic = "random_sevt", object = x)
}


#' @rdname random_sevt
#' @details For \code{random_sevt.list}, \code{x} should be
#'          a list representing an event tree, same format
#'          as lists provided to \code{\link{sevt.list}}.
#'          The random generated \code{sevt} will be
#'          obtained by randomly joining stages starting from
#'          a full staged event tree.
#' @export
#' @examples
#' model_gt <- random_sevt(list(
#'   X = c("a", "b"), Y = c("c", "d", "e"),
#'   Z = c("1", "2", "3"), W = c("yes", "no")
#' ))
#'
#' ## sample data from model_gt and estimate a staged tree
#' data <- sample_from(model_gt, 100)
#' model_est <- stages_bhc(full(data))
#'
#' ## compare true and estimated model
#' hamming_stages(model_gt, model_est)
#' compare_stages(model_gt, model_est, method = "hamming", plot = TRUE)
random_sevt.list <- function(x, ...) {
  model <- sevt(x, full = TRUE)
  random_sevt.sevt(model, ...)
}

#' @rdname random_sevt
#' @details For \code{random_sevt.parentslist}, \code{x} should be
#'          a \code{\link[=as_parentslist]{parentslist}} object
#'          representing a DAG, this could be obtained with
#'          \code{\link{as_parentslist}} or with
#'          \code{\link{random_parentslist}}.
#'          The random generated \code{sevt} will be
#'          obtained by randomly joining stages starting from
#'          a the staged tree equivalent to the DAG.
#' @export
random_sevt.parentslist <- function(x, ...) {
  model <- as_sevt(x)
  random_sevt.sevt(model, ...)
}

#' @rdname random_sevt
#' @param q probability of joining stages.
#' @param rfun a function which is used to generate random
#'        conditional probabilities associated to each stage.
#' @details For \code{random_sevt.sevt}, \code{x} should be
#'          a \code{\link{sevt}}.
#'          The random generated \code{sevt} will be
#'          obtained by randomly joining stages starting
#'          from the provided sevt object.
#'          Stages (conditional) probabilities are sampled from
#'          the corresponding probability simplex by generating
#'          a vector with the  uder-defined function \code{rfun} and
#'          sequentially normalizing to sum up to one.
#'          Absolute value is applied to assure non-negativity.
#'          The default \code{rfun = rexp} induces a uniform sampling
#'          from the probability simplex.
#' @importFrom stats rexp
#' @export
random_sevt.sevt <- function(x, q = 0.5, rfun = rexp, ...) {
  for (i in seq_along(x$tree)[-1]) {
    v <- names(x$tree)[i]
    for (s in unique(x$stages[[v]])) {
      if (runif(1) < q) {
        x$stages[[v]][x$stages[[v]] == s] <- sample(unique(x$stages[[v]]),
          size = 1
        )
      }
    }
  }
  x$prob <- list()
  x$prob <- lapply(seq_along(x$tree)[-1], function(i) {
    v <- names(x$tree)[i]
    stages <- x$stages[[v]]
    sapply(unique(stages), FUN = function(s) {
      p <- rfun(length(x$tree[[v]]))
      p <- abs(p) / sum(p)
      names(p) <- x$tree[[v]]
      attr(p, "n") <- NA
      return(p)
    }, simplify = FALSE, USE.NAMES = TRUE)
  })
  p <- rfun(length(x$tree[[1]]))
  p <- abs(p) / sum(p)
  names(p) <- x$tree[[1]]
  attr(p, "n") <- NA
  x$prob <- c(list(list("1" = p)), x$prob)
  names(x$prob) <- names(x$tree)
  return(x)
}
