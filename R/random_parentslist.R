#' Generate a random \code{parentslist} object (DAG)
#'
#' generate a random DAG coded as
#' \code{\link[=as_parentslist]{parentslist}} object.
#' @param n number of variables.
#' @param k maximum number of levels for each variable.
#' @param maxp maximum cardinality of parents sets.
#' @details For each variable a subset of random cardinality
#'          (maximum \code{maxp}) of the preceding
#'          variables is randomly selected as parents set.
#'          The possible levels of each variables are randomly selected
#'          in \code{2,...,k}.
#' @return a \code{\link[=as_parentslist]{parentslist}} object.
#' @examples
#' random_parentslist(5, 3, 2)
#'
#' ## we can generate the associated staged tree
#' pl <- random_parentslist(4, 2, 2)
#' plot(as_sevt(pl), main = as.character(pl))
#' @export
random_parentslist <- function(n, k = 2, maxp = n) {
  pl <- lapply(1:n, function(x) {
    list(
      parents = c(),
      values = paste0(seq_len(1 + sample(k - 1, 1)))
    )
  })
  for (i in 2:n) {
    pl[[i]]$parents <- paste0("X", sample(1:(i - 1),
      size = sample(min(maxp, i - 1), 1)
    ))
  }
  names(pl) <- paste0("X", 1:n)
  class(pl) <- "parentslist"
  pl
}
