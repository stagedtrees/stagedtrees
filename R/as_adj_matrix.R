#' Convert to an adjacency matrix
#' @param x an R object
#' @param ... additional parameters
#' @return the equivalent adjacency matrix
#' @export
as_adj_matrix <- function(x, ...) {
  UseMethod("as_adj_matrix", x)
}


#' @rdname as_adj_matrix
#' @export
as_adj_matrix.parentslist <- function(x, ...) {
  n <- length(x)
  adj <- matrix(
    nrow = n, ncol = n,
    dimnames = list(names(x), names(x)),
    data = 0
  )
  for (i in seq_along(x)) {
    adj[x[[i]]$parents, i] <- 1
  }
  adj
}

#' @rdname as_adj_matrix
#' @param ignore list of stages to be ignored.
#' @param endnode logical value. If \code{TRUE} a final node fil be added.
#' @return for \code{as_adj_matrix.ceg}: the adj matrix corresponding to the CEG.
#' @export
as_adj_matrix.ceg <- function(x, ignore = x$name_unobserved, endnode = TRUE, ...) {
  check_sevt(x)
  tree <- x$tree
  var <- names(tree)
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v) {
    pos[[v]][x$stages[[v]] %in% ignore]
  })
  allignore <- c(unique(unlist(pos.ignore)))
  allpos <- c(unique(unlist(pos)), "END")
  wignore <- allpos %in% allignore
  k <- length(allpos)
  mat <- matrix(nrow = k, ncol = k, 0, dimnames = list(allpos, allpos))
  m <- 1
  for (i in 2:length(var)) {
    v <- var[i - 1]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      for (p2 in pos[[var[i]]][((ix - 1) * m + 1):(ix * m)]) {
        mat[p1, p2] <- mat[p1, p2] + 1
      }
    }
  }
  v <- var[i]
  if (endnode) {
    for (p1 in unique(pos[[v]])) {
      mat[p1, "END"] <- length(x$tree[[v]])
    }
  }
  mat <- mat[!wignore, !wignore]
  return(mat)
}
