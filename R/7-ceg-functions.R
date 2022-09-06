#' Chain event graph (CEG)
#'
#' Build the CEG representation from an object of class \code{\link{sevt}}.
#' @param object an object of class \code{sevt}.
#' @details An object of class \code{ceg} is a staged event tree object with
#'          additional information on the positions.
#' @return an object of class \code{ceg}.
#' @examples
#' DD <- generate_xor_dataset(3, 100)
#' model <- stages_bhc(full(DD))
#' model.ceg <- ceg(model)
#' model.ceg$positions
#' @export
ceg <- function(object) {
  check_sevt(object)
  positions <- object$stages
  ls <- length(object$stages)
  us <- unique(object$stages[[ls]])
  positions[[ls]] <- vapply(object$stages[[ls]],
    FUN.VALUE = "a",
    FUN = function(s) {
      as.character(which(us %in% s))
    }, USE.NAMES = FALSE
  )
  for (i in (ls - 1):1) {
    temp <- 2
    nv <- length(object$tree[[i + 1]])
    positions[[i]][] <- NA
    positions[[i]][1] <- 1
    for (j in 2:length(positions[[i]])) {
      nj <- (nv * (j - 1) + 1):(nv * j)
      for (k in 1:(j - 1)) {
        nk <- (nv * (k - 1) + 1):(nv * k)
        if (object$stages[[i]][j] == object$stages[[i]][k] &&
          all(positions[[i + 1]][nj] == positions[[i + 1]][nk])) {
          positions[[i]][j] <- positions[[i]][k]
        }
      }
      if (is.na(positions[[i]][j])) {
        positions[[i]][j] <- temp
        temp <- temp + 1
      }
    }
  }
  positions[[names(object$tree)[1]]] <- "1"
  object$positions <- positions
  object$positions <- object$positions[c(ls + 1, 1:ls)]
  class(object) <- c("ceg", class(object))
  return(object)
}



#' Ceg to adjmat of graph
#'
#' Obtain the adjacency matrix corresponding to a CEG.
#' @param x an object of class \code{\link{ceg}}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @return the adj matrix
#' @details This utility function can be used to prepare the adjacency 
#' matrix to plot the CEG using a graph package (e.g. \pkg{igraph}).
#' @examples
#' model <- stages_fbhc(full(PhDArticles))
#' model.ceg <- ceg(model)
#' ceg2adjmat(model.ceg)
#' @export
ceg2adjmat <- function(x, ignore = x$name_unobserved) {
  tree <- x$tree
  var <- names(tree)
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v){
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
  for (p1 in unique(pos[[v]])) {
    mat[p1, "END"] <- length(x$tree[[v]])
  }
  mat <- mat[!wignore, !wignore]
  return(mat)
}
