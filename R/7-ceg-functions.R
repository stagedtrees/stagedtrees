#' Creates the CEG starting from a staged tree
#'
#' @param object a staged event tree
#' @details A ceg object is a staged event tree object with
#'          additional information on the positions.
#' @return a ceg object
#' @examples
#' DD <- generate_xor_dataset(3, 100)
#' model <- bhc.sevt(full(DD))
#' ceg <- ceg.sevt(model)
#' ceg$positions
#' @export
ceg.sevt <- function(object) {
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
#' @param x the ceg object
#' @details it is used to prepare the plot of the ceg.
#' It transforms the ceg object into an adjacent matrix.
#' @return the adj matrix
#' @examples
#' DD <- generate_xor_dataset(3, 100)
#' model <- bhc.sevt(full(DD))
#' ceg <- ceg.sevt(model)
#' ceg2adjmat(ceg)
#' @export
ceg2adjmat <- function(x) {
  tree <- x$tree
  var <- names(tree)
  pos <- uni_idx(x$positions)
  allpos <- c(unique(unlist(pos)), "END")
  k <- length(allpos)
  mat <- matrix(nrow = k, ncol = k, 0, dimnames = list(allpos, allpos))
  m <- 1
  for (i in 2:length(var)) {
    v <- var[i - 1]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      for (p2 in pos[[var[i]]][ ((ix - 1) * m + 1):(ix * m)]) {
        mat[p1, p2] <- mat[p1, p2] + 1
      }
    }
  }
  v <- var[i]
  for (p1 in unique(pos[[v]])) {
    mat[p1, "END"] <- length(x$tree[[v]])
  }
  return(mat)
}
