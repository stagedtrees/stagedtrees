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