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
  vars <- names(object$tree)
  positions <- object$stages
  ls <- length(object$tree)
  if (ls >= 2) {
    old <- unique(object$stages[[vars[ls]]])
    new <- paste0(seq_along(old))
    positions[[vars[ls]]] <- vapply(object$stages[[vars[ls]]],
      FUN = function(s) {
        new[which(old == s, useNames = FALSE)]
      },
      FUN.VALUE = "a", USE.NAMES = FALSE
    )
  }
  if (ls > 2) {
    for (i in (ls - 1):2) {
      nv <- length(object$tree[[i]])
      positions[[vars[i]]][] <- NA
      positions[[vars[i]]][1] <- 1
      temp <- 2
      for (j in 2:length(positions[[vars[i]]])) {
        nj <- (nv * (j - 1) + 1):(nv * j)
        for (k in 1:(j - 1)) {
          nk <- (nv * (k - 1) + 1):(nv * k)
          if (object$stages[[vars[i]]][j] == object$stages[[vars[i]]][k] &&
            all(positions[[vars[i + 1]]][nj] == positions[[vars[i + 1]]][nk])) {
            positions[[vars[i]]][j] <- positions[[vars[i]]][k]
          }
        }
        if (is.na(positions[[vars[i]]][j])) {
          positions[[vars[i]]][j] <- paste0(temp)
          temp <- temp + 1
        }
      }
    }
  }
  positions[[vars[1]]] <- "1"
  object$positions <- positions
  object$positions <- object$positions[vars]
  class(object) <- c("ceg", class(object))
  return(object)
}
