#' Add a variable to a staged event tree
#'
#' Return an updated staged event tree with one additional
#' variable at the end of the tree.
#' @param object an object of class \code{sevt}.
#' @param var character, the name of the new variable to be added.
#' @param data either a \code{data.frame} or a \code{table} containing
#'             the data from the variables in \code{object} plus \code{var}.
#' @param join_unobserved logical, passed to \code{\link{full}}.
#' @param useNA whether to include NA values in the tables.
#'              Argument passed to \code{\link{table}}.
#' @details This function update a staged event tree object with
#'          an additional variable. The stages structure of the
#'          new variable is initialized as in the saturated model.
#' @return An object of class \code{sevt} representing a
#' staged event tree model with \code{var} added as last variable.
#' @examples
#' model <- full(Titanic, order = c("Age", "Class"))
#' print(model)
#' model <- sevt_add(model, "Survived", Titanic)
#' print(model)
#' @export
#' @importFrom stats ftable
sevt_add <- function(object, var, data, join_unobserved = TRUE,
                     useNA = "ifany") {
  check_sevt(object)
  path <- sevt_varnames(object)
  if (is.data.frame(data)) {
    data <- table(data[, c(path, var)], useNA = useNA)
    ll <- lapply(attr(data, "dimnames"), function(x) !is.na(x))
    data <- do.call("[", c(list(data), ll))
  }
  if (!is.table(data)) {
    cli::cli_abort(c(
      "{.arg data} must be a data.frame or a table object.",
      "x" = "You've supplied {.arg data} which is {.type {data}}."
    ))
  }
  tt <- apply(data, MARGIN = c(path, var), sum)
  ctable <- ftable(tt,
    col.vars = var,
    row.vars = path
  )
  tmp <- sevt(data, full = TRUE, order = c(path, var))
  object$tree <- tmp$tree
  object$stages[[var]] <- tmp$stages[[var]]
  object$ctables[[var]] <- ctable
  object$prob[[var]] <- lapply(seq_along(object$stages[[var]]), function(ix) {
    tt <- ctable[ix, ]
    names(tt) <- object$tree[[var]]
    n <- sum(tt)
    tt <- (tt + object$lambda)
    tt <- tt / sum(tt)
    tt[is.nan(tt)] <- NA
    attr(tt, "n") <- n
    return(tt)
  })
  names(object$prob[[var]]) <- object$stages[[var]]
  if (join_unobserved) {
    ix <- rowSums(ctable) == 0
    if (any(ix)) {
      object$stages[[var]][ix] <- object$name_unobserved[1]
      p.unobserved <- object$prob[[var]][ix][[1]]
      object$prob[[var]][ix] <- NULL
      object$prob[[var]][[object$name_unobserved[1]]] <- p.unobserved
    }
  }
  object$ll <- NULL ## force recompute log-likelihood
  object$ll <- logLik(object)
  return(object)
}
