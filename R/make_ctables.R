#' Distribute counts along tree
#'
#' Create the list of \code{ftable}s
#' storing the observations distributed along
#' the path of the tree.
#' @param object A stratified event tree, a list with a \code{tree} field.
#' @param data table or data.frame containing observations
#'             of the variable in \code{object}.
#' @param useNA whether to include NA values in the tables.
#'              Argument passed to \code{\link{table}}.
#' @return  A list of \code{ftable}s.
#' @details Distribute the counts along the event tree.
#'          This is an internal function, the user will
#'          usually just directly fit the staged event tree
#'          model using \code{sevt.fit}.
#'          We refer here to stratified event tree, because actually
#'          the stage information is never used and thus this function
#'          will work for an object with only a \code{tree} field.
#' @keywords internal
#' @importFrom stats ftable
make_ctables <- function(object, data, useNA = "ifany") {
  order <- names(object$tree)
  if (is.data.frame(data)) {
    data <- table(data[, order], dnn = order, useNA = useNA)
  }
  if (!is.table(data)){
    cli::cli_abort(c(
      "{.arg data} must be a {.cls table}",
      "x" = "Supplied {.arg data} is a {.type {data}}."
    ))
  }
  ctables <- lapply(seq_along(order), function(i) {
    path <- order[i:1]
    tt <- apply(data, MARGIN = path, sum)
    if (i == 1) {
      return(tt[!is.na(attr(tt, "names"))])
    }
    ll <- lapply(attr(tt, "dimnames"), function(x) !is.na(x))
    return(ftable(do.call("[", c(list(tt), ll)),
      col.vars = order[i],
      row.vars = order[1:(i - 1)]
    ))
  })
  names(ctables) <- order
  return(ctables)
}
