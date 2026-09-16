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
  ## Counts are integers and rowSums returns double; the difference reaches
  ## identical() through the "n" attribute of every fitted probability, so the
  ## original storage mode is recorded here, before any of the steps below can
  ## change it.
  smode <- storage.mode(data)
  ## The cascade below sums out trailing dimensions BY POSITION, so the table's
  ## dimensions must be the tree's variables, in tree order. Neither is
  ## guaranteed: full() takes an `order` argument, a supplied table carries
  ## whatever order it was built with, and the tree may cover only a subset of
  ## the table's variables (search_best fits sub-models of one table). The
  ## previous code selected margins by name and so was insensitive to both.
  ## Permuting the wanted variables to the front and summing out the rest in
  ## one C-level pass restores that, far below the cost of the sweeps replaced.
  dn <- names(dimnames(data))
  if (!identical(dn, order)) {
    extra <- setdiff(dn, order)
    data <- aperm(data, c(order, extra))
    if (length(extra) > 0) data <- rowSums(data, dims = length(order))
  }
  ## The counts for the first i variables are the counts for the first i + 1
  ## summed over variable i + 1, so the whole set is one cascade of partial
  ## sums rather than one full sweep of the joint table per prefix.
  ## rowSums(x, dims = i) sums out every dimension after the i-th, in C.
  ## The previous code called apply(data, MARGIN = order[i:1], sum) once per
  ## prefix, each of which walks the entire joint table through an R-level
  ## closure: p passes instead of one, and the per-cell cost far higher.
  ## Measured on 10 variables with 4 levels, 12.3s -> 0.012s.
  np <- length(order)
  marg <- vector("list", np)
  marg[[np]] <- data
  if (np > 1) {
    for (i in (np - 1):1) marg[[i]] <- rowSums(marg[[i + 1]], dims = i)
  }
  ctables <- lapply(seq_len(np), function(i) {
    tt <- marg[[i]]
    ## counts are integers; rowSums returns double, and the difference is
    ## visible to identical() on every fitted probability's "n" attribute
    storage.mode(tt) <- smode
    if (i == 1) {
      return(tt[!is.na(attr(tt, "names"))])
    }
    ll <- lapply(attr(tt, "dimnames"), function(x) !is.na(x))
    return(ftable(do.call("[", c(list(tt), ll, list(drop = FALSE))),
      col.vars = order[i],
      row.vars = order[1:(i - 1)]
    ))
  })
  names(ctables) <- order
  return(ctables)
}
