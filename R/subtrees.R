#' Extract subtree
#'
#' @param object an object of class \code{sevt}.
#' @param path the path from root after which extract the subtree.
#' @details Returns the subtree of the staged event tree, starting from
#' \code{path}.
#' @return A staged event tree object corresponding to the subtree.
#' @examples
#' DD <- generate_random_dataset(4, 100)
#' model <- sevt(DD, full = TRUE)
#' plot(model)
#' model1 <- subtree(model, path = c("-1", "1"))
#' plot(model1)
#' @export
subtree <- function(object, path) {
  m <- 1
  idx <- tree_idx(path, object$tree)
  stage <- find_stage(object, path)
  varout <- sevt_varnames(object)[seq_along(path)]
  object$tree[varout] <- NULL ## remove previous variables
  object$stages[varout] <- NULL ## remove stages info
  object$ctables[varout] <- NULL
  var <- sevt_varnames(object)
  object$stages[[var[1]]] <-
    c(stage) ## keep stage name for first variable
  if (has_ctables(object)) {
    object$ctables[[var[1]]] <- object$ctables[[var[1]]][idx, ]
    attr(object$ctables[[var[1]]], "names") <- object$tree[[var[1]]]
  }
  for (i in 2:length(object$tree)) {
    m <- m * length(object$tree[[var[i - 1]]])
    tmpidx <- ((idx - 1) * m):(idx * m - 1) + 1
    object$stages[[var[i]]] <-
      object$stages[[var[i]]][tmpidx]
    if (has_ctables(object)) { # update ctables
      object$ctables[[var[i]]] <- ftable(object$ctables[[var[i]]][tmpidx, ])
      attr(object$ctables[[var[i]]], "row.vars") <- object$tree[1:(i - 1)]
      attr(object$ctables[[var[i]]], "col.vars") <- object$tree[i]
    }
  }
  if (has_prob(object)) {
    object$prob[varout] <- NULL
    object$prob[[var[1]]] <- object$prob[[var[1]]][stage]
    for (i in 2:length(object$tree)) {
      object$prob[[var[i]]] <-
        object$prob[[var[i]]][unique(object$stages[[var[i]]])]
    }
  }
  object$ll <- NULL
  return(object)
}

#' Extract dependency subtree
#'
#' Extract the dependency subtree of a staged tree with respect to
#' a variable
#' @param object an object of class \code{\link{sevt}}.
#' @param var the name of one of the variable of the staged event tree.
#' @param other_stages how to set stages for other variables (if any).
#' @return an object of class \code{\link{sevt}} representing the
#' dependency sub-tree.
#' @details The dependency sub-tree is a staged event tree which is
#' sufficient to describe the conditional distribution of the variable
#' \code{var} given its predecessors in the original tree represented by
#' \code{object}.
#' In particular the preceding variables are restricted to the
#' parents of \code{var} in the minimal-DAG obtained with
#' \code{\link{as_parentslist}}. This is the minimal set of
#' variables which contexts are sufficient to fully represent the
#' conditional distribution of \code{var}.
#' Stages for variables different from \code{var} are either set to
#' NA, or to the full or indep model, depending on \code{other_stages}.
#' @export
#' @examples
#' mod <- stages_kmeans(full(Titanic), k = 2)
#' par(mfrow = c(1, 2))
#' plot(mod, main = "staged tree")
#' plot(depsubtree(mod, "Age"), main = "dependency subtree for Age")
#' par(mfrow = c(1, 1))
depsubtree <- function(object, var, other_stages = c("NA", "indep", "full")) {
  check_sevt(object)
  other <- match.arg(other_stages)
  pl <- as_parentslist(object, silent = TRUE)
  st <- sevt(object$tree[c(rev(pl[[var]]$parents), var)],
             full = other == "full"
  )
  if (other == "NA") {
    st$stages <- lapply(st$stages, function(x) rep(NA, length(x)))
    st$stages[[sevt_varnames(st)[1]]] <- NA
  }
  st$stages[[var]] <- pl[[var]]$stages
  ## TODO: if has_prob then save relevant probabilities
  st
}