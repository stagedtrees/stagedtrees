#' Obtain the equivalent DAG as list of parents
#'
#' Convert to the equivalent representation as list of parents.
#' @param x an R object.
#' @param ... additional parameters.
#' @details The output of this function is an object of class
#' \code{parentslist} which is one of the possible encoding for
#' a directed graph. This is mainly an internal class and its
#' specification can be changed in the future.
#' For example, now it may also include information on the
#' sample space of the variables and the context/partial/local
#' independences.
#'
#' @return An object of class \code{parentslist} for which a
#' print method exists.
#' Basically a list with
#' one entries for each variable with fields:
#' * \code{parents} The parents of the variable.
#' * \code{context} Where context independences are detected.
#' * \code{partial} Where partial independences are detected.
#' * \code{local} Where no context/partial independences are detected,
#'                but local independences are present.
#' * \code{values} values for the variable.
#' @seealso \code{\link{print.parentslist}} and
#' \code{\link{as.character.parentslist}} for the parenthesis-encoding of the
#' DAG structure and the asymmetric independences.
#' @export
as_parentslist <- function(x, ...){
  UseMethod("as_parentslist", x)
}

#' @rdname as_parentslist
#' @details \code{parentslist()} is a simple wrapper of \code{as_parentslist.list()}.
#' In particular \code{parentslist(...)} is equivalent to \code{as_parentslist(list(...))}.
#' @export
parentslist <- function(...){
  as_parentslist.list(list(...))
}


#' @rdname as_parentslist
#' @export
as_parentslist.list <- function(x, ...){
  order <- topo_sort_parentslist(x)
  x <- x[order] ## order the list
  class(x) <- "parentslist"
  return(x)
}

#' @rdname as_parentslist
#' @param order order of the variables, usually a topological order.
#' @export
as_parentslist.bn <- function(x, order = NULL, ...) {

  ## create a list of parents
  plist <- lapply(x$nodes, function(n) list(parents = n$parents))
  # if no order is provided from the user
  # then a topological order is used
  if (!is.null(order)) {
    plist <- plist[order]
  }
  as_parentslist.list(plist)
}

#' @rdname as_parentslist
#' @export
as_parentslist.bn.fit <- function(x, order = NULL, ...) {

  ## create a list of parents
  plist <- lapply(x, function(n) list(parents = n$parents, values = dimnames(n$prob)[[1]]))

  # if no order is provided from the user
  # then a topological order is used
  if (!is.null(order)) {
    plist <- plist[order]
  }
  as_parentslist.list(plist)
}


#' @rdname as_parentslist
#' @param silent if function should be silent.
#' @details In `as_parentslist.sevt`, if a context-specific or a local-partial independence is detected
#' a message is printed (if \code{silent = FALSE}) and the minimal super-model is returned.
#' @examples
#' model <- stages_hclust(full(Titanic), k = 2)
#' pl <- as_parentslist(model)
#' pl$Age
#' @export
as_parentslist.sevt <- function(x, silent = FALSE, ...) {
  check_sevt(x)
  wrn <- FALSE
  Ms <- sapply(x$tree, length)
  Vs <- names(x$tree)
  prnt_list <- list()
  prnt_list[[Vs[1]]] <- list(parents = NULL, values = x$tree[[Vs[1]]])
  for (i in seq_along(x$tree[-1])) {
    prn <- character(0)
    cntx <- character(0)
    prtl <- character(0)
    lcl <- character(0)
    var <- Vs[i + 1]
    stgs <- x$stages[[var]]
    for (j in rev(seq(i))) {
      splitd <- matrix(nrow = Ms[j], stgs)
      cnts <- apply(splitd,
        MARGIN = 2,
        FUN = function(xx) length(unique(xx))
      )
      if (all(cnts == 1)) {
        ### it is not a parent
        stgs <- splitd[1, ] ## just take the first since they are all the same
      } else { ### it is a parent
        if (all(cnts == Ms[j])) {
          ### check for local partial independence
          sR <- sum(apply(splitd,
            MARGIN = 1,
            FUN = function(xx) length(unique(xx))
          ))
          if (sR != length(unique(c(splitd)))) {
            wrn <- TRUE
            lcl <- c(lcl, Vs[j])
          }
        } else {
          if (any(cnts == 1)) {
            ## we at least one pure context indep.
            cntx <- c(cntx, Vs[j])
          }
          if (any(cnts < Ms[j] & cnts > 1)) {
            prtl <- c(prtl, Vs[j])
          }
          wrn <- TRUE
        }
        ## take all rows
        stgs <- c(t(splitd))
        prn <- c(prn, Vs[j])
      }
    }
    prnt_list[[Vs[i + 1]]] <- list(
      parents = prn, context = cntx,
      partial = prtl, local = lcl,
      stages = stgs,
      values = x$tree[[Vs[i + 1]]]
    )
  }
  if (wrn && !silent) {
    cli::cli_warn(c("Context specific and/or local
                  partial independences detected.",
      "!" = "The input staged tree is not equivalent to a BN,
            a minimal super-model is returned.",
      "i" = "You can silence this worning by setting
            {.code silent = TRUE} in {.fun stagedtrees::as_parentslist}"
    ))
  }
  class(prnt_list) <- "parentslist"
  prnt_list
}

#' @rdname print.parentslist
#' @param x an object of class \code{parentslist}.
#' @param only_parents logical, if the basic DAG encoding is to be returned.
#' @param ... additional arguments for compatibility.
#' @return \code{as.character.parentslist} returns a string
#'         encoding the associated directed graph and eventually
#'         the context specific independences.
#'         The encoding is similar to the one returned by
#'         \code{modelstring} in package \pkg{bnlearn}
#'         and package \pkg{deal}.
#'         In particular, parents of a variable can be enclosed in:
#' * \code{( )} if a partial (conditional) independence is present.
#' * \code{{ }} if a context specific independence is present.
#' * \code{< >} if no context specific and partial (conditional)
#'                      independences are present, but at least a
#'                      local independence is detected.
#'
#' If a parent is not enclosed in parenthesis the dependence is full.
#'
#' If \code{only_parents = TRUE}, the simple DAG encoding as in \pkg{bnlearn}
#' is returned.
#' @examples
#' model <- stages_hclust(full(Titanic), k = 2)
#' pl <- as_parentslist(model)
#' pl
#' as.character(pl)
#' as.character(pl, only_parents = TRUE)
#' @export
as.character.parentslist <- function(x, only_parents = FALSE, ...) {
  if (only_parents) {
    paste(sapply(seq_along(x), function(i) {
      paste("[", names(x)[i], ifelse(length(x[[i]]$parents) > 0, "|", ""),
        paste0(x[[i]]$parents,
          collapse = ":"
        ), "]",
        sep = ""
      )
    }), collapse = "")
  } else {
    paste(sapply(seq_along(x), function(i) {
      paste("[", names(x)[i], ifelse(length(x[[i]]$parents) > 0, "|", ""),
        paste0(ifelse(x[[i]]$parents %in% x[[i]]$partial, "(", ""),
          ifelse(x[[i]]$parents %in% x[[i]]$context, "{", ""),
          ifelse(x[[i]]$parents %in% x[[i]]$local, "<", ""),
          x[[i]]$parents,
          ifelse(x[[i]]$parents %in% x[[i]]$local, ">", ""),
          ifelse(x[[i]]$parents %in% x[[i]]$context, "}", ""),
          ifelse(x[[i]]$parents %in% x[[i]]$partial, ")", ""),
          collapse = ":"
        ), "]",
        sep = ""
      )
    }), collapse = "")
  }
}

#' Print a parentslist object
#'
#' Nice print of a parentslist object
#' @param x an object of class \code{parentslist}.
#' @param ... additional arguments for compatibility.
#' @export
print.parentslist <- function(x, ...) {
  cat(" ", as.character.parentslist(x, ...))
  invisible(x)
}

#' find roots in graphs
#'
#' @param x a graph encoded as list of parents.
#' @return nodes with 0 incoming edges, that is with 0 in-degree.
find_roots <- function(x){
  vs <- names(x)
  parents <- vapply(vs, function(v) sum(x[[v]]$parents %in% vs) == 0,
                    FUN.VALUE = TRUE, USE.NAMES = TRUE)
  names(which(parents))
}

#' Topological sorting of parentslist
#'
#' @param x an object of class \code{parentslist},
#'          or more generally a list of nodes with parents information.
#' @param all, logical if \code{TRUE} all topological orders will be generated with
#'            function \code{all_topo_sort}.
#' @details This function will return a topological order of the graph encoded in
#' \code{x} if the graph is a DAG, otherwise a partial order will be returned.
#' @export
#' @examples
#' pl <- list(A = list(), B = list(), C = list(parents = c("A", "B")),
#' D = list(parents = c("C")),
#' E = list(parents = c("A")),
#' F = list(parents = c("B", "E")))
#' topo_sort_parentslist(pl)
topo_sort_parentslist <- function(x, all = FALSE){
  if (all){
    return(all_topo_sort(x))
  }
  L <- c()
  S <- find_roots(x)
  while (length(S) > 0){
    v <- S[1]
    S <- S[-1]
    L <- c(L, v)
    x[[v]] <- NULL
    S <- find_roots(x)
  }
  return(L)
}

#' @rdname topo_sort_parentslist
#'
#' @returns a matrix where every row is a topological ordeing of the DAG.
#' @details
#' Object \code{x} should represent a DAG, otherwise if cycles are present,
#' the output will not be complete orders of the graph.
#' @export
#' @examples
#' pl <- list(A = list(), B = list(), C = list(parents = c("A", "B")),
#' D = list(parents = c("C")),
#' E = list(parents = c("A")),
#' F = list(parents = c("B", "E")))
#' all_topo_sort(pl)
all_topo_sort <- function(x){
  if (length(x) == 1) return(matrix(names(x), nrow = 1, ncol = 1))
  S <- find_roots(x)
  do.call("rbind" , lapply(S, function(v){
    xx <- x
    xx[[v]] <- NULL
    cbind(c(v), all_topo_sort(xx))
  }))
}
