#' Coerce to sevt
#' 
#' Convert to an equivalent object of class \code{\link{sevt}}.
#' 
#' @param x   an R object.
#' @param ... additional parameters to be used by specific methods.
#' @return the equivalent object of class \code{\link{sevt}}.
#' @export
as_sevt <- function(x, ...){
  UseMethod("as_sevt", x)
} 

#' @rdname as_sevt
#' @param order order of the variables.
#' @details In \code{as_sevt.bn.fit} the \code{order} 
#' argument, if provided, must be a topological order of the 
#' \code{bn.fit} object (no check is performed). If the order is not provided 
#' a topological order will be used (the one returned by 
#' \code{bnlearn::node.ordering}).
#' @export
as_sevt.bn.fit <- function(x, order = NULL, ...) {
  as_sevt.parentslist(as_parentslist.bn.fit(x, order = order))
}


#' @rdname as_sevt
#' @export
as_sevt.bn <- function(x, order = NULL, values = NULL, ...){
  as_sevt.parentslist(as_parentslist(x, order = order), values = values, ...)
}


#' @rdname as_sevt
#' @param values the values for each variable, the sample space.
#' @details In \code{as_sevt.parentslist} the \code{order} 
#' argument, if provided, must be a topological order of the 
#' corresponding DAG (no check is performed). 
#' If the order is not provided 
#' \code{names(x)} is used.
#' 
#' The \code{values} parameter is used to specify the sample space 
#' of each variable. For a \code{parentslist} object created with 
#' \code{\link{as_parentslist}} from an object of class \code{sevt},
#' it is, usually, not needed to specify the \code{values} parameter,
#' since the sample space is saved in the \code{parentslist} object.
#' @examples 
#' model <- stages_hclust(full(Titanic), k = 2)
#' plot(model)
#' pl <- as_parentslist(model)
#' model2 <- as_sevt(pl)
#' plot(model2) ## this is a super-model of the first staged tree
#' ## we can check it with
#' inclusions_stages(model, model2)
#' @export
as_sevt.parentslist <- function(x, order = NULL, values = NULL, ...){
  if (is.null(order)){
    order <- names(x)    
  }
  if (is.null(values)){
    values <- lapply(x, function(vv) {
      if (is.null(vv$values)){
        warning("Missing values for a variable, a binary variable is used", call. = FALSE)
        c(0,1)
      } else {
        vv$values
      } 
    })
  }else{ ##combine values with info in the parentslist object
    values <- sapply(names(x), function(nn){
      if (is.null(values[[nn]])){
        if (is.null(x[[nn]]$values)){
          warning("Missing values for a variable, a binary variable is used", call. = FALSE)
          c(0,1)
        } else {
          x[[nn]]$values
        }
      } else {
        values[[nn]]
      }
    }, simplify = FALSE, USE.NAMES = TRUE)
  }
  
  # reorder the list
  values <- values[order]
  # create staged tree from list
  object <- sevt(values, order = order)
  # extract parents
  parents <- lapply(x, function(n) {
    n$parents
  })
  # build stages info respecting conditional 
  # independences depicted in the Bayesian network
  for (i in 2:length(order)) {
    # initialize stages for ith variable 
    stgs <- "1"
    # build stages by iteratively expanding stages along tree 
    for (j in seq(i-1)){
      if (order[j] %in% parents[[i]]){
        ## if  jth variable is a parent of ith expand different 
        ## stages for each value 
        stgs <- as.vector(sapply(stgs, function(x) paste0(x, values[[j]])  ))
      }else{
        ## otherwise replicate the same stages, since ith does not depend on jth
        stgs <- as.vector(sapply(stgs, function(x) rep(x,length(values[[j]]))))
      }
    }
    object$stages[[order[i]]] <- stgs
  }
  object <- stndnaming(object)
  object
}

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
#' @param order order of the variables, usually a topological order.
#' @export
as_parentslist.bn <- function(x, order = NULL, ...){
  # if no order is provided from the user
  # then a topological order is used
  if (is.null(order)){
    order <- bnlearn::node.ordering(x)
  }
  plist <- lapply(x$nodes[order], function(n) list(parents = n$parents))
  class(plist) <- "parentslist"
  plist
}

#' @rdname as_parentslist
#' @export
as_parentslist.bn.fit <- function(x, order = NULL,  ...){
  # if no order is provided from the user
  # then a topological order is used
  if (is.null(order)){
    order <- bnlearn::node.ordering(x)
  }
  plist <- lapply(x[order], function(n) list(parents = n$parents, values = dimnames(n$prob)[[1]]))
  class(plist) <- "parentslist"
  plist
}


#' @rdname as_parentslist
#' @details In `as_parentslist.sevt`, if a context-specific or a local-partial independence is detected
#' a message is printed and the minimal super-model is returned.
#' @examples 
#' model <- stages_hclust(full(Titanic), k = 2)
#' pl <- as_parentslist(model)
#' pl$Age 
#' @export
as_parentslist.sevt <- function(x, ...){
  check_sevt(x)
  wrn <- FALSE
  Ms <- sapply(x$tree, length)
  Vs <- names(x$tree)
  prnt_list <- list()
  prnt_list[[Vs[1]]] <- list(parents = NULL, values = x$tree[[Vs[1]]])
  for (i in seq_along(x$stages)) {
    prn <- character(0)
    cntx <- character(0)
    prtl <- character(0)
    lcl <- character(0)
    stgs <- x$stages[[i]]
    for (j in rev(seq(i))){
      splitd <- matrix(nrow = Ms[j], stgs)
      cnts <- apply(splitd, MARGIN = 2, 
                    FUN = function(xx) length(unique(xx)))
      if (all(cnts == 1)){
        ### it is not a parent      
        stgs <- splitd[1,] ## just take the first since they are all the same
      }else{ ### it is a parent
        if (all(cnts == Ms[j])){
          ### check for local partial independence 
          sR <- sum(apply(splitd, MARGIN = 1, 
                          FUN = function(xx) length(unique(xx))))
          if (sR != length(unique(c(splitd)))){
            wrn <- TRUE
            lcl <- c(lcl, Vs[j])
          }
        }else{
          if (any(cnts == 1)){
            ## we at least one pure context indep.
            cntx <- c(cntx, Vs[j])
          }
          if (any(cnts < Ms[j] & cnts > 1)){
            prtl <- c(prtl, Vs[j])
          }
          wrn <- TRUE
        }
        ## take all rows
        stgs <- c(t(splitd))
        prn <- c(prn, Vs[j])
      }
    }
    prnt_list[[Vs[i + 1]]] <- list(parents = prn, context = cntx, 
                                   partial = prtl, local = lcl, 
                                   stages = stgs,
                                   values = x$tree[[Vs[i + 1]]])
  }
  if (wrn){
    message("Context specific and/or local partial independences detected.")
    message("The input staged tree is not equivalent to a BN, 
            the minimal super-model is returned.")
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
as.character.parentslist <- function(x, only_parents = FALSE, ...){
  if (only_parents){
    paste(sapply(seq_along(x), function(i) {
      paste("[", names(x)[i], ifelse(length(x[[i]]$parents) > 0, "|", ""), 
            paste0(x[[i]]$parents, 
                  collapse = ":"), "]", sep = "")
    }), collapse = "")  
  }else{
    paste(sapply(seq_along(x), function(i) {
      paste("[", names(x)[i], ifelse(length(x[[i]]$parents) > 0, "|", ""), 
            paste0(ifelse(x[[i]]$parents %in%   x[[i]]$partial, "(", ""),
                   ifelse(x[[i]]$parents %in%   x[[i]]$context, "{", ""),
                   ifelse(x[[i]]$parents %in%   x[[i]]$local, "<", ""),
                  x[[i]]$parents, 
                  ifelse(x[[i]]$parents %in%   x[[i]]$local, ">", ""),
                  ifelse(x[[i]]$parents %in%   x[[i]]$context, "}", ""),
                  ifelse(x[[i]]$parents %in%   x[[i]]$partial, ")", ""),
                  collapse = ":"), "]", sep = "")
    }), collapse = "") 
  }
}

#' Print a parentslist object
#' 
#' Nice print of a parentslist object
#' @param x an object of class \code{parentslist}.
#' @param ... additional arguments for compatibility.
#' @export
print.parentslist <- function(x, ...){
  cat(" ", as.character.parentslist(x, ...))
  invisible(x)
}

#' Convert to a \pkg{bnlearn} object
#' 
#' Convert a staged tree object into an object of class \code{bn}
#' from the \pkg{bnlearn} package.
#' @param x an R object of class \code{sevt} or \code{parentslist}.
#' @return an object of class \code{bn} from package \pkg{bnlearn}.
#' @export
as_bn <- function(x){
  UseMethod("as_bn", x)
}

#' @rdname as_bn
#' @export
as_bn.parentslist <- function(x){
  bnlearn::model2network(as.character(x, only_parents = TRUE))
}

#' @rdname as_bn
#' @export
as_bn.sevt <- function(x){
  as_bn.parentslist(as_parentslist.sevt(x))
}


#' Convert to an adjacency matrix
#' @param x an R object
#' @return the equivalent adjacency matrix
#' @export
as_adj_matrix <- function(x){
  UseMethod("as_adj_matrix", x)
}


#' @rdname as_adj_matrix 
#' @export
as_adj_matrix.parentslist <- function(x){
  n <- length(x)
  adj <- matrix(nrow = n, ncol = n, 
                dimnames = list(names(x), names(x)), 
                data = 0)
  for (i in seq_along(x)){
    adj[x[[i]]$parents, i] <- 1
  }
  adj
}