#' Compare two staged event tree
#'
#' Compare two staged event trees, return the differences of the stages
#' structure and plot the difference tree. Three different methods to
#' compute the difference tree are available (see Details).
#' @param object1 an object of class \code{sevt}.
#' @param object2 an object of class \code{sevt}.
#' @param method character, method to compare staged event trees. 
#' One of: \code{"naive"}, 
#' \code{"hamming"} or \code{"stages"}.
#' @param return_tree logical, if \code{TRUE} the difference tree is returned.
#' @param plot logical.
#' @param ... additional parameters to be passed to \code{\link{plot.sevt}}.
#' @details \code{compare_stages} tests if the stage structure of two \code{sevt}
#' objects
#' is the same.
#' Three methods are available:
#' * \code{naive} first applies \code{\link{stndnaming}} to both
#' objects and then simply compares the resulting stage names.
#' * \code{hamming} uses the \code{hamming_stages} function that finds
#' a minimal subset of nodes which stages 
#' must be changed to obtain the same structure.
#' * \code{stages} uses the \code{diff_stages} function that compares
#' stages to check whether the same stage structure is present in both models.
#'
#' Setting \code{return_tree = TRUE} will return the stages
#' difference obtained with the selected method.
#' The stages difference is a list of numerical vectors with same 
#' lengths and structure as \code{stages(object1)} or \code{stages(object2)}, 
#' where values are 1 if the corresponding node has different 
#' (with respect to the selected \code{method}) associated stage, and
#' 0 otherwise. 
#'
#' With \code{plot = TRUE} the plot of the difference tree is displayed.
#'
#' If \code{return_tree = FALSE} and \code{plot = FALSE} 
#' the logical output is the same for the
#' three methods and thus the \code{naive} method should be used
#' since it is computationally faster.
#' @return 
#' \code{compare_stages}: if \code{return_tree = FALSE}, logical: \code{TRUE} if the two
#' models are exactly equal, otherwise \code{FALSE}.
#' Else if \code{return_tree = TRUE}, the differences between
#' the two trees, according to the selected \code{method}.
#' @export
#' @examples
#' data("Asym")
#' mod1 <- stages_bhc(full(Asym, lambda = 1))
#' mod2 <- stages_fbhc(full(Asym, lambda = 1))
#' compare_stages(mod1, mod2)
compare_stages <-
  function(object1,
           object2,
           method = "naive",
           return_tree = FALSE,
           plot = FALSE,
           ...) {
    # check and rename stages
    check_sevt(object1)
    check_sevt(object2)
    stopifnot(sevt_nvar(object1) == sevt_nvar(object2))
    stopifnot(all(names(object1$tree) == names(object2$tree)))
    object1 <- stndnaming(object1)
    object2 <- stndnaming(object2)
    # use the appropriate method
    difftree <- switch(
      method,
      naive = sapply(names(object1$tree)[-1],
                     function(v) {
                       as.numeric(object1$stages[[v]] != object2$stages[[v]])
                     },
                     USE.NAMES = TRUE
      ),
      hamming = hamming_stages(object1, object2, return_tree = TRUE),
      stages = diff_stages(object1, object2),
      sapply(names(object1$tree)[-1],
             function(v) {
               as.numeric(object1$stages[[v]] != object2$stages[[v]])
             },
             USE.NAMES = TRUE
      )
    )
    # root is always ok
    tmp <- list()
    tmp[[names(object1$tree)[1]]] <- 0
    difftree <- c(tmp, difftree)
    # plot if required
    if (plot) {
      toplot <- list(tree = object1$tree)
      class(toplot) <- "sevt"
      toplot$stages <- difftree
      for (v in names(toplot$tree)[-1]){
        toplot$stages[[v]][object1$stages[[v]] %in% object1$name_unobserved & 
                             object2$stages[[v]] %in% object2$name_unobserved] <- "UNOBSERVED"
      }
      toplot$name_unobserved <- "UNOBSERVED"
      plot(
        toplot,
        col = function(x) {
          c("1" = "red",
            "0" = 0)
        },
        pch = 16,
        ...
      )
    }
    if (return_tree) {
      return(difftree)
    } else {
      return(all(sapply(difftree, function(x) {
        all(x == 0)
      })))
    }
  }


#' @rdname compare_stages
#' @details 
#' \code{hamming_stages} finds a minimal set of nodes for which the associated stages
#' should be changed to obtain equivalent structures. To do that, a maximum-weight bipartite 
#' matching problem between the stages of the two staged trees is solved using the 
#' Hungarian method implemented in the \code{solve_LSAP} function of the \pkg{clue}
#' package. 
#' \code{hamming_stages} requires the package \code{clue}.
#' @return \code{hamming_stages}: if \code{return_tree = FALSE}, integer, the minimum
#' number of situations where the stage should be changed to obtain the same 
#' models. If \code{return_tree = TRUE} a stages-like structure showing which 
#' situations should be modified to obtain the same models.
#' @export
hamming_stages <- function(object1, object2, return_tree = FALSE) {
  check_sevt(object1)
  check_sevt(object2)
  # check if models are over the same variables, and same order
  stopifnot(sevt_nvar(object1) == sevt_nvar(object2))
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  if (!requireNamespace("clue", quietly = TRUE)) {
    stop("Package \"clue\" needed for this function to work. Please install it.",
         call. = FALSE
    )
  }
  # rename stages with increasing integers
  object1 <- stndnaming(object1)
  object2 <- stndnaming(object2)
  # for each variable match stages to obtain hamming distance
  for (v in names(object1$tree)[-1]) {
    # extract stages vectors
    ss1 <- object1$stages[[v]]
    ss2 <- object2$stages[[v]]
    # and unique stages
    u1 <- unique(ss1)
    u2 <- unique(ss2)
    # build matrix describing bipartite matching problem
    M <- matrix(
      nrow = length(u1),
      ncol = length(u2),
      dimnames = list(u1, u2)
    )
    for (s1 in u1) {
      for (s2 in u2) {
        M[s1, s2] <- sum(ss1 == s1 & ss2 == s2)
      }
    }
    ## solve bipartite matching problem using function in clue package
    if (length(u1) < length(u2)) {
      res <- clue::solve_LSAP(M, maximum = TRUE)
      map <- cbind(u1[seq_along(res)], u2[res]) ## u1 -> u2
      new <- vapply(ss1, function(s) {
        map[map[, 1] == s, 2]
      }, FUN.VALUE = "a")
      object1$stages[[v]] <- new
    } else {
      res <- clue::solve_LSAP(t(M), maximum = TRUE)
      map <- cbind(u2[seq_along(res)], u1[res]) ## u2 -> u1
      new <- vapply(ss2, function(xs) {
        map[map[, 1] == xs, 2]
      }, FUN.VALUE = "a")
      object2$stages[[v]] <- new
    }
  }
  # build the tree of differences
  difftree <- sapply(names(object1$tree)[-1], function(v) {
    as.numeric(object1$stages[[v]] != object2$stages[[v]])
  }, USE.NAMES = TRUE)
  # return tree if required or simply the hamming distance
  if (return_tree) {
    return(difftree)
  } else {
    sum(sapply(difftree, function(x) {
      sum(as.numeric(x), na.rm = TRUE)
    }))
  }
}


#' @rdname compare_stages
#' @return \code{diff_stages}: a stages-like structure marking the situations belonging 
#' to stages which are not the exactly equal.
#' @export
#' @examples
#'
#' ##########
#' m0 <- full(PhDArticles[, 1:4], lambda = 0)
#' m1 <- stages_bhc(m0)
#' m2 <- stages_bj(m0, distance = "totvar", thr = 0.25)
#' diff_stages(m1, m2)
diff_stages <- function(object1, object2) {
  check_sevt(object1)
  check_sevt(object2)
  stopifnot(all(names(object1$tree) == names(object2$tree)))
  out <- rep(list(c()), length(object1$stages))
  attr(out, "names") <- attr(object1$stages, "names")
  for (k in seq_along(object1$stages)) {
    a <- object1$stages[[k]]
    b <- object2$stages[[k]]
    unique_a <- unique(a)
    unique_b <- unique(b)
    out_a <- out_b <- rep(0, length(a))
    for (i in seq_along(unique_a)) {
      ifelse((length(unique(b[which(a == unique_a[i])])) == 1),
             out_a[which(a == unique_a[i])] <- 1,
             out_a[which(a == unique_a[i])] <- 0
      )
    }
    for (i in seq_along(unique_b)) {
      ifelse((length(unique(a[which(b == unique_b[i])])) == 1),
             out_b[which(b == unique_b[i])] <- 1,
             out_b[which(b == unique_b[i])] <- 0
      )
    }
    # stages exactly equal have sign(out_a) + sign(out_b) == 2.
    out[[k]] <- ifelse((sign(out_a) + sign(out_b)) == 2, 0, 1)
  }
  return(out)
}


#' Context specific interventional discrepancy
#' 
#' Compute the context specific interventional discrepeancy 
#' of a staged tree with respect to a reference staged tree.
#' @param object1 an object of class \code{\link{sevt}}.
#' @param object2 an object of class \code{\link{sevt}}.
#' @param FUN a function that is used to aggregate CID for each variable.
#'                  The default \code{mean} will obtain the CID 
#'                  as defined in Leonelli and Varando (2021).
#' @return A list with components:
#' 
#' * \code{wrong} a stages-like structure which record where 
#'           \code{object2} wrongly infer the interventional distance 
#'           with respect to \code{object1}. 
#' * \code{cid} the value of the computed CID.
#'         
#' @examples 
#' model1 <- stages_bhc(full(Titanic))
#' model2 <- stages_bhc(full(Titanic, 
#'                           order = c("Survived", "Sex", "Age", "Class")))
#' cid(model1, model2)$cid
#' cid(model1, model2)$wrong
#' @export
cid <- function(object1, object2, FUN = mean){
  check_sevt(object1)
  check_sevt(object2)
  stopifnot(is.function(FUN))
  vs1 <- names(object1$tree)
  vs2 <- names(object2$tree)
  wrong <- list()
  for (v in vs1){
    if (is.null(object1$stages[[v]])) object1$stages[[v]] <- "1"
    wrong[[v]] <- sapply(object1$stages[[v]], function(x) 0)
    stages2 <- unique(object2$stages[[v]])
    if (is.null(stages2)) stages2 <- "1"
    ## get all paths with stages 
    j1 <- which(names(object1$tree) == v)
    j2 <- which(names(object2$tree) == v)
    both <- intersect(vs1[1:(j1-1)], vs2[1:(j2-1)])
    if (j1 > 1) paths1 <- expand.grid(object1$tree[(j1-1):1])
    if (j1 == 1) paths1 <- NA
    if (is.null(dim(paths1))) dim(paths1) <- c(1,1)
    for (s2 in stages2){
      paths2 <- get_path(object2, v, s2)
      if (is.null(dim(paths2))) dim(paths2) <- c(1,length(paths2))
      whichpaths1 <- c(apply(paths2,1, function(x){
        which(apply(paths1, 1, function(y) all(y[both] == x[both])))
      }))
      stages1 <- object1$stages[[v]][whichpaths1]
      for (id in whichpaths1){
        if (length(unique(stages1)) > 1){
          wrong[[v]][id] <- wrong[[v]][id] + 1   
        }
      }
    }
    wrong[[v]] <- ifelse(wrong[[v]] > 0, 1, 0)
  }
  return(list(wrong = wrong, cid = sum(sapply(wrong, FUN))))
}


