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
    check_same_tree(object1, object2)
    object1 <- stndnaming(object1)
    object2 <- stndnaming(object2)
    # use the appropriate method
    difftree <- switch(method,
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
      for (v in names(toplot$tree)[-1]) {
        toplot$stages[[v]][object1$stages[[v]] %in% object1$name_unobserved &
          object2$stages[[v]] %in% object2$name_unobserved] <- "UNOBSERVED"
      }
      toplot$name_unobserved <- "UNOBSERVED"
      plot(
        toplot,
        col = function(x) {
          c(
            "1" = "red",
            "0" = 0
          )
        },
        pch = 16,
        ...
      )
    }
    if (return_tree) {
      return(difftree)
    } else {
      return(all(vapply(difftree, function(x) {
        all(x == 0)
      }, FUN.VALUE = TRUE)))
    }
  }


#' @rdname compare_stages
#' @param FUN a function that is used to aggregate the Hamming distance
#'            for each variable. The default \code{sum} produces
#'            the traditional Hamming distance.  Use \code{mean}
#'            to obtain the normalized Hamming distance.
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
hamming_stages <- function(object1, object2, return_tree = FALSE, FUN = sum) {
  check_sevt(object1)
  check_sevt(object2)
  # check if models are over the same variables, and same order
  check_same_tree(object1, object2)
  if (!requireNamespace("clue", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg clue} needs to be installed
      for {.fun stagedtrees::hamming_stages} to work.",
      "x" = "Can't load package {.pkg clue}."
    ))
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
    sum(vapply(difftree, function(x) {
      FUN(as.numeric(x))
    }, FUN.VALUE = 1.0))
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
  check_same_tree(object1, object2)
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
