#' Context specific interventional discrepancy
#'
#' Compute the context specific interventional discrepeancy
#' of a staged tree with respect to a reference staged tree.
#' @param object1 an object of class \code{\link{sevt}}.
#' @param object2 an object of class \code{\link{sevt}}.
#' @param FUN a function that is used to aggregate CID for each variable.
#'                  The default \code{mean} will obtain the CID
#'                  as defined in Leonelli and Varando (2023).
#' @return A list with components:
#'
#' * \code{wrong} a stages-like structure which record where
#'           \code{object2} wrongly infer the interventional distance
#'           with respect to \code{object1}.
#' * \code{cid} the value of the computed CID.
#'
#' @references
#' Leonelli M., Varando G.
#' _Context-Specific Causal Discovery for Categorical Data Using Staged Trees_,
#'  The 26th International Conference on Artificial Intelligence and Statistics (AISTATS), 2023,
#'  <https://arxiv.org/abs/2106.04416>
#' @examples
#' model1 <- stages_bhc(full(Titanic))
#' model2 <- stages_bhc(full(Titanic,
#'   order = c("Survived", "Sex", "Age", "Class")
#' ))
#' cid(model1, model2)$cid
#' cid(model1, model2)$wrong
#' @export
cid <- function(object1, object2, FUN = mean) {
  check_sevt(object1)
  check_sevt(object2)
  stopifnot(is.function(FUN))
  vs1 <- names(object1$tree)
  vs2 <- names(object2$tree)
  wrong <- list()
  for (v in vs1) {
    if (is.null(object1$stages[[v]])) object1$stages[[v]] <- "1"
    wrong[[v]] <- sapply(object1$stages[[v]], function(x) 0)
    stages2 <- unique(object2$stages[[v]])
    if (is.null(stages2)) stages2 <- "1"
    ## get all paths with stages
    j1 <- which(names(object1$tree) == v)
    j2 <- which(names(object2$tree) == v)
    both <- intersect(vs1[1:(j1 - 1)], vs2[1:(j2 - 1)])
    if (j1 > 1) paths1 <- expand.grid(object1$tree[(j1 - 1):1])
    if (j1 == 1) paths1 <- NA
    if (is.null(dim(paths1))) dim(paths1) <- c(1, 1)
    for (s2 in stages2) {
      paths2 <- get_path(object2, v, s2)
      if (is.null(dim(paths2))) dim(paths2) <- c(1, length(paths2))
      whichpaths1 <- c(apply(paths2, 1, function(x) {
        which(apply(paths1, 1, function(y) all(y[both] == x[both])))
      }))
      stages1 <- object1$stages[[v]][whichpaths1]
      for (id in whichpaths1) {
        if (length(unique(stages1)) > 1) {
          wrong[[v]][id] <- wrong[[v]][id] + 1
        }
      }
    }
    wrong[[v]] <- ifelse(wrong[[v]] > 0, 1, 0)
  }
  return(list(wrong = wrong, cid = sum(sapply(wrong, FUN))))
}
