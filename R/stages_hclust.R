#' Learn a staged tree with hierarchical clustering
#'
#' Build a stage event tree with \code{k} stages for each variable by
#' clustering stage probabilities with hierarchical clustering.
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param distance character, the distance measure to be used, either
#'                 a possible `method` for \code{\link{dist}} or
#'                 one of the following: \code{"totvar", "hellinger"}.
#' @param ignore vector of stages which will be ignored and left untouched.
#'               By default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param k integer or (named) vector: number of clusters, that is stages per variable.
#'        Values will be recycled if needed. If \code{NA} (default) a search of
#'        the number of stage is performed with respect to the maximization of
#'        the \code{score} function. \code{NA} and integer can be mixed
#'        to fix the number of stage for some variables and use the
#'        score to select others.
#' @param method the agglomeration method to be used in \code{\link{hclust}}.
#' @param limit the maximum number of variables to consider.
#' @param scope names of the variables to consider.
#' @param score A function. Score to maximize for automatic selection
#'              of the number of stages. Used if \code{k=NA} for some variables.
#' @details \code{hclust_sevt} performs hierarchical clustering
#'          of the initial stage probabilities in \code{object}
#'          and it aggregates them into the specified number
#'          of stages (\code{k}).
#'          A different number of stages for the different variables
#'          in the model can be specified by supplying a (named) vector
#'          via the argument \code{k}.
#'          If \code{k} is \code{NA} for some variables, all
#'          possible number of stages will be checked and the
#'          one that maximize the \code{score} will be selected.
#' @return A staged event tree object.
#' @importFrom stats dist hclust cutree
#' @examples
#' data("Titanic")
#' model <- stages_hclust(full(Titanic, join_unobserved = TRUE, lambda = 1), k = 2)
#' summary(model)
#'
#' ### or search k via BIC minimization
#' model1 <- stages_hclust(full(Titanic), k = NA)
#' @export
stages_hclust <-
  function(object,
           distance = "totvar",
           k = NA,
           method = "complete",
           ignore = object$name_unobserved,
           limit = length(object$tree),
           scope = NULL,
           score = function(x) {
             return(-BIC(x))
           }) {
    check_sevt_fit(object)
    stopifnot(is.character(distance))
    if (is.null(scope)) scope <- sevt_varnames(object)[2:limit]
    stopifnot(all(scope %in% sevt_varnames(object)[-1]))
    if (is.null(names(k))) {
      k <- rep(k, length(scope))[seq_along(scope)]
      names(k) <- scope
    }
    for (v in scope) {
      wch <- names(object$prob[[v]])
      wch <- wch[!(wch %in% ignore)]
      pp <- t(as.matrix(as.data.frame(object$prob[[v]][wch])))
      rownames(pp) <- wch
      M <- switch(distance,
        "totvar" = 0.5 * dist(pp, method = "manhattan"),
        "hellinger" = dist(sqrt(pp), method = "euclidean") / sqrt(2),
        dist(pp, method = distance)
      )
      if (!is.na(k[v])) {
        groups <- cutree(hclust(M, method = method), k = min(k[v], attr(M, "Size")))
      } else {
        hcres <- hclust(M, method = method)
        res <- lapply(1:attr(M, "Size"), function(k) {
          groups <- cutree(hcres, k = k)
          old <- object$stages[[v]]
          object2 <- object
          for (s in 1:k) {
            object2$stages[[v]][old %in% names(which(groups == s))] <- paste0(s)
          }
          object2 <- sevt_fit(object2, scope = v)
          list(score = score(object2), groups = groups)
        })
        k[v] <- which.max(sapply(res, function(r) r$score))
        groups <- res[[k[v]]]$groups
      }
      ### assign stages
      old <- object$stages[[v]]
      for (s in 1:k[v]) {
        object$stages[[v]][old %in% names(which(groups == s))] <- paste0(s)
      }
      object <- sevt_fit(object, scope = v)
    }
    object$call <- sys.call()
    return(object)
  }
