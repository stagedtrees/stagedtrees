#' Learn a staged tree with hierarchical clustering
#'
#' Build a stage event tree with \code{k} stages for each variable by
#' clustering stage probabilities with hierarchical clustering.
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param distance character, the distance measure to be used, either
#'                 a possible `method` for \code{\link{dist}} or
#'                 one of the following: \code{"totvar", "hellinger"}.
#'                 Alternatively, a function which compute a distance
#'                 matrix (see Details).
#' @param ignore vector of stages which will be ignored and left untouched.
#'               By default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param k integer or (named) vector: number of clusters, that is stages per variable.
#'        Values will be recycled if needed. If \code{NA} (default) a search of
#'        the number of stage is performed with respect to the maximization of
#'        the \code{score} function. \code{NA} and integer can be mixed
#'        to fix the number of stage for some variables and use the
#'        score to select others.
#' @param max_k integer, maximum number of stages to consider per variable.
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
#'          A custom distance can be passed as a function in argument \code{distance}.
#'          This must resturn an object of class \code{"dist"} similarly to \code{\link{dist}}.
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
           max_k = Inf,
           method = "complete",
           ignore = object$name_unobserved,
           limit = length(object$tree),
           scope = NULL,
           score = function(x) {
             return(-BIC(x))
           }) {
    check_sevt_fit(object)
    if (!is.character(distance) & !is.function(distance)) {
      cli::cli_abort(c(
        "{.arg distance} should be a character string or a function.",
        "x" = "You've supplied {.type {distance}}.",
        "i" = "Possible available distances are: {.val totvar},
        {.val hellinger} or any possible value for {.arg method} in
        {.fun stats::dist}."
      ))
    }
    if (!all(is.na(k)) && !is.numeric(k)) {
      cli::cli_abort(c(
        "{.arg k} should be an integer scalar, {.val NA} or a
        (possibly named) vector with integers and {.val NA} values.",
        "x" = "You've supplied {.type {k}}."
      ))
    }
    if (!requireNamespace("fastcluster", quietly = TRUE)) {
      hclu <- fastcluster::hclust
    }else{
      hclu <- hclust
    }
    if (is.null(scope)) scope <- sevt_varnames(object)[2:limit]
    check_scope(scope, object)
    if (is.null(names(k))) {
      k <- rep(k, length(scope))[seq_along(scope)]
      names(k) <- scope
    }
    for (v in scope) {
      wch <- names(object$prob[[v]])
      wch <- wch[!(wch %in% ignore)]
      if (length(wch) < 2) next
      pp <- t(as.matrix(as.data.frame(object$prob[[v]][wch])))
      rownames(pp) <- wch
      if (is.function(distance)){
        M <- distance(pp)
      }else{
        M <- switch(distance,
                    "totvar" = 0.5 * dist(pp, method = "manhattan"),
                    "hellinger" = dist(sqrt(pp), method = "euclidean") / sqrt(2),
                    dist(pp, method = distance)
        )
      }
      if (!is.na(k[v])) {
        groups <- cutree(hclu(M, method = method), k = min(k[v], attr(M, "Size"), max_k))
      } else {
        hcres <- hclu(M, method = method)
        res <- lapply(1:min(attr(M, "Size"), max_k), function(k) {
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
    object$call <- match.call()
    return(object)
  }
