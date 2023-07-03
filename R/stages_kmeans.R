#' Learn a staged tree with k-means clustering
#'
#' Build a stage event tree with \code{k} stages for each variable
#' by clustering (transformed) probabilities with k-means.
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param k integer or (named) vector: number of clusters, that is stages per variable.
#'          Values will be recycled if needed.
#' @param algorithm character: as in \code{\link{kmeans}}.
#' @param transform function applied to the probabilities before clustering.
#' @param limit the maximum number of variables to consider.
#' @param scope names of the variables to consider.
#' @param nstart as in \code{\link{kmeans}}
#' @details \code{kmenas_sevt} performs k-means clustering
#' to aggregate the stage probabilities of the initial
#' staged tree \code{object}.
#' Different values for k can be specified by supplying a
#' (named) vector to \code{k}.
#' \code{\link{kmeans}} from the \code{stats} package is used
#' internally and arguments \code{algorithm} and \code{nstart}
#' refer to the same arguments as \code{\link{kmeans}}.
#' @return A staged event tree.
#' @importFrom stats kmeans
#' @examples
#' data("Titanic")
#' model <- stages_kmeans(full(Titanic, join_unobserved = TRUE, lambda = 1), k = 2)
#' summary(model)
#' @export
stages_kmeans <- function(object,
                          k = length(object$tree[[1]]),
                          algorithm = "Hartigan-Wong",
                          transform = sqrt,
                          ignore = object$name_unobserved,
                          limit = length(object$tree),
                          scope = NULL,
                          nstart = 1) {
  check_sevt_fit(object)
  if (!(is.function(transform) || is.null(transform))) {
    cli::cli_abort(c(
      "{.arg transform} must be a {.cls function} or {.val NULL} (for the identity).",
      "x" = "You've supplied {.type {transform}}"
    ))
  }
  if (!is.numeric(k)) {
    cli::cli_abort(c(
      "{.arg k} should be an integer scalar or a
        (possibly named vector of integers.",
      "x" = "You've supplied {.type {k}}."
    ))
  }
  if (is.null(transform)) {
    transform <- function(x) {
      return(x)
    }
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
    pp <- transform(t(as.matrix(as.data.frame(object$prob[[v]][wch]))))
    rownames(pp) <- wch
    if (nrow(pp) > k[v]) {
      groups <- kmeans(pp,
        centers = min(k[v], nrow(pp) - 1),
        algorithm = algorithm, nstart = nstart
      )$cluster
      ### remove probabilitites and assign stages
      object$prob[[v]] <- list()
      old <- object$stages[[v]]
      for (s in 1:k[v]) {
        object$stages[[v]][old %in% names(which(groups == s))] <- paste0(s)
      }
    }
    object <- sevt_fit(object, scope = v, compute_logLik = FALSE)
  }
  object$ll <- logLik(object)
  object$call <- match.call()
  return(object)
}
