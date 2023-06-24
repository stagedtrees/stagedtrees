#'  Standard renaming of stages
#'
#' Rename all stages in a staged event tree.
#' @param object an object of class \code{sevt}.
#' @param uniq logical, if stage numbers should be unique over all tree.
#' @param prefix logical, if stage names should be prefixed with variable name.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @return a staged event tree object with stages named with
#' consecutive integers.
#' @examples
#' model <- stages_fbhc(full(PhDArticles, join_unobserved = TRUE))
#' model$stages
#' model1 <- stndnaming(model)
#' model1$stages
#'
#' ### unique stage names in all tree
#' model2 <- stndnaming(model, uniq = TRUE)
#' model2$stages
#'
#' ### prefix stage names with variable name
#' model3 <- stndnaming(model, prefix = TRUE)
#' model3$stages
#'
#' ### manuallty select stage names left untouched
#' model4 <- stndnaming(model, ignore = c("2", "6"), prefix = TRUE)
#' model4$stages
#' @export
stndnaming <- function(object, uniq = FALSE,
                       prefix = FALSE,
                       ignore = object$name_unobserved) {
  var <- names(object$tree)
  start <- 0
  for (i in 2:length(var)) {
    v <- var[i]
    old <- unique(object$stages[[v]])
    old <- old[!(old %in% ignore)]
    if (length(old) > 0) {
      new <- as.character(start + (seq_along(old)))
      if (prefix) new <- paste0(v, new)
      if (uniq) start <- start + length(old)
      object$stages[[v]] <- vapply(object$stages[[v]], function(s) {
        if (s %in% ignore) {
          return(s)
        }
        new[which(old %in% s, useNames = FALSE)]
      }, FUN.VALUE = "a", USE.NAMES = FALSE)
      if (is_fitted_sevt(object)) {
        object$prob[[v]][new] <- object$prob[[v]][old]
        object$prob[[v]][old[!(old %in% new)]] <-
          NULL ## remove old prob
      }
    }
  }
  if (is_fitted_sevt(object)) {
    object$prob[[var[1]]] <- list("1" = object$prob[[var[1]]][[1]])
  }
  return(object)
}
