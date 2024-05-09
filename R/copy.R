#' Copy a sevt obejct
#'
#' @param object an object of class \code{sevt}.
#' @param additional arguments.
#' @returns  the copied object.
#' @export
copy_sevt <- function(object, extra = NULL){
  copied <- object[c("tree", "stages")] ## basic
  if (!is.null(extra)){
    extra <- names(extra)
    copied <- c(copied, do.call("[", c(list(object), extra)))
  }
  class(copied) <- class(object)
  return(copied)
}
