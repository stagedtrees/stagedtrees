#' Copy a sevt obejct
#'
#' @param object an object of class \code{sevt}.
#' @param extra additional arguments.
#' @returns  the copied object.
#' @export
copy_sevt <- function(object, extra = NULL){
  copied <- object[c("tree", "stages")] ## basic
  if (!is.null(extra)){
    copied[extra] <- object[extra]
  }
  class(copied) <- class(object)
  return(copied)
}
