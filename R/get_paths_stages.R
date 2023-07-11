#' Get stage or path
#'
#' Utility functions to obtain stages from paths and
#' paths from stages.
#'
#' @param object an object of class \code{sevt}.
#' @param path character vector, the path from root or
#' a two dimensional array where each row is a path
#' from root.
#' @return \code{get_stage} returns
#' the stage name(s)  for given path(s).
#' @examples
#' model <- stages_fbhc(full(PhDArticles))
#' get_stage(model, c("0", "male"))
#' paths <- expand.grid(model$tree[2:1])[, 2:1]
#' get_stage(model, paths)
#' @export
get_stage <- function(object, path) {
  check_sevt(object)
  if (is.null(dim(path))) {
    find_stage(object, path)
  } else {
    apply(path,
      MARGIN = 1,
      function(x) find_stage(object, x)
    )
  }
}


#' @rdname get_stage
#'
#' @param var character, one of the variable in
#'            the staged tree.
#' @param stage character vector, the name
#' of the stages for which the paths should be
#' returned.
#' @return  \code{get_path} returns a
#'         data.frame containing the paths
#'         corresponding to the given stage(s).
#' @examples
#' get_path(model, "Kids", "5")
#' get_path(model, "Gender", "2")
#' get_path(model, "Kids", c("5", "6"))
#' @export
get_path <- function(object, var, stage) {
  check_sevt(object)
  check_var_in(var, object)

  # list all paths
  paths <- expand.grid(object$tree[(which(var == sevt_varnames(object)) - 1):1],
    stringsAsFactors = FALSE
  )
  # extract paths for given stage
  paths <- paths[object$stages[[var]] %in% stage, ncol(paths):1]
  # format to data.frame if var is not the first
  if (var %in% sevt_varnames(object)[2]) {
    paths <- data.frame(paths)
    colnames(paths) <- sevt_varnames(object)[1]
  }
  return(paths)
}
