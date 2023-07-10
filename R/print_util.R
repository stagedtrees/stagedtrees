#' Tree string
#'
#' @param tree ordered list of variables
#' @keywords internal
tree_string <- function(tree, max){
  ls <- vapply(tree, length, FUN.VALUE = 1)
  mm <- min(max, length(tree))
  vvs <- paste0(names(tree)[1:mm], "[", ls[1:mm], "]")
  if (mm < length(tree)) vvs <- c(vvs, paste0("... [",
                                  length(tree) - mm, " omitted]"))
  paste(vvs, collapse = "->")
}
