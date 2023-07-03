#' Bar plots of stage probabilities
#'
#' Create a bar plot visualizing probabilities associated to the
#' different stages of a variable in a staged event tree.
#' @param height an object of class \code{sevt}.
#' @param var name of a variable in \code{object}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param beside a logical value. See \code{\link{barplot}}.
#' @param horiz a logical value. See \code{\link{barplot}}.
#' @param legend.text logical.
#' @param col color mapping for the stages, see \code{col}
#'        argument in \code{\link{plot.sevt}}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... additional arguments passed to \code{\link{barplot}}.
#' @return As \code{\link{barplot}}:
#'         A numeric vector (or matrix, when beside = TRUE),
#'         giving the coordinates of all the bar midpoints drawn, useful
#'         for adding to the graph.
#' @export
#' @examples
#' model <- stages_fbhc(full(PhDArticles, lambda = 1))
#' barplot(model, "Kids", beside = TRUE)
#' @importFrom graphics barplot
barplot.sevt <- function(height, var,
                         ignore = height$name_unobserved,
                         beside = TRUE,
                         horiz = FALSE,
                         legend.text = FALSE,
                         col = NULL,
                         xlab = ifelse(horiz, "probability", NA),
                         ylab = ifelse(!horiz, "probability", NA),
                         ...){
  check_sevt_prob(height)
  check_var_in(var, height)
  stg <- stages(height, var)
  stg <- stg[!(stg %in% ignore)]
  ustg <- unique(stg)
  col <- make_stages_col(height, col = col, ignore = ignore)[[var]]
  if (!is.null(names(col))){
    if (all(ustg %in% names(col))){
      ## order (and select) colors
      col <- col[ustg]
    }else{
      cli::cli_abort(c(
        "{.arg col} should contains a named (stages) vector of colors.",
        "x" = "You've provided {.arg col} which is named differently from
        stages.",
        "i" = "Use {.fun stagedtrees::make_stages_col} to create valid
        color specification."
      ))
    }
  }
  tmp <- summary(height)[["stages.info"]][[var]]
  if (legend.text){
    legend.text = tmp$stage[tmp$stage %in% ustg]
  }
  hei <- as.matrix(tmp[tmp$stage %in% ustg, -(1:3)])
  hei[is.nan(hei)] <- 0
  barplot(hei, col = col,
          legend.text = legend.text, beside = beside,
          xlab = xlab, ylab = ylab,
          horiz = horiz, ...)
}
