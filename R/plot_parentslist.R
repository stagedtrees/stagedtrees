#' igraph plotting of DAGs (parentslist)
#'
#' @param x an R object of calss \code{parentslist}.
#' @param col specification for the colors of the edge type.
#' @param ... additional parameters passed to \code{plot.igraph}.
#' @details This function is a simple wrapper around
#'  \pkg{igraph}'s \code{plot.igraph}.
#'  The \code{parentslist} object is converted to an igraph object
#'  with \code{\link{as_igraph}} and then passed to \code{plot.igraph}.
#'  Additional arguments \code{...} are passed to the \pkg{igraph}
#'  plotting function, allowing customization.
#'
#' @examples
#'  \dontrun{
#'  model <- stages_bhc(full(Titanic))
#'  pl <- as_parentslist(model, silent = TRUE)
#'  plot(model, edge.label = NA)
#'  }
#' @export
plot.parentslist <- function(x, col = c("context" = "red",
                                        "context-partial" = "orange",
                                        "partial" = "blue",
                                        "local" =  "purple",
                                        "total" = "black"), ...){

  ## check if igraph is available
  if (!requireNamespace("igraph", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg igraph} is needed to plot objects of class {.cls parentslist}.",
      "x" = "Unabe to load package {.pkg igraph}.",
      "i" = "You can install {.pkg igraph} with
      {.run install.packages('igraph')}"
    ))
  }

  ## get the igraph translation
  G <- as_igraph(x)

  ## map colors
  igraph::E(G)$color <- as.character(factor(igraph::E(G)$label, levels = c("context",
                                                              "context-partial",
                                                              "partial",
                                                              "local",
                                                              "total"), labels = col))

  igraph::plot.igraph(G, ...)
}
