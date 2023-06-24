#' igraph's plotting for CEG 
#' 
#' @param x an object of class \code{\link{ceg}}. 
#' @param col colors specification see \code{\link{plot.sevt}}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @param layout an igraph layout.
#' @param ... additional arguments passed to \code{plot.igraph}.
#' @details This function is a simple wrapper around 
#'  \pkg{igraph}'s \code{plot.igraph}.
#'  The ceg object is converted to an igraph object 
#'  with \code{\link{as_igraph}}.
#'  If not specified, the default \code{layout} used is 
#'  a rotated \code{layout.sugiyama}.
#'  
#'  We use \code{palette()} as palette for
#'  the \pkg{igraph} plotting, while \code{plot.igraph} uses 
#'  as default a different palette. This is to allow matching 
#'  stages colors between \code{plot.ceg} 
#'  and \code{\link{plot.sevt}}.
#' @examples 
#' \dontrun{
#'  model <- stages_bhc(full(Titanic))
#'  model.ceg <- ceg(model)
#'  plot(model.ceg, edge.arrow.size = 0.1, vertex.label.dist = -2)
#'  }
#' @importFrom grDevices palette
#' @importFrom stats na.exclude
#' @export
plot.ceg <- function(x, col = NULL,
                     ignore = x$name_unobserved, 
                     layout = NULL,
                      ...){
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" is needed to plot ceg.",
         call. = FALSE
    )
  }
  nms <- sevt_varnames(x)
  if (is.null(x$stages[[nms[1]]])){ ## add stage name also to root
    x$stages[[nms[1]]] <- c('0')
  }
  ### get colors as in plot.sevt
  col <- make_stages_col(x, col, ignore)
  ### ssociate stages color to node of CEG
  col.pos <- lapply(seq_along(x$positions), function(i){
    upos <- unique(x$positions[[nms[i]]])
    ustag <- x$stages[[nms[i]]][sapply(upos, function(pp) 
      which.max(x$positions[[nms[i]]] == pp))]
    cc <- col[[nms[i]]][ustag]
    if (is.null(cc)) cc <- NA
    names(cc) <- paste0(nms[i], ":", upos)
    return(na.exclude(cc))
  })
  ucol <- unlist(col.pos)
  ucol <- c(ucol,1)
  g <- as_igraph(x, ignore = ignore)
  igraph::V(g)$color <- ucol
  if (is.null(layout)){
    layout = igraph::layout.sugiyama(g)$layout
    layout = layout[,2:1]
    layout[,1] <- -layout[,1]
  }
  igraph::plot.igraph(g, layout = layout, 
                      palette = palette(), ...)
}