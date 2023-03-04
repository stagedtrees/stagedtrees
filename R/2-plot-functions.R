#' Plot method for staged event trees
#'
#' Plot method for staged event tree
#' objects. It allows easy plotting of staged event trees with some
#' options (see Examples).
#' @param x an object of class \code{sevt}.
#' @param y alias for \code{limit} for compatibility with \code{plot}.
#' @param limit maximum number of variables plotted.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param ylim the y limits of the plot.
#' @param main an overall title for the plot.
#' @param sub a sub title for the plot.
#' @param asp the y/x aspect ratio.
#' @param cex_label_nodes the magnification to be used for
#'                        the node labels. 
#'                        If set to \code{0} (as default) 
#'                        node labels are not showed.
#' @param cex_label_edges the magnification 
#'                        for the edge labels. 
#'                        If set to \code{0} edge labels are not displayed.
#' @param cex_nodes the magnification  for 
#'                  the nodes of the tree.
#' @param cex_tree_y the magnification for the 
#'                   tree in the vertical direction.
#'                   Default is \code{0.9} to leave some space 
#'                   for the variable names. 
#' @param col 
#'        color mapping for stages, one of the following: 
#'        NULL (color will be assigned based on the current palette);
#'        a named (variables) list of named (stages)
#'        vectors of colors;
#'        the character \code{"stages"}, in which case the stage names 
#'        will be used as colors; 
#'        a function that takes
#'        as input a vector of stages and output the corresponding colors.
#'        Check the provided examples. 
#'        The function \code{make_stages_col} is used internally
#'        and \code{make_stages_col(x, NULL)} or \code{make_stages_col(x, "stages")} 
#'        can be used as a starting point for colors tweaking.
#' @param col_edges color for the edges. 
#' @param var_names logical, if variable names should be added to the plot,
#'                  otherwise variable names can be added manually using 
#'                  \code{\link{text.sevt}}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @param pch_nodes either an integer specifying a symbol or a single character 
#'                  to be used as the default in plotting nodes shapes see 
#'                  \code{\link{points}}.
#' @param lwd_nodes the line width for edges, a positive number, defaulting to 1.
#' @param lwd_edges the line width for nodes, a positive number, defaulting to 1.
#' @param ... additional graphical parameters to be passed to
#'         \code{points}, \code{lines}, \code{title},
#'         \code{text} and \code{plot.window}.
#' @export
#' @importFrom graphics lines plot.new plot.window title
#'
#' @examples
#'
#' data("PhDArticles")
#' mod <- stages_bj(full(PhDArticles, join_unobserved = TRUE))
#'
#' ### simple plotting
#' plot(mod)
#'
#' ### labels in nodes 
#' plot(mod, cex_label_nodes = 1, cex_nodes = 0)
#'
#' ### reduce nodes size
#' plot(mod, cex_nodes = 0.5)
#'
#' ### change line width and nodes style
#' plot(mod, lwd_edges = 3, pch_nodes = 5)
#'
#' ### changing palette
#' plot(mod, col = function(s) heat.colors(length(s)))
#' 
#' ### or changing global palette
#' palette(hcl.colors(10, "Harmonic"))
#' plot(mod)
#' palette("default") ##
#' 
#' ### forcing plotting of unobserved stages
#' plot(mod, ignore = NULL)
#' 
#' ### use function to specify colors 
#' plot(mod, col = function(stages){
#'     hcl.colors(n = length(stages))
#' })
#'
#' ### manually give stages colors
#' ### as an example we will assign colors only to the stages of two variables
#' ### Gender (one stage named "1") and Mentor (six stages)
#' col <- list(Gender = c("1" = "blue"), 
#'             Mentor = c("UNOBSERVED" = "grey",
#'                         "2" = "red", 
#'                         "3" = "purple",
#'                         "10" = "pink",
#'                         "18" = "green",
#'                         "22" = "brown"))
#' ### by setting ignore = NULL we will plot also the UNOBSERVED stage for Mentor
#' plot(mod, col = col, ignore = NULL)
plot.sevt <-
  function(x,
           y = 10,
           limit = y,
           xlim = c(0, 1),
           ylim = c(0, 1),
           main = NULL,
           sub = NULL,
           asp = 1,
           cex_label_nodes = 0,
           cex_label_edges = 1,
           cex_nodes = 2,
           cex_tree_y = 0.9,
           col = NULL,
           col_edges = "black",
           var_names = TRUE,
           ignore = x$name_unobserved,
           pch_nodes = 16,
           lwd_nodes = 1,
           lwd_edges = 1,
           ...) {
    check_sevt(x)
    plot.new()
    d <- min(length(x$tree), limit) ## avoid too much plotting
    nms <- names(x$tree) ## name of variable
    if (is.null(x$stages[[nms[1]]])){ ## add stage name also to root
      x$stages[[nms[1]]] <- c(NA)
    }
    col <- make_stages_col(x, col, ignore,limit =  d) 
    if (is.null(col_edges)){
      col_edges <- "black"
    }
    M <- prod(sapply(x$tree[1:d], length))
    cex_nodes <- rep(cex_nodes, d)[1:d]
    cex_label_nodes <- rep(cex_label_nodes, d)[1:d]
    plot.window(
      xlim = xlim,
      ylim = ylim,
      asp = asp,
      ...
    )
    title(main = main, sub = sub, ...)
    n <- x$tree
    p <- length(x$tree)
    Ls <- rep(0, d)
    Ls[d] <- cex_tree_y*(ylim[2] - ylim[1])
    ns <- M
    As <- rep(0, d)
    nv <- length(x$tree[[1]])
    if (d >= 2) {
      for (i in d:2) {
        nv <- length(x$tree[[i]])
        ns <- ns / nv
        As[i] <- Ls[i] / (ns + (ns - 1) / (nv - 1))
        Ls[i - 1] <- Ls[i] - As[i]
      }
      nv <- length(x$tree[[i - 1]])
      ns <- ns / nv
      As[i - 1] <- Ls[i - 1] / (ns + (ns - 1) / (nv - 1))
    }
    s1 <- ifelse(is.null(x$stages[[nms[1]]]), "1",
                 x$stages[[nms[1]]][1]
    )
    node(
      c(xlim[1], mean(ylim)),
      label = s1,
      cex_label = cex_label_nodes[1],
      cex_node = cex_nodes[1],
      col = col[[nms[1]]][s1],
      pch = pch_nodes,
      lwd = lwd_nodes,
      ...
    ) # plot first node
    xx <- xlim[1]
    y <- yy <- mean(ylim)
    ns <- 1
    step <- (xlim[2] - xlim[1]) / d
    for (k in 1:d) {
      # plot nodes for every strata
      v <- x$tree[[k]]
      yyy <- yy
      yy <- c()
      lj <- 0
      xx <- step * k # increase x position
      nv <- length(v)
      for (i in 1:ns) {
        # for every old node
        y <-
          yyy[i] + As[k] * seq(
            from = -0.5,
            to = 0.5,
            length.out = nv
          )
        # compute new y positions
        yy <- c(yy, y)
          for (j in 1:nv) {
            # plot nodes
            lj <- lj + 1
            if (k < d) {
              if (!(x$stages[[nms[k + 1]]][lj] %in% ignore)){
                node(
                  c(xx, y[j]),
                  label = x$stages[[nms[k + 1]]][lj],
                  cex_label = cex_label_nodes[k + 1],
                  col = col[[nms[k + 1]]][x$stages[[nms[k + 1]]][lj]],
                  cex_node = cex_nodes[k + 1],
                  pch = pch_nodes,
                  lwd = lwd_nodes,
                  ...
                )
              }
              if (!(x$stages[[nms[k]]][i] %in% ignore)){
                edge(c(
                  xx - step,
                  yyy[i]
                ), c(xx, y[j]),
                v[j],
                col = col_edges,
                cex_label = cex_label_edges, 
                lwd = lwd_edges,
                ...) # plot edge with previous node
              }
            }else{
              if (!(x$stages[[nms[k]]][i] %in% ignore)){
                edge(c(
                  xx - step,
                  yyy[i]
                ), c(xx, y[j]),
                v[j],
                col = col_edges,
                cex_label = cex_label_edges, 
                lwd = lwd_edges,
                ...
                ) # plot edge with previous nodes
              }
            }
          }
      }
      ns <- ns * nv
    }
    if (var_names){
      text.sevt(x, limit = limit, xlim = xlim, ylim = ylim, adj = 0)
    }
  }

#' Plot a node
#'
#' @param x the center
#' @param label the label
#' @param col color
#' @param cex_label cex parameter to be passed to text
#' @param cex_node cex parameter for nodes
#' @param ... additional parameters passed to \code{par()}
#' @importFrom graphics text lines points
#' @keywords internal
node <- function(x,
                 label = "",
                 col = "black",
                 cex_label = 1,
                 cex_node = 1,
                 ...) {
  points(x[1], x[2], col = col, cex = cex_node, ...)
  if (cex_label > 0) {
    text(
      x = x[1],
      y = x[2],
      labels = label,
      col = col,
      cex = cex_label,
      ...
    )
  }
}

#' @rdname plot.sevt
#' @export
make_stages_col <- function(x, col = NULL, 
                            ignore = x$name_unobserved, 
                            limit = NULL){
  d <- min(length(x$tree), limit)
  nms <- names(x$tree)
  if (is.null(col)) {
    col <- lapply(x$stages[nms[1:d]], function(stages) {
      if (is.null(stages)) {
        return(list("1" = "black"))
      }
      stages <- unique(stages)
      stages <- stages[!(stages %in% ignore)]
      vc <- seq_along(stages)
      names(vc) <- stages
      return(vc)
    })
  } else if (is.function(col)) {
    col <- lapply(x$stages[nms[1:d]], function(stages) {
      if (is.null(stages)) {
        ## this should be checked 
        return(list("1" = "black"))
      }
      stages <- unique(stages)
      stages <- stages[!(stages %in% ignore)]
      cs <- col(unique(stages))
      if (is.null(names(cs))){
        names(cs) <- unique(stages)[seq_along(cs)]
      }
      return(cs)
    })
  } else if (length(col) == 1 && col == "stages") {
    if (col == "stages") {
      col <- lapply(x$stages[nms[1:d]], function(stages) {
        if (is.null(stages)) {
          return(list("1" = 1))
        }
        stages <- unique(stages)
        stages <- stages[!(stages %in% ignore)]
        names(stages) <- stages
        return(stages)
      })
    }
  }
  return(col)
}

#' Plot an edge
#'
#' @param from From
#' @param to To
#' @param label the label
#' @param col color
#' @param cex_label numerical
#' @param ... additional parameters passed to \code{par()}
#' @importFrom graphics text lines
#' @keywords internal
edge <-
  function(from,
           to,
           label = "",
           col = "black",
           cex_label = 1,
           ...) {
    lines(c(from[1], to[1]), c(from[2], to[2]), col = col, ...)
    a <-
      180 * atan2((to[2] - from[2]), (to[1] - from[1])) / pi ## compute the angle of the line
    if (cex_label > 0) {
      ## put the label rotated of the proper angle
      text(
        x = (from[1] + to[1]) / 2,
        y = (from[2] + to[2]) / 2,
        labels = label,
        srt = a,
        col = col,
        cex = cex_label,
        ...
      )
    }
  }


#' Add text to a staged event tree plot
#'
#' @param x An object of class \code{sevt}.
#' @param y the position of the labels.
#' @param limit maximum number of variables plotted.
#' @param xlim graphical parameter.
#' @param ylim graphical parameter.
#' @param ... additional parameters passed to \code{\link{text}}.
#' @importFrom graphics text
#' @export
text.sevt <-
  function(x,
           y = ylim[1],
           limit = 10,
           xlim = c(0, 1),
           ylim = c(0, 1),
           ...) {
    check_sevt(x)
    d <- min(length(x$tree), limit) ## avoid too much plotting
    step <- (xlim[2] - xlim[1]) / d
    yy <- y
    var <- names(x$tree)
    text(x = seq(from = xlim[1], to = xlim[2], length.out = d + 1)[1:d], y = y, 
         labels = var[1:d], ...)
  }


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
  stg <- stages(height, var)
  stg <- stg[!(stg %in% ignore)]
  ustg <- unique(stg) 
  if (is.null(col)) {
      if (is.null(stg)) {
        col = list("1" = "black")
      }else{
        col <- seq_along(ustg)
        names(col) <- ustg
      }
  } else if (is.function(col)) {
      if (is.null(stg)) {
        col <- list("1" = 1)
      }else{
        col <- col(ustg)
        if (is.null(names(col))){
          names(col) <- ustg
        }
      }
  } else if (length(col) == 1 && col == "stages") {
    if (col == "stages") {
        if (is.null(stg)) {
          col <- list("1" = 1)
        }else{
          col <- ustg
          names(col) <- ustg
        }
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


#' igraph's plotting for CEG 
#' 
#' @param x an object of class \code{\link{ceg}}. 
#' @param col colors specification see \code{\link{plot.sevt}}.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @param layout an igraph layout.
#' @param endnode if the end node should be plotted.
#' @param ... additional arguments passed to \code{plot.igraph}.
#' @details This function is a simple wrapper around 
#'  \pkg{igraph}'s \code{plot.igraph}.
#'  The ceg object is converted to an igraph object 
#'  by firstly obtaining the adjacency matrix representation
#'  with \code{\link{ceg2adjmat}}. 
#'  If not specified, the default \code{layout} used is 
#'  a rotated \code{layout.reingold.tilford}.
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
                     endnode = TRUE,
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
  if (endnode) ucol <- c(ucol,1)
  g <- graph_from_ceg(x, ignore = ignore, endnode = endnode)
  igraph::V(g)$color <- ucol
  if (is.null(layout)){
    layout = igraph::layout.sugiyama(g)$layout
    layout = layout[,2:1]
    layout[,1] <- -layout[,1]
  }
  igraph::plot.igraph(g, layout = layout, 
                      palette = palette(), ...)
}