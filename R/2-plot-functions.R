#' Plot method for staged event trees
#'
#' \code{plot.sevt} is the plot method for staged event tree
#' objects. It allows easy plotting of staged event tree with some
#' options, mainly different ways to specify colors for the stages (see
#' examples).
#' @param x an object of class \code{sevt}.
#' @param y alias for \code{limit} for compatibility with \code{plot}.
#' @param limit maximum number of variables plotted.
#' @param xlim the x limits (x1, x2) of the plot.
#' @param ylim the y limits of the plot.
#' @param main an overall title for the plot.
#' @param sub a sub title for the plot.
#' @param asp the y/x aspect ratio.
#' @param cex.label.nodes The magnification to be used for
#'                        the node labels. 
#'                        If set to \code{0} node labels are not showed.
#' @param cex.label.edges The magnification to be used 
#'                        for the edge labels. 
#'                        If set to \code{0} edge labels are not showed.
#' @param cex.nodes The magnification to be used for 
#'                  for the nodes of the tree.
#' @param cex.tree.y The magnification to be used for the 
#'                   tree in the vertical direction.
#'                   Default is \code{0.9} to leave some space 
#'                   for the variable names. 
#' @param col color mapping for the stages, a named list with
#'        names equal to the variables names in the model and
#'        vectors named with stages names as components; otherwise
#'        if \code{col == "stages"} the stage names will be used as
#'        colors; otherwise if \code{col} is a function it will take
#'        as input a vector of stages and output the corresponding colors.
#' @param col.edges color for the edges. 
#' @param var.names logical, if variable names should be added to the plot,
#'                  otherwise variable names can be added manually using 
#'                  \code{\link{text.sevt}}.
#' @param ignore array of stages name that should not be plotted.
#' @param ... additional graphical parameters to be passed to
#'         \code{points}, \code{lines}, \code{title},
#'         \code{text} and \code{plot.window}.
#' @export
#' @importFrom graphics lines plot.new plot.window title
#'
#' @examples
#'
#' data("PhDArticles")
#' mod <- stages_bj(full(PhDArticles, join.unobserved = TRUE))
#'
#' ### simple plotting
#' plot(mod)
#'
#' ### removing lables from nodes and edges and fill nodes
#' plot(mod, cex.label.nodes = 0, cex.label.edges = 0, pch = 16)
#'
#' ### reduce nodes size
#' plot(mod, cex.nodes = 1)
#'
#' ### change line width and nodes style
#' plot(mod, lwd = 3, pch = 5)
#'
#' ### changing palette
#' plot(mod, col = function(s) heat.colors(length(s)))
#' 
#' ### or changing global palette
#' palette(hcl.colors(10, "Harmonic"))
#' plot(mod)
#' 
#' ### forcing plotting of unobserved stages
#' plot(mod, ignore = NULL)
#'
#' ### manually give stages colors
#' simple <- stages_hclust(full(PhDArticles, lambda = 1), k = 2)
#' #### simple has 2 stages per variable "1" and "2"
#' col <- lapply(simple$stages, function(s) {
#'   c("1" = "purple", "2" = "cyan")
#' })
#' plot(simple, col = col)
plot.sevt <-
  function(x,
           y = 10,
           limit = y,
           xlim = c(0, 1),
           ylim = c(0, 1),
           main = NULL,
           sub = NULL,
           asp = 1,
           cex.label.nodes = 1,
           cex.label.edges = 1,
           cex.nodes = 2,
           cex.tree.y = 0.9,
           col = NULL,
           col.edges = "black",
           var.names = TRUE,
           ignore = x$name.unobserved,
           ...) {
    plot.new()
    d <- min(length(x$tree), limit) ## avoid too much plotting
    nms <- names(x$tree) ## name of variable
    if (is.null(x$stages[[nms[1]]])){ ## add stage name also to root
      x$stages[[nms[1]]] <- c("1")
    }
    if (is.null(col)) {
      col <- lapply(x$stages[nms[1:d]], function(stages) {
        if (is.null(stages)) {
          return(list("1" = "black"))
        }
        stages <- unique(stages)
        vc <- seq_along(stages)
        names(vc) <- stages
        return(vc)
      })
    } else if (is(col, "function")) {
      col <- lapply(x$stages[nms[1:d]], function(stages) {
        if (is.null(stages)) {
          return(list("1" = "black"))
        }
        cs <- col(unique(stages))
        names(cs) <- unique(stages)
        return(cs)
      })
    } else if (length(col) == 1 && col == "stages") {
      if (col == "stages") {
        col <- lapply(x$stages[nms[1:d]], function(stages) {
          if (is.null(stages)) {
            return(list("1" = 1))
          }
          stages <- unique(stages)
          names(stages) <- stages
          return(stages)
        })
      }
    }
    if (is.null(col.edges)){
      col.edges <- "black"
    }
    M <- prod(sapply(x$tree[1:d], length))
    cex.nodes <- rep(cex.nodes, d)[1:d]
    cex.label.nodes <- rep(cex.label.nodes, d)[1:d]
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
    Ls[d] <- cex.tree.y*(ylim[2] - ylim[1])
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
      cex.label = cex.label.nodes[1],
      cex.node = cex.nodes[1],
      col = col[[nms[1]]][s1],
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
                  cex.label = cex.label.nodes[k + 1],
                  col = col[[nms[k + 1]]][x$stages[[nms[k + 1]]][lj]],
                  cex.node = cex.nodes[k + 1],
                  ... 
                )
                edge(c(
                  xx - step,
                  yyy[i]
                ), c(xx, y[j]),
                v[j],
                col = col.edges,
                cex.label = cex.label.edges, ...
                ) # plot edge with previous nodes
              }
            }else{
              if (!(x$stages[[nms[k]]][i] %in% ignore)){
                edge(c(
                  xx - step,
                  yyy[i]
                ), c(xx, y[j]),
                v[j],
                col = col.edges,
                cex.label = cex.label.edges, ...
                ) # plot edge with previous nodes
              }
            }
          }
      }
      ns <- ns * nv
    }
    if (var.names){
      text.sevt(x, limit = limit, xlim = xlim, ylim = ylim)
    }
  }

#' Plot a node
#'
#' @param x the center
#' @param label the label
#' @param col color
#' @param cex.label cex parameter to be passed to text
#' @param cex.node cex parameter for nodes
#' @param ... additional parameters passed to \code{par()}
#' @importFrom graphics text lines points
#' @keywords internal
node <- function(x,
                 label = "",
                 col = "black",
                 cex.label = 1,
                 cex.node = 1,
                 ...) {
  points(x[1], x[2], col = col, cex = cex.node, ...)
  if (cex.label > 0) {
    text(
      x = x[1],
      y = x[2],
      labels = label,
      col = col,
      cex = cex.label,
      ...
    )
  }
}


#' Plot an edge
#'
#' @param from From
#' @param to To
#' @param label the label
#' @param col color
#' @param cex.label numerical
#' @param ... additional parameters passed to \code{par()}
#' @importFrom graphics text lines
#' @keywords internal
edge <-
  function(from,
           to,
           label = "",
           col = "black",
           cex.label = 1,
           ...) {
    lines(c(from[1], to[1]), c(from[2], to[2]), col = col, ...)
    a <-
      180 * atan2((to[2] - from[2]), (to[1] - from[1])) / pi ## compute the angle of the line
    if (cex.label > 0) {
      ## put the label rotated of the proper angle
      text(
        x = (from[1] + to[1]) / 2,
        y = (from[2] + to[2]) / 2,
        labels = label,
        srt = a,
        col = col,
        cex = cex.label,
        ...
      )
    }
  }


#' Add text to a staged even tree plot
#'
#' @param x staged event tree object
#' @param y the position of the labels
#' @param limit maximum number of variables plotted
#' @param xlim graphical parameter
#' @param ylim graphical parameter
#' @param ... additional parameters
#' @importFrom graphics text
#' @export
text.sevt <-
  function(x,
           y = ylim[1],
           limit = 10,
           xlim = c(0, 1),
           ylim = c(0, 1),
           ...) {
    d <- min(length(x$tree), limit) ## avoid too much plotting
    step <- (xlim[2] - xlim[1]) / d
    xx <- xlim[1]
    yy <- y
    var <- names(x$tree)
    for (i in 1:d) {
      text(
        x = xx,
        y = yy,
        labels = var[i],
        ...
      )
      xx <- xx + step
    }
  }


#' Barplot of stage probabilities
#' 
#' @param height An object of class \code{sevt}.
#' @param var name of a variable in \code{object}.
#' @param ignore array of stages name that should not be plotted.
#' @param beside a logical value. See \code{\link{barplot}}.
#' @param horiz a logical value. See \code{\link{barplot}}.
#' @param legend.text logical.
#' @param col color mapping for the stages, see \code{col}
#'        argument in \code{\link{plot.sevt}}.
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param ... additional arguments passed to \code{\link{barplot}}.
#' @export
#' @examples 
#' model <- stages_fbhc(full(PhDArticles, lambda = 1))
#' barplot(model, "Kids", beside = TRUE)
#' @importFrom graphics barplot
barplot.sevt <- function(height, var = variable.names(height)[1], 
                           ignore = height$name.unobserved,
                           beside = TRUE,
                           horiz = FALSE,
                           legend.text = FALSE, 
                           col = NULL, 
                           xlab = ifelse(horiz, "probability", NA),
                           ylab = ifelse(!horiz, "probability", NA),
                           ...){
  stopifnot(is_fitted_sevt(height))
  if (is.null(var)){
    
  }
  if (is.null(col)) {
    col <- lapply(height$stages[var], function(stages) {
      if (is.null(stages)) {
        return(list("1" = "black"))
      }
      stages <- unique(stages)
      vc <- seq_along(stages)
      names(vc) <- stages
      return(vc)
    })
  } else if (is(col, "function")) {
    col <- lapply(height$stages[var], function(stages) {
      if (is.null(stages)) {
        return(list("1" = "black"))
      }
      cs <- col(unique(stages))
      names(cs) <- unique(stages)
      return(cs)
    })
  } else if (length(col) == 1 && col == "stages") {
    if (col == "stages") {
      col <- lapply(height$stages[var], function(stages) {
        if (is.null(stages)) {
          return(list("1" = 1))
        }
        stages <- unique(stages)
        names(stages) <- stages
        return(stages)
      })
    }
  }
  tmp <- summary(height)[["stages.info"]]
  if (legend.text){
    legend.text = tmp[[var]]$stage
  }
  hei <- as.matrix(tmp[[var]][!(tmp[[var]][["stage"]] %in% ignore),
                              -(1:3)])
  hei[is.nan(hei)] <- 0
  
  barplot(hei, col = col[[var]], 
          legend.text = legend.text, beside = beside,
          xlab = xlab, ylab = ylab,
          horiz = horiz, ...)
}