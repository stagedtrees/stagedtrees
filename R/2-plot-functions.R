#' plot a stratified event tree
#'
#' @param x event tree object
#' @param rmax graphical parameter
#' @param rmin graphical parameter
#' @param step graphical parameter
#' @param limit maximum number of plotted levels
#' @param ... additional graphical parameters
#' @export
#' @importFrom graphics lines plot.new plot.window
plot.strt_ev_tree <- function(x,
                              rmax = 1,
                              rmin = 0.1,
                              step = 2,
                              limit = 10,
                              ...) {
  plot.new()
  d <- min(length(x$tree), limit) ##avoid too much plotting
  plot.window(
    xlim = c(0, step * d),
    ylim = c(-step * d / 2 - 0.5, step * d / 2 + 0.5),
    asp = 1,
    ...
  )
  nms <- names(x$tree)
  node(c(0, 0), rmax, label = nms[1]) #plot first node
  ns <- 1
  xx <- 0
  y <- 0
  yy <- 0
  coef <- 1
  for (k in 1:length(x$tree)) {
    #plot nodes for every strata
    v <- x$tree[[k]]
    yyy <- yy
    yy <- c()
    xx <- step * k #increase x position
    nv <- length(v)
    for (i in 1:ns) {
      #for every old node
      y <-
        yyy[i] + (step / 2) * coef * d * seq(from = -1,
                                             to = 1,
                                             length.out = nv) / (ns * nv) #compute new y positions
      yy <- c(yy, y)
      for (j in 1:nv) {
        #plot nodes
        node(c(xx, y[j]), coef * rmax / sqrt(nv * ns), label = nms[k + 1])
        edge(c(xx - step, yyy[i]), c(xx, y[j]), v[j]) #plot edge with previous nodes
      }
    }
    ns <- ns * nv
    if (nv %% 2 == 1) {
      ## this is to avoid overlapping,
      coef <- coef * 0.5   ## not sure why it works like this but ok
    } else {
      coef <- 1 * coef
    }
  }
}


#' plot a staged event tree
#'
#' @param x staged event tree object
#' @param limit maximum number of variables plotted
#' @param xlim graphical parameter
#' @param ylim graphical parameter
#' @param asp graphical parameter
#' @param cex.label.nodes graphical parameter
#' @param cex.label.edges graphical parameter
#' @param cex.nodes graphical parameter
#' @param col color mapping for the stages, a named list with
#'        names equal to the variables names in the model and
#'        vectors named with stages names as components; otherwise
#'        if \code{col == "stages"} the stage names will be used as
#'        colors; otherwise if \code{col} is a function it will take
#'        as input a vector of stages and output the corresponding colors.
#' @param ... additional graphical parameters to be passed to
#'         \code{points}, \code{lines}, \code{title},
#'         \code{text} and \code{plot.window}.
#' @export
#' @importFrom graphics lines plot.new plot.window title
plot.sevt <-
  function(x,
           limit = 10,
           xlim = c(0, 1),
           ylim = c(0, 1),
           asp = 1,
           cex.label.nodes = 1,
           cex.label.edges = 1,
           cex.nodes = 2,
           col = NULL,
           ...) {
    plot.new()
    d <- min(length(x$tree), limit) ##avoid too much plotting
    nms <- names(x$tree) ##name of variable
    if (is.null(col)) {
      col <- lapply(x$stages[nms[1:d]], function(stages) {
        if (is.null(stages)){
          return(list("1" = "black"))
        }
        stages <- unique(stages)
        vc <- 1:length(stages)
        names(vc) <- stages
        return(vc)
      })
    } else if (is(col, "function")) {
      col <- lapply(x$stages[nms[1:d]], function(stages) {
        if (is.null(stages)){
          return(list("1" = "black"))
        }
        stages <- col(unique(stages))
        names(stages) <- stages
        return(stages)
      })
    } else if (col == "stages") {
      col <- lapply(x$stages[nms[1:d]], function(stages) {
        if (is.null(stages)){
          return(list("1" = 1))
        }
        stages <- unique(stages)
        names(stages) <- stages
        return(stages)
      })
    }
    M <- prod(sapply(x$tree[1:d], length))
    cex.nodes <- rep(cex.nodes, d)[1:d]
    cex.label.nodes = rep(cex.label.nodes, d)[1:d]
    plot.window(xlim = xlim,
                ylim = ylim,
                asp = asp,
                ...)
    title(...)
    n <- x$tree
    p <- length(x$tree)
    Ls <- rep(0, d)
    Ls[d] <- ylim[2] - ylim[1]
    ns <- M
    As <- rep(0, d)
    nv <- length(x$tree[[1]])
    if (d >= 2) {
      for (i in d:2) {
        nv <- length(x$tree[[i]])
        ns <- ns / nv
        As[i] <- Ls[i] / (ns  + (ns - 1) / (nv - 1))
        Ls[i - 1] <- Ls[i] - As[i]
      }
      nv <- length(x$tree[[i - 1]])
      ns <- ns / nv
      As[i - 1] <- Ls[i - 1] / (ns  + (ns - 1) / (nv - 1))
    }
    s1 <- ifelse(is.null(x$stages[[ nms[1] ]]), "1", 
                 x$stages[[ nms[1] ]][1] )
    node(
      c(xlim[1], mean(ylim)),
      label = s1,
      cex.label = cex.label.nodes[1],
      cex.node = cex.nodes[1],
      col = col[[ nms[1] ]][ s1 ], 
      ...
    ) #plot first node
    xx <- xlim[1]
    y <- yy <- mean(ylim)
    ns <- 1
    step <- (xlim[2] - xlim[1]) / d
    for (k in 1:d) {
      #plot nodes for every strata
      v <- x$tree[[k]]
      yyy <- yy
      yy <- c()
      lj <- 0
      xx <- step * k #increase x position
      nv <- length(v)
      for (i in 1:ns) {
        #for every old node
        y <-
          yyy[i]  + As[k] * seq(from = -0.5 ,
                                to = 0.5,
                                length.out = nv)
        #compute new y positions
        yy <- c(yy, y)
        for (j in 1:nv) {
          #plot nodes
          lj <- lj + 1
          if (k < d) {
            node(
              c(xx, y[j]),
              label = x$stages[[ nms[k + 1] ]][lj],
              #label = nms[k+1],
              cex.label = cex.label.nodes[k + 1],
              col = col[[ nms[k + 1 ] ]][x$stages[[ nms[k + 1] ]][lj]],
              cex.node = cex.nodes[k + 1],
              ...
            )
          }
          edge(c(xx - step,
                 yyy[i]), c(xx, y[j]),
               v[j], cex.label = cex.label.edges, ...) #plot edge with previous nodes
        }
      }
      ns <- ns * nv
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
edge <-
  function(from,
           to,
           label = "" ,
           col = "black",
           cex.label = 1,
           ...) {
    lines(c(from[1], to[1]), c(from[2], to[2]), col = col, ...)
    a <-
      180 * atan2((to[2] - from[2]), (to[1] - from[1])) / pi   ## compute the angle of the line
    if (cex.label > 0) {
      ## put the label rotated of the proper angle
      text(
        x = (from[1] + to[1]) / 2 ,
        y = (from[2] + to[2]) / 2 ,
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
#' @param x a staged event tree
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
    d <- min(length(x$tree), limit) ##avoid too much plotting
    step <- (xlim[2] - xlim[1]) / d
    xx <- xlim[1]
    yy <- y
    var <- names(x$tree)
    for (i in 1:d) {
      text(x = xx,
           y = yy,
           labels = var[i],
           ...)
      xx <- xx + step
    }
  }
