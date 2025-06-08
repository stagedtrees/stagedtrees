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
#'        the character \code{"classic"} for the classical coloring, where
#'        singleton stages are not colored.
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
#' plot(mod, col = function(stages) {
#'   hcl.colors(n = length(stages))
#' })
#'
#' ### manually give stages colors
#' ### as an example we will assign colors only to the stages of two variables
#' ### Gender (one stage named "1") and Mentor (six stages)
#' col <- list(
#'   Gender = c("1" = "blue"),
#'   Mentor = c(
#'     "UNOBSERVED" = "grey",
#'     "2" = "red",
#'     "3" = "purple",
#'     "10" = "pink",
#'     "18" = "green",
#'     "22" = "brown"
#'   )
#' )
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
    if (is.null(x$stages[[nms[1]]])) { ## add stage name also to root
      x$stages[[nms[1]]] <- c(NA)
    }
    col <- make_stages_col(x, col, ignore, limit = d)
    if (is.null(col_edges)) {
      col_edges <- "black"
    }
    M <- prod(vapply(x$tree[1:d], length, FUN.VALUE = 1))
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
    Ls[d] <- cex_tree_y * (ylim[2] - ylim[1])
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
            if (!(x$stages[[nms[k + 1]]][lj] %in% ignore)) {
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
            if (!(x$stages[[nms[k]]][i] %in% ignore)) {
              edge(
                c(
                  xx - step,
                  yyy[i]
                ), c(xx, y[j]),
                v[j],
                col = col_edges,
                cex_label = cex_label_edges,
                lwd = lwd_edges,
                ...
              ) # plot edge with previous node
            }
          } else {
            if (!(x$stages[[nms[k]]][i] %in% ignore)) {
              edge(
                c(
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
    if (var_names) {
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

#' @rdname plot.sevt
#' @export
make_stages_col <- function(x, col = NULL,
                            ignore = x$name_unobserved,
                            limit = NULL) {
  check_sevt(x)
  d <- min(length(x$tree), limit)
  nms <- names(x$tree)
  if (is.null(col)) {
    col <- sapply(nms[1:d], function(vv) {
      stages <- x$stages[[vv]]
      if (is.null(stages)) {
        return(NULL)
      }
      stages <- unique(stages)
      stages <- stages[!(stages %in% ignore)]
      vc <- seq_along(stages)
      names(vc) <- stages
      return(vc)
    }, simplify = FALSE)
  } else if (is.function(col)) {
    col <- sapply(nms[1:d], function(vv) {
      stages <- x$stages[[vv]]
      if (!is.null(stages)) {
        stages <- unique(stages)
        stages <- stages[!(stages %in% ignore)]
      }
      if (length(formals(col)) == 0){
        cs <- col()
      }
      if (length(formals(col)) == 1){
        cs <- col(stages)
      }
      if (length(formals(col)) > 1){
        cs <- col(stages, vv)
      }
      if (is.null(names(cs))) {
        names(cs) <- stages[seq_along(cs)]
      }
      return(cs)
    }, simplify = FALSE)
  } else if (length(col) == 1) {
    if (col == "stages") {
      col <- sapply(nms[1:d], function(vv) {
        stages <- x$stages[[vv]]
        if (is.null(stages)) {
          return(c("1" = 1))
        }
        stages <- unique(stages)
        stages <- stages[!(stages %in% ignore)]
        names(stages) <- stages
        return(stages)
      }, simplify = FALSE)
    } else if (startsWith(col, "classic")){
      isclassic <- col == "classic"
      col <- sapply(nms[1:d], function(vv) {
        stages <- x$stages[[vv]]
        singles <- names(which(table(stages) == 1))
        stages <- unique(stages)
        stages <- stages[!(stages %in% c(ignore, singles))]
        if (length(stages) > 0){
          vc <- seq_along(stages)
          names(vc) <- stages
        } else {
          vc <- NULL
        }
        return(vc)
      }, simplify = FALSE)
      if (isclassic){
        tot <- seq_along(unlist(col))
        for (i in seq_along(col)){
          kci <- length(col[[i]])
          if (kci > 0){
            col[[i]][] <- tot[(1:kci)]
            tot <- tot[-(1:kci)]
          }
        }
      }
    }
  } else {
    if (is.list(col) && !is.null(names(col))) {
      col <- sapply(nms[1:d], function(nm) {
        col[[nm]]
      }, simplify = FALSE)
    } else {
      cli::cli_abort(c(
        "{.arg col} must be one of: {.val NULL}, {.val stages},
        {.val classic},
        a function or a named list.",
        "x" = "You've supplied {.type {col}}.",
        "i" = "Check sevt plotting documentation
               {.fun stagedtrees::plot.sevt}."
      ))
    }
  }
  return(col)
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
    text(
      x = seq(from = xlim[1], to = xlim[2], length.out = d + 1)[1:d], y = y,
      labels = var[1:d], ...
    )
  }
