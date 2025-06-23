#' Export the staged tree or CEG graph to tikz
#'
#' Generate tikz code to draw the staged tree or CEG graph.
#' @param x an object of class \code{\link{sevt}} or \code{\link{ceg}}.
#' @param layout the layout of the graph, given as matrix with two columns
#'               and as many rows as nodes in the staged tree.
#'              By default, a modified sugiyama layout is used.
#'              The layout matrix can be obtained with \pkg{igraph}
#'              layout functions.
#' @param file A connection or a character string naming the file to print to.
#'             Passed to \code{\link{cat}}.
#' @param col color specifications for the stages of the staged even tree.
#'            Same as \code{\link{plot.sevt}} and \code{\link{make_stages_col}}.
#' @param ignore vector of stages which will be ignored and not plotted,
#' by default the name of the unobserved stages stored in \code{x$name_unobserved}.
#' @param node_label a function that produces nodes labels.
#' @param edge_label a function that produces edge labels.
#' @param edge_label_options a function that produces edge label options.
#' @param scale for the tikzfigure.
#' @param normalize_layout logical, if \code{TRUE} layout positions are normalized,
#' see also \code{xlim}.
#' @param xlim \code{NULL} or a two dimensional vector, if not \code{NULL}
#'        layout positions in the x-axis are scaled to the \code{xlim} interval.
#' @param ylim same as \code{xlim} for the y-axis.
#' @param node_shape the shape to be used for nodes.
#' @param node_inner_sep the \code{inner sep} parameter.
#' @param node_minimum_size the \code{minimum size} parameter for the nodes.
#' @param node_draw_color the color for line drawing the nodes.
#' @param node_thickness the thickness of the lines.
#' @param node_text_color the color for label in nodes.
#' @details This function can be used to create a working
#'          tikz code that compile to a graph similar to the
#'          one obtained by \code{plot.sevt(x, ...)} or
#'          \code{plot.ceg(x, ...)}.
#' @references Code partially inspired by the code in
#'             _Exporting graphs to LaTeX, using igraph and TikZ_
#'             \url{http://igraph.wikidot.com/r-recipes#toc2}
#' @importFrom grDevices col2rgb
#' @export
write_tikz <- function(x, layout = NULL, file = "",
                       col = NULL, ignore = x$name_unobserved,
                       node_label = function(node) {
                         ifelse(is.na(node$stage), "", node$stage)
                       },
                       edge_label = function(edge) {
                         ifelse(is.na(edge$label), "", edge$label)
                       },
                       edge_label_options = function(edge) {
                         return("sloped")
                       },
                       scale = 10,
                       normalize_layout = TRUE,
                       xlim = c(0,1),
                       ylim = c(0,1),
                       node_shape = "circle",
                       node_inner_sep = "1mm",
                       node_minimum_size = "0.3cm",
                       node_draw_color = "black",
                       node_thickness = "very thick",
                       node_text_color = "black") {
  UseMethod("write_tikz", x)
}

#' @rdname write_tikz
#' @export
write_tikz.sevt <- function(x, layout = NULL, file = "",
                            col = NULL, ignore = x$name_unobserved,
                            node_label = function(node) {
                              ifelse(is.na(node$stage), "", node$stage)
                            },
                            edge_label = function(edge) {
                              ifelse(is.na(edge$label), "", edge$label)
                            },
                            edge_label_options = function(edge) {
                              return("sloped")
                            },
                            scale = 10,
                            normalize_layout = TRUE,
                            xlim = c(0,1),
                            ylim = c(0,1),
                            node_shape = "circle",
                            node_inner_sep = "1mm",
                            node_minimum_size = "0.3cm",
                            node_draw_color = "black",
                            node_thickness = "very thick",
                            node_text_color = "black") {
  edgs <- get_edges(x, ignore = ignore)
  verts <- get_vertices(x, ignore = ignore)

  col <- make_stages_col(x, col, ignore = ignore)

  if (is.null(layout)) {
    layout <- igraph::layout_with_sugiyama(as_igraph(x, ignore = ignore))$layout
    layout <- layout[, 2:1]
    layout[, 1] <- -layout[, 1]
  }

  if (is.function(layout)) {
    layout <- layout(as_igraph(x, ignore = ignore))
  }
  if (normalize_layout){
    if (is.null(xlim)) xlim <- c(0,1)
    if (is.null(ylim)) ylim <- c(0,1)
  }
  if (length(xlim) >= 2){
    layout[, 1] <- xlim[1] +
      (xlim[2] - xlim[1]) * (layout[, 1] - min(layout[, 1])) /
      (max(layout[, 1]) - min(layout[, 1]))
  }
  if (length(ylim) >= 2) {
    layout[, 2] <- ylim[1] +
      (ylim[2] - ylim[1]) * (layout[, 2] - min(layout[, 2])) /
      (max(layout[, 2]) - min(layout[, 2]))
  }

  cat2 <- function(x) cat(x, file = file, append = TRUE)

  cat(paste0("\\begin{tikzpicture}[auto, scale=", scale, ",\n"), file = file)

  nodestyle <- "\t%s/.style={%s,inner sep=%s,minimum size=%s,draw,%s,%s,fill=%s,text=%s},\n"
  v1 <- names(x$tree)[1]
  s11 <- stages(x)[[v1]]
  c1 <- col2rgb(ifelse(is.null(col[[v1]][s11]), "white", col[[v1]][s11]))
  c1 <- sprintf("{rgb,255:red,%s; green,%s; blue,%s}", c1[1], c1[2], c1[3])
  cat2(sprintf(
    nodestyle,
    paste(verts[1, "var"], verts[1, "stage"], sep = "_"),
    node_shape,
    node_inner_sep,
    node_minimum_size,
    node_draw_color,
    node_thickness,
    c1,
    node_text_color
  ))
  for (v in names(x$tree[-1])) {
    for (s in unique(x$stages[[v]])) {
      c1 <- col2rgb(ifelse(is.null(col[[v]][s]), "white", col[[v]][s]))
      c1 <- sprintf("{rgb,255:red,%s; green,%s; blue,%s}", c1[1], c1[2], c1[3])
      cat2(sprintf(
        nodestyle, paste(v, s, sep = "_"),
        node_shape,
        node_inner_sep,
        node_minimum_size,
        node_draw_color,
        node_thickness,
        c1,
        node_text_color
      ))
    }
  }
  cat2("\t leaf/.style={circle,inner sep=1mm,minimum size=0.2cm,draw,very thick,black,fill=gray,text=black}")
  cat2("]\n")
  cat2("\n")


  for (i in 1:nrow(verts)) {
    vert <- .fix_n(verts[i, "name"])
    var <- verts[i, "var"]
    stage <- verts[i, "stage"]
    label <- node_label(verts[i, , drop = FALSE])

    if (is.na(var)) {
      ## drawing leaves
      cat2(sprintf(
        "\t\\node [leaf] (%s) at (%f, %f)\t{%s};\n",
        # var,
        vert, layout[i, 1], layout[i, 2], .escape(label)
      ))
    } else {
      ## drawing vertices
      cat2(sprintf(
        "\t\\node [%s_%s] (%s) at (%f, %f)\t{%s};\n",
        var, stage,
        vert, layout[i, 1], layout[i, 2], .escape(label)
      ))
    }
  }
  cat2("\n")
  for (i in 1:nrow(edgs)) {
    from <- .fix_n(edgs[i, "from"])
    to <- .fix_n(edgs[i, "to"])
    label <- edge_label(edgs[i, , drop = FALSE])
    opt <- paste(edge_label_options(edgs[i, , drop = FALSE]),
      collapse = ","
    )
    cat2(sprintf(
      "\t\\draw[->] (%s) -- node [%s]{%s} (%s);\n",
      from, opt, .escape(label), to
    ))
  }

  cat2("\\end{tikzpicture}\n")
}

.fix_n <- function(xx){
  gsub("(\\.|\\_)+", "-",  make.names(xx))
}

.escape <- function(xx){
  gsub("(\\$|\\_)", "\\\\\\1", xx)
}

