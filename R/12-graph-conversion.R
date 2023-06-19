#' \pkg{igraph} conversion
#'
#' Obtain the graph representation of a staged tree or a CEG as
#' an object from the \pkg{igraph} package.
#' @name igraph-conversion
#' @param x an object of class \code{\link{sevt}} or \code{\link{ceg}}.
#' @param ignore vector of stages which will be ignored and excluded,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @details Functions to transalte the graph structure of a \code{\link{sevt}}
#' or \code{\link{ceg}} object to a graph object from the 
#' \pkg{igraph} package.
#' Additional functions that extract the edge lists 
#' and the vertices are available.
#' This can be useful, for example to plot the staged tree  with 
#' \pkg{igraph} or additional packages (see the examples).
#' @examples 
#' mod <-  stages_bhc(full(Titanic))
#' get_edges(mod)
#' get_vertices(mod)
#' \dontrun{
#' library(igraph)
#' library(ggraph)
#' ######## sevt example ########
#' ## convert to igraph object
#' g <- as_igraph(mod)
#' ## plot with igraph directly
#' plot(g, layout = layout_with_sugiyama)
#' ## plot with ggraph
#' ggraph(g, "sugiyama") + 
#' geom_edge_fan(aes( 
#'     label = label,
#'    label_pos = 0.5 + runif(length(label),-0.1,0.1)), 
#'    angle_calc = "along",show.legend = FALSE, check_overlap = FALSE,
#'    end_cap = circle(0.02, 'npc'),
#'    arrow = grid::arrow(angle = 25, 
#'                        length = unit(0.025, "npc"),
#'                        type = "closed")) +
#'  geom_node_point(aes(x = x, y = y, color = stage), size = 5, 
#'                  show.legend = FALSE) +
#'  ggforce::theme_no_axes() + coord_flip() + scale_y_reverse()
#' 
#' ######## ceg example ########
#' g.ceg <- as_igraph(ceg(mod))
#' ### igraph plotting functions can be used 
#' plot(g.ceg, layout = layout.sugiyama)
#' ### igraph object can be also plotted with ggplot2 and ggraph
#' ggraph(g.ceg, "sugiyama") + 
#'  geom_edge_fan(aes(label = label, 
#'                    color = label,
#'                    label_pos = 0.5 + runif(length(label),-0.1,0.1)), 
#'                angle_calc = "along",show.legend = FALSE,check_overlap = FALSE,
#'                end_cap = circle(0.02, 'npc'),
#'                arrow = grid::arrow(angle = 25, 
#'                                    length = unit(0.025, "npc"),
#'                                    type = "closed")) +
#'  geom_node_point(aes(x = x, y = y, color = stage), size = 3, show.legend = FALSE) +
#'  ggforce::theme_no_axes() + coord_flip() + scale_y_reverse()
#' }
NULL

#' @rdname igraph-conversion
#' @param ... additional parameters.
#' @return for \code{get_edges}: the edges list corresponding 
#'         to the graph associated to \code{x}.
#' @export
get_edges <- function(x, ignore = x$name_unobserved, ...){
  UseMethod("get_edges", x)
}

#' @rdname igraph-conversion
#' @export 
get_edges.sevt <- function(x, ignore = x$name_unobserved, ...){
  check_sevt(x)
  tree <- x$tree
  var <- sevt_varnames(x)
  if (is.null(x$stages[[var[1]]])){ ## add stage name also to root
    x$stages[[var[1]]] <- c(NA)
  }
  edges <- data.frame(from = NA, to = NA, label = NA, var = NA, stage = NA)
  now <- "root"
  for (i in 1:length(var)){
    v <- var[i]
    m <- length(x$tree[[v]])
    for (k in seq_along(now)){
      stg <- x$stages[[v]][k]
      from <- now[k]
      if (!(stg %in% ignore)){
        for (j in 1:m){
          if (i<length(var)){
            stg_to <- x$stages[[var[i+1]]][m*(k-1) + j]
            if (stg_to %in% ignore){
              next
            }
          }
          lbl <- x$tree[[v]][j]
          to <- paste(from, lbl, sep = "-")
          edges <- rbind(edges, c(from = from, to = to, label = lbl,
                                    var = v, stage = stg))
        }
      }
    }
    now <- apply(expand.grid(x$tree[i:1])[,i:1, drop = FALSE], 
                 MARGIN = 1, function(x){
      paste0("root-", paste0(x, collapse = "-"))
      })
  }
  return(edges[-1,])
}


#' @rdname igraph-conversion
#' @return for \code{get_vertices}: the vertices list corresponding 
#'         to the graph associated to \code{x}.
#' @export
get_vertices <- function(x, ignore = x$name_unobserved, ...){
  UseMethod("get_vertices", x)
}

#' @rdname igraph-conversion
#' @export
get_vertices.sevt <- function(x, ignore = x$name_unobserved, ...){
  tree <- x$tree
  var <- sevt_varnames(x)
  vert <- data.frame(name = "root", stage = NA, var = var[1])
  for (i in 2:length(var)){
    v <- var[i]
    now <- apply(expand.grid(x$tree[(i-1):1])[, (i-1):1, drop = FALSE], 
                 MARGIN = 1, function(x){
      paste0("root-", paste0(x, collapse = "-"))
    })
    ix <- !(x$stages[[v]] %in% ignore)
    if (any(ix)){
      vert <- rbind(vert, data.frame(name = now[ix], 
                                     stage = x$stages[[v]][ix], 
                                     var = v))
    }
  }
  if (any(ix)){
    now <- paste(c(vapply(now[ix], FUN = rep, 
                          FUN.VALUE = x$tree[[var[i]]],
                          length(x$tree[[var[i]]]))), 
                 x$tree[[var[i]]], sep = "-")
    vert <- rbind(vert, data.frame(name = now, 
                                   stage = NA,
                                   var = NA))
  }
  return(vert)
}

#' @rdname igraph-conversion
#' @export
get_edges.ceg <- function(x, ignore = x$name_unobserved, ...){
  tree <- x$tree
  var <- sevt_varnames(x)
  if (is.null(x$stages[[var[1]]])){ ## add stage name also to root
    x$stages[[var[1]]] <- c("1")
  }
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v){
    pos[[v]][x$stages[[v]] %in% ignore]
  })
  allignore <- c(unique(unlist(pos.ignore)))
  allpos <- c(unique(unlist(pos)), "END")
  wignore <- allpos %in% allignore
  k <- length(allpos)
  m <- 1
  edges <- data.frame(from = NA, to = NA, label = NA, stage = NA, var = NA)
  for (i in 2:length(var)){
    v <- var[i - 1]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      for (j in 1:m){
        p2 <- pos[[var[i]]][ix*m - m + j]
        if (!(p2 %in% allignore)){
          edges <- rbind(edges, c(from = p1, to = p2, 
                                  label = x$tree[[v]][j], 
                                  stage = x$stages[[v]][ix],
                                  var = v))
        }
      }
    }
  }
  v <- var[i]
  ## add endonode
  for (p1 in unique(pos[[v]])){
    if (!(p1 %in% allignore)){
      for (label in x$tree[[v]]){
        edges <- rbind(edges, c(from = p1, to = "END", label = label, 
                                stage = "END", var = v))
      }
    }
  }
  return(edges[-1,])
}

#' @rdname igraph-conversion
#' @export
get_vertices.ceg <- function(x, ignore = x$name_unobserved, ...){
  tree <- x$tree
  var <- sevt_varnames(x)
  if (is.null(x$stages[[var[1]]])){ ## add stage name also to root
    x$stages[[var[1]]] <- c("0")
  }
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v){
    pos[[v]][x$stages[[v]] %in% ignore]
  })
  allignore <- c(unique(unlist(pos.ignore)))
  allpos <- c(unique(unlist(pos)), "END")
  wignore <- allpos %in% allignore
  k <- length(allpos)
  m <- 1
  vert <- data.frame(name = NA, stage = NA, var = NA)
  for (i in 1:length(var)){
    v <- var[i]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      if (!(x$stages[[v]][ix]) %in% ignore){
        vert <- rbind(vert, c(name = p1, stage = x$stages[[v]][ix], var = v))
      }
    }
  }    
  vert <- rbind(vert, c(name = "END", stage = "END", var = NA))
  return(vert[-1,])
}

#' @rdname igraph-conversion
#' @return for \code{as.igraph}: a graph object from the 
#' \pkg{igraph} package.
#' @export
as_igraph <- function(x, ignore = x$name_unobserved, ...){
  UseMethod("as_igraph", x)
}

#' @rdname igraph-conversion
#' @export
as_igraph.sevt <- function(x, ignore = x$name_unobserved, ...){
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" is needed.",
         call. = FALSE
    )
  }
  edges <- get_edges(x, ignore)
  vert <- get_vertices(x, ignore)
  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = vert)
}


#' @rdname igraph-conversion
#' @export
as_igraph.ceg <- function(x, ignore = x$name_unobserved, ...){
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" is needed for this function.",
         call. = FALSE
    )
  }
  edges <- get_edges(x, ignore)
  vert <- get_vertices(x, ignore)
  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = vert)
}


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
#' @param normalize_layout a logical value. If \code{TRUE}
#'        layout positions are scaled to the \code{[0,1]} interval.
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
                       node_label = function(node) 
                         ifelse(is.na(node$stage), "", node$stage),
                       edge_label = function(edge){
                         ifelse(is.na(edge$label), "", edge$label)
                       },
                       edge_label_options = function(edge){
                         return("sloped")
                       },
                       scale = 10, 
                       normalize_layout = TRUE,
                       node_shape = "circle",
                       node_inner_sep = "1mm",
                       node_minimum_size = "0.3cm",
                       node_draw_color = "black",
                       node_thickness = "very thick",
                       node_text_color = "black"){
  UseMethod("write_tikz", x)
}

#' @rdname write_tikz
#' @export
write_tikz.sevt <- function(x, layout = NULL, file = "", 
                       col = NULL, ignore = x$name_unobserved,
                       node_label = function(node) 
                         ifelse(is.na(node$stage), "", node$stage),
                       edge_label = function(edge){
                         ifelse(is.na(edge$label), "", edge$label)
                       },
                       edge_label_options = function(edge){
                         return("sloped")
                       },
                       scale = 10, 
                       normalize_layout = TRUE,
                       node_shape = "circle",
                       node_inner_sep = "1mm",
                       node_minimum_size = "0.3cm",
                       node_draw_color = "black",
                       node_thickness = "very thick",
                       node_text_color = "black") {
  
  edgs <- get_edges(x, ignore  = ignore)
  verts <- get_vertices(x, ignore = ignore)
  
  col <- make_stages_col(x, col, ignore = ignore)
  col <- lapply(col, function(cc) if (all(is.numeric(cc))){
    sapply(cc, function(ccc) palette()[ccc])
  }else cc)
  
  if (is.null(layout)){
    layout <- igraph::layout_with_sugiyama(as_igraph(x, ignore = ignore))$layout
    layout <- layout[,2:1]
    layout[,1] <- -layout[,1]
  }
  
  if (is.function(layout))
    layout <- layout(as_igraph(x, ignore = ignore))
  
  if (normalize_layout){
    layout[,1] <-  (layout[,1] - min(layout[,1])) / 
      (max(layout[,1]) - min(layout[,1]))
    layout[,2] <-  (layout[,2]- min(layout[,2])) / 
      (max(layout[,2]) - min(layout[,2]))
  }
  
  cat2 <- function(x) cat(x, file = file, append = TRUE)
  
  cat(paste0("\\begin{tikzpicture}[auto, scale=",scale,",\n"), file = file)
  
  nodestyle <- "\t%s/.style={%s,inner sep=%s,minimum size=%s,draw,%s,%s,fill=%s,text=%s},\n"
  c1 <- col2rgb(col[[1]][[1]])
  c1 <- sprintf("{rgb,255:red,%s; green,%s; blue,%s}" , c1[1], c1[2], c1[3])
  cat2(sprintf(nodestyle, 
               paste(verts[1, "var"], verts[1, "stage"],sep = "_"), 
               node_shape,
               node_inner_sep,
               node_minimum_size,
               node_draw_color,
               node_thickness,
               c1,
               node_text_color))
  for (v in names(x$tree[-1])){
    for (s in unique(x$stages[[v]])){
      c1 <- col2rgb(col[[v]][s])
      c1 <- sprintf("{rgb,255:red,%s; green,%s; blue,%s}" , c1[1], c1[2], c1[3])
      cat2(sprintf(nodestyle, paste(v, s,sep = "_"), 
                   node_shape,
                   node_inner_sep,
                   node_minimum_size,
                   node_draw_color,
                   node_thickness,
                   c1,
                   node_text_color))
    }
  }
  cat2("\t leaf/.style={circle,inner sep=1mm,minimum size=0.2cm,draw,very thick,black,fill=gray,text=black}")
  cat2("]\n")
  cat2("\n")
  
  
  for (i in 1:nrow(verts)) {
    vert <- verts[i, "name"]
    var <- verts[i, "var"]
    stage <- verts[i, "stage"]
    label <- node_label(verts[i,,drop = FALSE])
    
    if (is.na(var)){
      ##drawing leaves
      cat2(sprintf("\t\\node [leaf] (%s) at (%f, %f)\t{%s};\n", 
                   #var, 
                   vert, layout[i,1], layout[i,2], label))
    }else{
      ##drawing vertices
      cat2(sprintf("\t\\node [%s_%s] (%s) at (%f, %f)\t{%s};\n", 
                   var, stage, 
                   vert, layout[i,1], layout[i,2], label))
    }
    
    
  }
  cat2("\n")
  for (i in 1:nrow(edgs)){
    from <- edgs[i, "from"]
    to <- edgs[i, "to"]
    label <- edge_label(edgs[i, ,drop = FALSE])
    opt <- paste(edge_label_options(edgs[i, ,drop = FALSE]), 
                 collapse = ",")
    cat2(sprintf ("\t\\draw[->] (%s) -- node [%s]{%s} (%s);\n", 
                  from, opt, label, to))
  }
  
  cat2("\\end{tikzpicture}\n")
}

