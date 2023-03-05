#' sevt to graph structure
#'
#' Obtain the graph representation of a staged tree, either as adjacency
#' matrix or as edges list or as \pkg{igraph} object.
#' @name sevt2graph
#' @param x an object of class \code{\link{sevt}}.
#' @param ignore vector of stages which will be ignored and excluded,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @details This utility functions can be used to obtain the adjacency 
#' matrix or the edge list structure 
#' to build the staged tree using a graph package (e.g. \pkg{igraph}). 
#' We also provide a function \code{graph_from_sevt} to build directly the 
#' graph structure as in \pkg{igraph}. 
#' This can be useful, for example to plot the staged tree (see the examples).
NULL


#' @rdname sevt2graph
#' @return for \code{sevt2edges}: the edges list corresponding 
#'         to the staged tree represented by \code{x}
#' @export 
sevt2edges <- function(x, ignore = x$name_unobserved){
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

#' @rdname sevt2graph
#' @export
sevt2vert <- function(x, ignore = x$name_unobserved){
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
    now <- paste(now[ix], c(sapply(x$tree[[var[i]]], rep, sum(ix))), sep = "-")
    vert <- rbind(vert, data.frame(name = now, 
                                   stage = NA,
                                   var = NA))
  }
  return(vert)
}

#' @rdname sevt2graph
#' @return for \code{graph_from_sevt}: a graph object from the 
#' \pkg{igraph} package.
#' @examples 
#' \dontrun{
#' library(igraph)
#' library(ggplot2)
#' library(ggraph)
#' mod <- full(Titanic) |> stages_bhc()
#' g <- graph_from_sevt(mod)
#' ggraph(g, "sugiyama") + 
#' geom_edge_fan(aes( 
#'     label = label,
#'    label_pos = 0.5 + runif(length(label),-0.1,0.1)), 
#'    angle_calc = "along",show.legend = FALSE,check_overlap = FALSE,
#'    end_cap = circle(0.02, 'npc'),
#'    arrow = grid::arrow(angle = 25, 
#'                        length = unit(0.025, "npc"),
#'                        type = "closed")) +
#'  geom_node_point(aes(x = x, y = y, color = stage), size = 5, 
#'                  show.legend = FALSE) +
#"  ggforce::theme_no_axes() + coord_flip() + scale_y_reverse() + 
#' theme(aspect.ratio = 2) 
#' }
#' @export
graph_from_sevt <- function(x, ignore = x$name_unobserved){
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" is needed to plot ceg.",
         call. = FALSE
    )
  }
  edges <- sevt2edges(x, ignore)
  vert <- sevt2vert(x, ignore)
  igraph::graph_from_data_frame(edges, directed = TRUE, vertice = vert)
}



#' CEG to graph struct
#' 
#' Obtain the graph representation of a CEG, either as adjacency
#' matrix or as edges list or as \pkg{igraph} object.
#' @name ceg2graph
#' @details This utility functions can be used to obtain the adjacency 
#' matrix or the edge list structure 
#' to build the CEG using a graph package (e.g. \pkg{igraph}). 
#' We also provide a function \code{graph_from_ceg} to build directly the 
#' graph structure as in \pkg{igraph}.
#' This can be useful, for example to plot the staged tree (see the examples).
#' @examples
#' model <- stages_fbhc(full(PhDArticles))
#' model.ceg <- ceg(model)
#' ### adjmat
#' A <- ceg2adjmat(model.ceg)
#' ### edge list
#' E <- ceg2edges(model.ceg)
NULL


#' @rdname ceg2graph
#' @param x an object of class \code{\link{ceg}}.
#' @param endnode if the end node should be added.
#' @param ignore vector of stages which will be ignored and excluded,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @return for \code{ceg2adjmat}: the adj matrix corresponding to the CEG 
#' @export
ceg2adjmat <- function(x, ignore = x$name_unobserved, endnode = TRUE) {
  tree <- x$tree
  var <- names(tree)
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v){
    pos[[v]][x$stages[[v]] %in% ignore]
  })
  allignore <- c(unique(unlist(pos.ignore)))
  allpos <- c(unique(unlist(pos)), "END")
  wignore <- allpos %in% allignore
  k <- length(allpos)
  mat <- matrix(nrow = k, ncol = k, 0, dimnames = list(allpos, allpos))
  m <- 1
  for (i in 2:length(var)) {
    v <- var[i - 1]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      for (p2 in pos[[var[i]]][((ix - 1) * m + 1):(ix * m)]) {
        mat[p1, p2] <- mat[p1, p2] + 1
      }
    }
  }
  v <- var[i]
  if (endnode){
    for (p1 in unique(pos[[v]])) {
      mat[p1, "END"] <- length(x$tree[[v]])
    }
  }
  mat <- mat[!wignore, !wignore]
  return(mat)
}


#' @rdname ceg2graph
#' @return for \code{ceg2edges}: the edges list 
#'         corresponding to the CEG \code{x} 
#' @export
ceg2edges <- function(x, ignore = x$name_unobserved, endnode = TRUE){
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
  edges <- data.frame(from = NA, to = NA, label = NA, stage = NA)
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
                                  stage = x$stages[[v]][ix]))
        }
      }
    }
  }
  v <- var[i]
  if (endnode){
    for (p1 in unique(pos[[v]])){
      if (!(p1 %in% allignore)){
        for (label in x$tree[[v]]){
          edges <- rbind(edges, c(from = p1, to = "END", label = label, 
                                  stage = "END"))
        }
      }
    }
  }
  return(edges[-1,])
}

#' @rdname ceg2graph
#' @export
ceg2vert <- function(x, ignore = x$name_unobserved, endnode = TRUE){
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
  vert <- data.frame(name = NA, stage = NA)
  for (i in 1:length(var)){
    v <- var[i]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      if (!(x$stages[[v]][ix]) %in% ignore){
        vert <- rbind(vert, c(name = p1, stage = x$stages[[v]][ix]))
      }
    }
  }
  if (endnode){
    vert <- rbind(vert, c(name = "END", stage = "END"))
  }
  return(vert[-1,])
}

#' @rdname ceg2graph
#' @export
#' @return for \code{graph_from_ceg}: a graph object from the 
#' \pkg{igraph} package.
#' @examples 
#' \dontrun{
#' library(igraph)
#' g <- graph_from_ceg(model.ceg)
#' ### igraph plotting functions can be used 
#' plot(g, layout = layout.sugiyama)
#' ### igraph object can be also plotted with ggplot2 and ggraph
#' library(ggraph)
#' library(ggplot2)
#' ggraph(g, "sugiyama") + 
#'  geom_edge_fan(aes(label = label, 
#'                    color = label,
#'                    label_pos = 0.5 + runif(length(label),-0.1,0.1)), 
#'                angle_calc = "along",show.legend = FALSE,check_overlap = TRUE,
#'                end_cap = circle(0.02, 'npc'),
#'                arrow = grid::arrow(angle = 25, 
#'                                    length = unit(0.025, "npc"),
#'                                    type = "closed")) +
#'  geom_node_point(aes(x = x, y = y, color = stage), size = 3, show.legend = FALSE) +
#'  ggforce::theme_no_axes() + coord_flip() + scale_y_reverse()
#'  }
graph_from_ceg <- function(x, ignore = x$name_unobserved,
                           endnode = TRUE){
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package \"igraph\" is needed to plot ceg.",
         call. = FALSE
    )
  }
  edges <- ceg2edges(x, ignore, endnode = endnode)
  vert <- ceg2vert(x, ignore, endnode = endnode)
  igraph::graph_from_data_frame(edges, directed = TRUE, vertice = vert)
}

