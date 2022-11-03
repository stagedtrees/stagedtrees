#' Chain event graph (CEG)
#'
#' Build the CEG representation from an object of class \code{\link{sevt}}.
#' @param object an object of class \code{sevt}.
#' @details An object of class \code{ceg} is a staged event tree object with
#'          additional information on the positions.
#' @return an object of class \code{ceg}.
#' @examples
#' DD <- generate_xor_dataset(3, 100)
#' model <- stages_bhc(full(DD))
#' model.ceg <- ceg(model)
#' model.ceg$positions
#' @export
ceg <- function(object) {
  check_sevt(object)
  positions <- object$stages
  ls <- length(object$stages)
  us <- unique(object$stages[[ls]])
  positions[[ls]] <- vapply(object$stages[[ls]],
    FUN.VALUE = "a",
    FUN = function(s) {
      as.character(which(us %in% s))
    }, USE.NAMES = FALSE
  )
  for (i in (ls - 1):1) {
    temp <- 2
    nv <- length(object$tree[[i + 1]])
    positions[[i]][] <- NA
    positions[[i]][1] <- 1
    for (j in 2:length(positions[[i]])) {
      nj <- (nv * (j - 1) + 1):(nv * j)
      for (k in 1:(j - 1)) {
        nk <- (nv * (k - 1) + 1):(nv * k)
        if (object$stages[[i]][j] == object$stages[[i]][k] &&
          all(positions[[i + 1]][nj] == positions[[i + 1]][nk])) {
          positions[[i]][j] <- positions[[i]][k]
        }
      }
      if (is.na(positions[[i]][j])) {
        positions[[i]][j] <- temp
        temp <- temp + 1
      }
    }
  }
  positions[[names(object$tree)[1]]] <- "1"
  object$positions <- positions
  object$positions <- object$positions[c(ls + 1, 1:ls)]
  class(object) <- c("ceg", class(object))
  return(object)
}


#' CEG to graph struct
#' 
#' Obtain the graph representation of a CEG, either as adjacency
#' matrix or as edges list.
#' @name ceg2graph
#' @details This utility functions can be used to obtain the adjacency 
#' matrix or the edge list structure 
#' to build the CEG using a graph package (e.g. \pkg{igraph}).
#' @examples
#' model <- stages_fbhc(full(PhDArticles))
#' model.ceg <- ceg(model)
#' ### adjmat
#' A <- ceg2adjmat(model.ceg)
#' ### edge list
#' E <- ceg2edges(model.ceg)

#' @rdname ceg2graph
#' @param x an object of class \code{\link{ceg}}.
#' @param endnode if the end node should be added.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#' @return for \code{ceg2adjmat}: the adj matrix
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
#' @return for \code{ceg2edges}: the edges list corresponding to the 
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
      #for (p2 in pos[[var[i]]][((ix - 1) * m + 1):(ix * m)]) {
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
#' g <- graph_from_ceg(model.ceg)
#' ### igraph plotting functions can be used 
#' plot(g, layout = layout.sugiyama)
#' ### igraph object can be also plotted with ggplot2 and ggraph
#' ggraph(g, "sugiyama") + 
#'  geom_edge_fan(aes(label = label, 
#'                    color = label,
#'                    label_pos = 0.5 + runif(length(label),-0.1,0.1)), 
#'                angle_calc = "along",show.legend = FALSE,check_overlap = TRUE,
#'                end_cap = circle(0.02, 'npc'),
#'                arrow = grid::arrow(angle = 25, 
#'                                    length = unit(0.025, "npc"),
#'                                    type = "closed")) +
#'  geom_node_circle(aes(x = y, y = x, r= 0.1, fill = stage), show.legend = FALSE) +
#'  ggforce::theme_no_axes() + coord_flip() + scale_y_reverse()
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
