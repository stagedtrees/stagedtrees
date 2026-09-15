#' \pkg{igraph} conversion
#'
#' Obtain the graph representation of a staged tree or a CEG as
#' an object from the \pkg{igraph} package.
#' @name igraph-conversion
#' @param x an object of class \code{\link{sevt}}, \code{\link{ceg}} or
#' \code{parentslist}.
#' @param ignore vector of stages which will be ignored and excluded,
#'               by default the name of the unobserved stages stored in
#'               `x$name_unobserved`.
#'
#' @details Functions to translate the graph structure of a \code{\link{sevt}}
#' or \code{\link{ceg}} object to a graph object from the
#' \pkg{igraph} package.
#' Additional functions that extract the edge lists
#' and the vertices are available.
#' This can be useful, for example to plot the staged tree  with
#' \pkg{igraph} or additional packages (see the examples).
#' @examples
#' mod <- stages_bhc(full(Titanic))
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
#'   geom_edge_fan(
#'     aes(
#'       label = label,
#'       label_pos = 0.5 + runif(length(label), -0.1, 0.1)
#'     ),
#'     angle_calc = "along", show.legend = FALSE, check_overlap = FALSE,
#'     end_cap = circle(0.02, "npc"),
#'     arrow = grid::arrow(
#'       angle = 25,
#'       length = unit(0.025, "npc"),
#'       type = "closed"
#'     )
#'   ) +
#'   geom_node_point(aes(x = x, y = y, color = stage),
#'     size = 5,
#'     show.legend = FALSE
#'   ) +
#'   ggforce::theme_no_axes() + coord_flip() + scale_y_reverse()
#'
#' ######## ceg example ########
#' g.ceg <- as_igraph(ceg(mod))
#' ### igraph plotting functions can be used
#' plot(g.ceg, layout = layout.sugiyama)
#' ### igraph object can be also plotted with ggplot2 and ggraph
#' ggraph(g.ceg, "sugiyama") +
#'   geom_edge_fan(
#'     aes(
#'       label = label,
#'       color = label,
#'       label_pos = 0.5 + runif(length(label), -0.1, 0.1)
#'     ),
#'     angle_calc = "along", show.legend = FALSE, check_overlap = FALSE,
#'     end_cap = circle(0.02, "npc"),
#'     arrow = grid::arrow(
#'       angle = 25,
#'       length = unit(0.025, "npc"),
#'       type = "closed"
#'     )
#'   ) +
#'   geom_node_point(aes(x = x, y = y, color = stage), size = 3, show.legend = FALSE) +
#'   ggforce::theme_no_axes() + coord_flip() + scale_y_reverse()
#' }
NULL

#' @rdname igraph-conversion
#' @param ... additional parameters.
#' @return for \code{get_edges}: the edges list corresponding
#'         to the graph associated to \code{x}.
#' @export
get_edges <- function(x, ignore = x$name_unobserved, ...) {
  UseMethod("get_edges", x)
}

#' @rdname igraph-conversion
#' @export
get_edges.sevt <- function(x, ignore = x$name_unobserved, ...) {
  check_sevt(x)
  tree <- x$tree
  var <- sevt_varnames(x)
  if (is.null(x$stages[[var[1]]])) { ## add stage name also to root
    x$stages[[var[1]]] <- c(NA)
  }
  edges <- data.frame(from = NA, to = NA, label = NA, var = NA, stage = NA)
  now <- "root"
  for (i in 1:length(var)) {
    v <- var[i]
    m <- length(x$tree[[v]])
    for (k in seq_along(now)) {
      stg <- x$stages[[v]][k]
      from <- now[k]
      if (!(stg %in% ignore)) {
        for (j in 1:m) {
          if (i < length(var)) {
            stg_to <- x$stages[[var[i + 1]]][m * (k - 1) + j]
            if (stg_to %in% ignore) {
              next
            }
          }
          lbl <- x$tree[[v]][j]
          to <- paste(from, lbl, sep = "-")
          edges <- rbind(edges, c(
            from = from, to = to, label = lbl,
            var = v, stage = stg
          ))
        }
      }
    }
    now <- apply(expand.grid(x$tree[i:1])[, i:1, drop = FALSE],
      MARGIN = 1, function(x) {
        paste0("root-", paste0(x, collapse = "-"))
      }
    )
  }
  return(edges[-1, ])
}


#' @rdname igraph-conversion
#' @return for \code{get_vertices}: the vertices list corresponding
#'         to the graph associated to \code{x}.
#' @export
get_vertices <- function(x, ignore = x$name_unobserved, ...) {
  UseMethod("get_vertices", x)
}

#' @rdname igraph-conversion
#' @export
get_vertices.sevt <- function(x, ignore = x$name_unobserved, ...) {
  tree <- x$tree
  var <- sevt_varnames(x)
  vert <- data.frame(name = "root", stage = NA, var = var[1])
  for (i in 2:length(var)) {
    v <- var[i]
    now <- apply(expand.grid(x$tree[(i - 1):1])[, (i - 1):1, drop = FALSE],
      MARGIN = 1, function(x) {
        paste0("root-", paste0(x, collapse = "-"))
      }
    )
    ix <- !(x$stages[[v]] %in% ignore)
    if (any(ix)) {
      vert <- rbind(vert, data.frame(
        name = now[ix],
        stage = x$stages[[v]][ix],
        var = v
      ))
    }
  }
  if (any(ix)) {
    now <- paste(
      c(vapply(now[ix],
        FUN = rep,
        FUN.VALUE = x$tree[[var[i]]],
        length(x$tree[[var[i]]])
      )),
      x$tree[[var[i]]],
      sep = "-"
    )
    vert <- rbind(vert, data.frame(
      name = now,
      stage = NA,
      var = NA
    ))
  }
  return(vert)
}

#' @rdname igraph-conversion
#' @export
get_edges.ceg <- function(x, ignore = x$name_unobserved, ...) {
  tree <- x$tree
  var <- sevt_varnames(x)
  if (is.null(x$stages[[var[1]]])) { ## add stage name also to root
    x$stages[[var[1]]] <- c("1")
  }
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v) {
    pos[[v]][x$stages[[v]] %in% ignore]
  })
  allignore <- c(unique(unlist(pos.ignore)))
  allpos <- c(unique(unlist(pos)), "END")
  wignore <- allpos %in% allignore
  k <- length(allpos)
  m <- 1
  edges <- data.frame(from = NA, to = NA, label = NA, stage = NA, var = NA)
  for (i in 2:length(var)) {
    v <- var[i - 1]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      for (j in 1:m) {
        p2 <- pos[[var[i]]][ix * m - m + j]
        if (!(p2 %in% allignore)) {
          edges <- rbind(edges, c(
            from = p1, to = p2,
            label = x$tree[[v]][j],
            stage = x$stages[[v]][ix],
            var = v
          ))
        }
      }
    }
  }
  v <- var[i]
  ## add endonode
  for (p1 in unique(pos[[v]])) {
    if (!(p1 %in% allignore)) {
      for (label in x$tree[[v]]) {
        ix <- min(which(pos[[v]] %in% p1))
        edges <- rbind(edges, c(
          from = p1, to = "END", label = label,
          stage = x$stages[[v]][ix], var = v
        ))
      }
    }
  }
  return(edges[-1, ])
}

#' @rdname igraph-conversion
#' @export
get_vertices.ceg <- function(x, ignore = x$name_unobserved, ...) {
  tree <- x$tree
  var <- sevt_varnames(x)
  if (is.null(x$stages[[var[1]]])) { ## add stage name also to root
    x$stages[[var[1]]] <- c("0")
  }
  pos <- uni_idx(x$positions)
  pos.ignore <- lapply(var[-1], function(v) {
    pos[[v]][x$stages[[v]] %in% ignore]
  })
  allignore <- c(unique(unlist(pos.ignore)))
  allpos <- c(unique(unlist(pos)), "END")
  wignore <- allpos %in% allignore
  k <- length(allpos)
  m <- 1
  vert <- data.frame(name = NA, stage = NA, var = NA)
  for (i in 1:length(var)) {
    v <- var[i]
    m <- length(x$tree[[v]])
    for (p1 in unique(pos[[v]])) {
      ix <- min(which(pos[[v]] %in% p1))
      if (!(x$stages[[v]][ix]) %in% ignore) {
        vert <- rbind(vert, c(name = p1, stage = x$stages[[v]][ix], var = v))
      }
    }
  }
  vert <- rbind(vert, c(name = "END", stage = "END", var = NA))
  return(vert[-1, ])
}

#' @rdname igraph-conversion
#' @return for \code{as.igraph}: a graph object from the
#' \pkg{igraph} package.
#' @export
as_igraph <- function(x, ignore = x$name_unobserved, ...) {
  UseMethod("as_igraph", x)
}

#' @rdname igraph-conversion
#' @export
as_igraph.sevt <- function(x, ignore = x$name_unobserved, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg igraph} is needed.",
      "x" = "Unable to load {.pkg igraph}."
    ))
  }
  edges <- get_edges(x, ignore)
  vert <- get_vertices(x, ignore)
  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = vert)
}


#' @rdname igraph-conversion
#' @export
as_igraph.ceg <- function(x, ignore = x$name_unobserved, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg igraph} is needed.",
      "x" = "Unable to load {.pkg igraph}."
    ))
  }
  edges <- get_edges(x, ignore)
  vert <- get_vertices(x, ignore)
  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = vert)
}

#' @rdname igraph-conversion
#' @export
as_igraph.parentslist <- function(x, ...){
  vert <- data.frame(name = names(x))
  edges <- data.frame(from = NA, to = NA, context = NA, partial = NA,
                      local = NA, label = NA)
  for (v in names(x)){
    to <- v
    if (!is.null(x[[v]]$parents)){
      for (w in x[[v]]$parents){
        from <- w
        context <- w %in% x[[v]]$context
        partial <- w %in% x[[v]]$partial
        local <- w %in% x[[v]]$local
        if (context & partial) label <- "context-partial"
        else if (context) label <- "context"
        else if (partial) label <- "partial"
        else if (local) label <- "local"
        else label <- "total"
        edges <- rbind(edges, c(
          from = from, to = to, context = context,
          partial = partial, local = local, label = label
        ))
      }
    }
  }
  edges <- edges[-1,]

  igraph::graph_from_data_frame(edges, directed = TRUE, vertices = vert)
}
