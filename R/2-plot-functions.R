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

plot.strt_ev_tree <- function(x, rmax=1, rmin= 0.1, step = 2, limit = 10, ...){
 plot.new()
 d <- min(length(x$tree), limit) ##avoid too much plotting
 plot.window(xlim = c(0, step * d),
             ylim = c(- step * d/2 - 0.5,step * d / 2 + 0.5),
             asp = 1, ...)
 nms <- names(x$tree)
 node(c(0,0),rmax,label = nms[1]) #plot first node
 ns <- 1
 xx<-0
 y<-0
 yy <- 0
 coef <- 1
 for (k in 1:length(x$tree)){ #plot nodes for every strata
   v <- x$tree[[k]]
   yyy <- yy
   yy <- c()
   xx <- step*k #increase x position
   nv <- length(v)
   for (i in 1:ns){ #for every old node
     y <- yyy[i] + (step/2)*coef*d*seq(from=-1, to = 1, length.out = nv)/(ns * nv  ) #compute new y positions
     yy <- c(yy,y)
     for (j in 1:nv){ #plot nodes
       node(c(xx, y[j]), coef*rmax/sqrt(nv*ns), label = nms[k+1])
       edge(c(xx-step, yyy[i]), c(xx,y[j]), v[j] ) #plot edge with previous nodes
     }
   }
   ns <- ns*nv
   if ( nv %% 2 == 1   ) { ## this is to avoid overlapping,
     coef <- coef*0.5   ## not sure why it works like this but ok
   } else {
     coef <- 1*coef
   }
 }
}


#' plot a staged event tree
#'
#' @param x event tree object
#' @param rmax graphical parameter
#' @param rmin graphical parameter
#' @param step graphical parameter
#' @param limit maximum number of levels plotted
#' @param ... additional graphical parameters
#' @export
#' @importFrom graphics lines plot.new plot.window

plot.staged_ev_tree <- function(x, rmax=1, rmin= 0.1, step = 2, limit = 10,...){
  plot.new()
  d <- min(length(x$tree), limit) ##avoid too much plotting
  ttt <- prod(sapply(x$tree[1:d], length))
  plot.window(xlim = c(0,step * d),
              ylim = c( - step * (d+2) / 2 - 0.5, step * (d+2) / 2 + 0.5),
              asp = 1, ...)
  n <- x$tree
  nms <- names(x$tree)
  node(c(0,0),rmax,label = nms[1]) #plot first node
  ns <- 1
  xx<-0
  y<-0
  yy <- 0
  for (k in 1:d){ #plot nodes for every strata
    v <- x$tree[[k]]
    yyy <- yy
    yy <- c()
    lj <- 0
    xx <- step*k #increase x position
    nv <- length(v)
    for (i in 1:ns){ #for every old node
      y <- yyy[i] +  (step) * d  * seq(from=-1, to = 1, length.out = nv) / (ns * nv  + 2) #compute new y positions
      yy <- c(yy,y)
      for (j in 1:nv){ #plot nodes
        lj <- lj +1
        if (k < length(x$tree)) {
          node(c(xx, y[j]), rmax/sqrt(nv*ns),
               label = nms[k+1], col = x$stages[[k]][lj])
        }
        edge(c(xx-step, yyy[i]), c(xx,y[j]), v[j] ) #plot edge with previous nodes
      }
    }
    ns <- ns * nv
  }
}

#' Plot a node
#'
#' @param x the center
#' @param r the radius
#' @param label the label
#' @param col color
#' @param cex.label cex parameter to be passed to text
#' @param ... additional parameters passed to \code{par()}
#' @importFrom graphics text lines
node <- function(x, r, label = "", col = "black", cex.label = 2*sqrt(r),...){
  circle(x,r, col=col,...)
  text(x = x[1],y = x[2],labels = label, col=col, cex=cex.label,...)
}


#' Plot an edge
#'
#' @param from From
#' @param to To
#' @param label the label
#' @param col color
#' @param ... additional parameters passed to \code{par()}
#' @importFrom graphics text lines
edge <- function(from, to, label,col="black",...){
   lines(c(from[1], to[1]), c(from[2], to[2]), col = col, ...)
   a <- 180 * atan2((to[2] - from[2]),(to[1] - from[1]))/pi   ## compute the angle of the line
   text(x = (from[1] + to[1]) / 2 , y = (from[2] + to[2]) / 2 ,
        labels = label, srt = a, col=col)  ## put the label rotated of the proper angle
}

# simple function to plot circle on existing plot
circle<-function(x,r, col="black", ...){
 tt <- 2*pi*(-1:20)/20
 lines( x=x[1] + r*cos(tt), y=x[2] + r*sin(tt), col=col,...  )
}


