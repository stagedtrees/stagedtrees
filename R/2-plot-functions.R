#' plot a stratified event tree
#'
#' @param x event tree object
#' @param ...
#' @export
#' @importFrom graphics lines plot.new plot.window

plot.strt_ev_tree <- function(evt, rmax=1, rmin= 0.1, step = 2, ...){
 plot.new()
 plot.window(xlim=c(0,step*length(evt$tree)),
             ylim=c(-step*length(evt$tree)/2-0.5,step*length(evt$tree)/2+0.5),
             asp=1, ...)
 n <- evt$tree
 nms <- names(evt$tree)
 d <- length(evt$tree)
 node(c(0,0),rmax,label = nms[1]) #plot first node
 ns <- 1
 x<-0
 y<-0
 yy <- 0
 coef <- 1
 for (k in 1:length(evt$tree)){ #plot nodes for every strata
   v <- evt$tree[[k]]
   yyy <- yy
   yy <- c()
   x <- step*k #increase x position
   nv <- length(v)
   for (i in 1:ns){ #for every old node
     y <- yyy[i] + (step/2)*coef*d*seq(from=-1, to = 1, length.out = nv)/(ns * nv  ) #compute new y positions
     yy <- c(yy,y)
     for (j in 1:nv){ #plot nodes
       node(c(x, y[j]), rmax/sqrt(nv*ns), label = nms[k+1])
       edge(c(x-step, yyy[i]), c(x,y[j]), v[j] )
     }
   }
   ns <- ns*nv
   if ( nv %% 2 == 1   ) {
     coef <- coef*0.4
   } else {
     coef <- 1*coef
   }
 }
}


node <- function(x, r, label = "", col = "black", ...){
  circle(x,r, col=col,...)
  text(x = x[1],y = x[2],labels = label, col=col,...)
}

edge <- function(from, to, label,col="black",...){
   lines(c(from[1], to[1]), c(from[2], to[2]), col = col, ...)
   a <- 180 * atan2((to[2] - from[2]),(to[1] - from[1]))/pi
   text(x = (from[1] + to[1]) / 2 , y = (from[2] + to[2]) / 2 ,
        labels = label, srt = a, col=col)
}

# simple function to plot circle on existing plot
circle<-function(x,r, col="black", ...){
 tt <- 2*pi*(-1:20)/20
 lines( x=x[1] + r*cos(tt), y=x[2] + r*sin(tt), col=col,...  )
}
