#' plot a stratified event tree
#'
#' @param x event tree object
#' @param ...
#' @export
#' @importFrom graphics lines plot.new plot.window

plot.strt_ev_tree <- function(evt, ...){
 plot.new()
 plot.window(xlim=c(0,length(evt$tree)),
             ylim=c(-length(evt$tree)-0.5,length(evt$tree)+0.5),
             asp=1, ...)

 circle(c(0,0),1) #plot first node
 ns <- 1
 x<-0
 for (v in evt$tree){ #plot nodes for every strata
   x<- x + 2
   ns <- ns * length(v)
   y <- length(evt$tree)*seq(from=-ns, to=ns , length.out= ns)/(ns)
   for (a in y){
     circle(c(x, a), max(2/ns,0.1))
   }
 }
}

# simple function to plot circle on existing plot
circle<-function(x,r){
 tt <- 2*pi*(-1:20)/20
 lines( x=x[1] + r*cos(tt), y=x[2] + r*sin(tt)  )
}
