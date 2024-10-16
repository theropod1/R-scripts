##function: plotbg()
#' plot a colored box on the current plotting device
#' @param col color for the background
#' @param xy vector of coordinates for the box to draw
#' @param ... additional arguments to pass on to polygon()
#' @return nothing
#' @export plotbg
#' @importFrom graphics polygon
#' @examples
#' plot(NA,NA,type="n", xlim=c(1,2), ylim=c(1,2))
#' plotbg("grey")

plotbg<-function(col,xy=par("usr"),...){

rangelr<-abs(diff(range(xy[c(1,2,2,1)])))
lr<-xy[c(1,2,2,1)]


rangetd<-abs(diff(range(xy[c(3,3,4,4)])))
td<-xy[c(3,3,4,4)]

if(sum(xy==par("usr"))==length(xy==par("usr"))){#extend range if whole background is to be plotted
lr[which(lr==min(lr))]<-lr[which(lr==min(lr))]-2*rangelr
lr[which(lr==max(lr))]<-lr[which(lr==max(lr))]+2*rangelr
td[which(td==min(td))]<-td[which(td==min(td))]-2*rangetd
td[which(td==max(td))]<-td[which(td==max(td))]+2*rangetd
}

polygon(x=lr, y=td, col=col,...)


}##
