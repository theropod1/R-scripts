##function draw_chull()

#' Draw a convex hull for a dataset
#' @param x dataset (data.frame with x in first and y in second column) or vector with x values
#' @param y (optional) vector with y values
#' @param subset an optional subset of x (and y) given as a vector of indices, (row)names or logical condition.
#' @param fac an optional grouping variable for drawing multiple convex hulls at once
#' @param color the fill colors to use
#' @param border the border colors to use
#' @param alpha alpha value to use for the fill (defaults to 0.5)
#' @param lwd line width for polygons
#' @param lty line type for polygons
#' @param legend settings for legend, either a list() object containing legend settings, or FALSE (default) for not drawing a legend
#' @param ... other graphical parameters to pass on to polygon()
#' @return nothing (adds convex hull to plot
#' @importFrom grDevices chull
#' @export draw_chull

draw_chull<-function(x,y=NULL, subset=NULL, fac=NULL, color=ggcol, border=color, lwd=1, lty=1, alpha=0.5,legend=FALSE,...){
#define base function:
draw_chull_<-function(x,y=NULL, subset=NULL,...){

if(is.data.frame(x) | is.matrix(x)){
if(!is.null(subset)) x<-x[subset,]

chull(x)->ind
polygon(x[c(ind),1],x[c(ind),2],...)
}else{
	if(!is.null(subset)){
	x<-x[subset]
	y<-y[subset]}
chull(data.frame(x=x,y=y))->ind
polygon(x[c(ind)],y[c(ind)],...)
}
}

#helper function:
n<-function(x){
if(is.data.frame(x) | is.matrix(x)){return(nrow(x))
}else{return(length(x))}
}

if(is.null(fac)){
if(is.function(color)) color<-color(1)
if(is.function(border)) border<-border(1)

draw_chull_(x=x,y=y,subset=subset,col=add.alpha(color,alpha), border=border, lwd=lwd, lty=lty,...)

}else{
fac<-rep(fac,n(x))[1:n(x)]
factor(fac)->fac

#generate or cycle graphical settings
if(is.function(color)){color<-color(length(levels(fac)))}else{color<-rep(color,n(levels(fac)))[1:n(levels(fac))]}
if(is.function(border)){border<-border(length(levels(fac)))}else{border<-rep(border,n(levels(fac)))[1:n(levels(fac))]}
lwd<-rep(lwd,n(levels(fac)))[1:n(levels(fac))]
lty<-rep(lwd,n(levels(fac)))[1:n(levels(fac))]

for(i in 1:length(levels(fac))){
if(length(which(fac==levels(fac)[i]))>2) draw_chull_(x=x,y=y,subset=which(fac==levels(fac)[i]), col=add.alpha(color[i],alpha), border=border[i], lwd=lwd, lty=lty,...)
}


if(length(legend)==1){
if(legend!=FALSE) list(x=as.character(legend),bty="n")->legend
}

if(inherits(legend,"list")){

legend$legend<-levels(fac)
legend$fill<-add.alpha(color,alpha)
legend$border<-border
legend$text.col<-border
legend$col<-border
if(!("lty"%in%names(legend))) legend$lty<-NA
legend$lwd<-lwd
#print(legend)
do.call(graphics::legend, args=legend)

}


}

}
