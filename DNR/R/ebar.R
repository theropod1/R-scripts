##function ebar()
#' Plot error bars (or confidence polygons) for a series of values
#'
#' @param x Either a numeric vector of x values at which to plot error bars, or a data.frame containing any or all of the following collumns.
#' @param upper Upper end of error bars to plot.
#' @param lower Lower end of error bars to plot.
#' @param y Y values of the points for which to plot error bars
#' @param plusminus Error margin based on which to construct error bars around y values.
#' @param vertical Logical indicating whether to plot vertical error bars (if FALSE, the x values are interpreted as y values and the y values/upper/lower values as x values)
#' @param angle Angle for arror heads at ends of error bars
#' @param code Arrow code for plotting error bars (defaults to 3, i.e. head at both ends of error bar)
#' @param polygon Logical indicating whether to plot error as bars (if FALSE, default) or as polygon (if TRUE). Note that when plotting a polygon, the values are automatically reordered along the x values.
#' @param alpha Opacity to use (for polygon only), defaults to 0.5
#' @param col (fill) color to use for error bars (or polygon)
#' @param border Border color to use (for polygon only)
#' @param ... Other named parameters to be passed on to arrows() or polygon()
#' @details ebar is made to flexibly accept either a mean value and error margins (e.g. standard errors or deviations) to be added and subtracted from the mean, or a lower and upper absolute value, depending on the settings.
#' @return Adds error bars of specified length or with specified lower and upper ends to the current plot.
#' @importFrom graphics arrows
#' @export ebar
#' @examples
#' plot(c(1,2,3,4,5,6)~c(2,3,4,5,6,7))
#' ebar(c(2,3,4,5,6,7), y=c(1,2,3,4,5,6), plusminus=0.3)


ebar<-function(x, upper=NULL, lower=NULL, y=NULL, plusminus=NULL, vertical=TRUE,angle=90,code=3, alpha=0.5, col="black", border=NA,polygon=FALSE,...){
#prep points
if(is.data.frame(x)){
if("x" %in% colnames(x) & "y" %in% colnames(x) & is.null(y)){
x$y->y
x$x->x

}else{
y<-x[,2]
x<-x[,1]
}

if(is.null(lower)){

if("lwr" %in% colnames(x)){
x$lwr->lower
}
if("lower" %in% colnames(x)){
x$lower->lower
}

}

if(is.null(upper)){
if("upr" %in% colnames(x)){
x$lwr->lower
}
if("upper" %in% colnames(x)){
x$lower->lower
}

}

}

#prep error bar end points
if(!is.null(plusminus) & !is.null(y)){
lower<-y-plusminus
upper<-y+plusminus
}

if(polygon==TRUE){
lower<-lower[order(x)]
upper<-upper[order(x)]
x<-x[order(x)]
}

if(vertical==TRUE){
if(polygon==FALSE){
arrows(x0=x, x1=x, y0=lower, y1=upper, angle=angle,code=code,col=col,...)
}else{
polygon(x=c(x,rev(x)), y=c(lower,rev(upper)), col=add.alpha(col, alpha), border=border,...)

}

}else{

if(polygon==FALSE){
arrows(y0=x, y1=x, x0=lower, x1=upper, angle=angle,code=code,col=col,...)
}else{

polygon(y=c(x,rev(x)), x=c(lower,rev(upper)), col=add.alpha(col, alpha), border=border,...)

}


}

}
