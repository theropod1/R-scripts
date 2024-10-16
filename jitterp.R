##function jitterp()

#' plot data as a jitter-plot
#' @param x x values to plot (if single value and y is a vector, plot is vertical)
#' @param y y value at which to plot (if single value, plot is horizontal)
#' @param width standard deviation for jitter
#' @param col color for points
#' @param alpha opacity for points
#' @param ... other parameters to be passed on to points()
#' @return nothing (adds the points to the open plotting device as a jitter plot
#' @importFrom graphics points
#' @importFrom stats rnorm
#' @importFrom paleoDiv add.alpha
#' @export jitterp
#' @examples
#' c(1,2,3,2,3,2,3,4,4)->tmp
#' hist(tmp)
#' jitterp(x=tmp, y=1, width=0.1)

jitterp<-function(x,y,width,col="black",alpha=0.5,...){

if(length(y)==1){
y<-rep(y,length(x))+rnorm(n=length(x), mean=0, sd=width)
}else if(length(x)==1){
x<-rep(x,length(y))+rnorm(n=length(y), mean=0, sd=width)
}else{
stop("Please make sure either x or y is length()==1")
}

points(x=x, y=y,col=paleoDiv::add.alpha(col,alpha),...)

invisible(data.frame(x,y))

}
