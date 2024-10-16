##function multijitter()

#' plot multiple jitter plots
#' @param x plotting statistic (numeric vector) or formula object from which a plotting statistic and grouping variable can be extracter (i.e. of form x~group)
#' @param group grouping variable
#' @param horiz logical indicating whether to plot horizontally
#' @param order order of factor levels of categorical factor
#' @param data data.frame object containing x and y
#' @param xlab x axis label
#' @param ylab y axis label
#' @param col vector of border colors
#' @param pch vector of symbols
#' @param xlim x limits (data limits used if NULL)
#' @param ylim y limits (data limits used if NULL)
#' @param width standard deviation for jitter
#' @param ax whether to plot axes
#' @param add logical whether to add to existing plot (default: TRUE)
#' @param ... other arguments to pass on to jitterp() and plot()
#' @importFrom graphics axis
#' @importFrom graphics mtext
#' @export multijitter
#' @examples 
#' data.frame(p=rnorm(50), cat=rep(c("A","B","B","B","B"),10))->d
#' multijitter(p~cat,d, add=FALSE)

multijitter<-function(x, data=NULL, group=NULL, horiz=FALSE, order=NULL, xlab="", ylab="", col="black", pch=16, width=0.1, xlim=NULL, ylim=NULL,add=TRUE,ax=FALSE,...){
if(inherits(x,"formula")){

if(x[1]!=`~`()) stop("no ~ operator found in formula supplied to x")

if(!is.null(data)){
data[[as.character(x[2])]]->x_
data[[as.character(x[3])]]->group
x_->x
}else{
get(as.character(x[2]))->x_
get(as.character(x[3]))->group
x_->x
}
#print(data.frame(x=x,group=group))

}


##xrange and number of categories
range(x)->rx
levels(factor(group))->cat
length(cat)->ncat

if(!is.null(order)) cat[order]->cat

##visual settings
if(length(col)<ncat) rep(col,ncat)[1:ncat]->col
if(length(pch)<ncat) rep(pch,ncat)[1:ncat]->pch

##conditional plot limits
if(horiz){
if(is.null(xlim)) xlim<-rx
if(is.null(ylim)) ylim<-c(0,ncat+1)
}else{
if(is.null(xlim)) xlim<-c(0,ncat+1)
if(is.null(ylim)) ylim<-rx
}


##now plot
if(add==FALSE) plot(NA,type="n", axes=F, ylim=ylim, xlim=xlim,xlab=xlab, ylab=ylab,...)#base plot

out<-list()

##add jitterplots
if(horiz==T){#horizontal viols
for(i in 1:ncat){#loop


jitterp(x=x[group==cat[i]], y=i, width=width, col=col[i], pch=pch[i],...)->out[[i]]


}#end loop

if(ax==TRUE){
axis(1)
}

}else{#vertical viols
for(i in 1:ncat){#loop


jitterp(y=x[group==cat[i]], x=i, width=width, col=col[i], pch=pch[i],...)->out[[i]]


}#end loop

if(ax==TRUE){
axis(2)
}

}

invisible(out)

}
