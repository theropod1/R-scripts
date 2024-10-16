##function violins()

#' plot multiple violin plots
#' @param x plotting statistic (numeric vector) or formula object from which a plotting statistic and grouping variable can be extracter (i.e. of form x~group)
#' @param group grouping variable
#' @param horiz logical indicating whether to plot horizontally
#' @param order order of factor levels of categorical factor
#' @param data data.frame object containing x and y
#' @param xlab x axis label
#' @param ylab y axis label
#' @param col vector of border colors
#' @param fill vector of fill colors
#' @param lwd vector of line widths
#' @param lty vector of line types
#' @param xlim x limits (data limits used if NULL)
#' @param ylim y limits (data limits used if NULL)
#' @param dscale density scaling factors (numeric) to apply to individual violins
#' @param add logical whether to add to existing plot (default: FALSE)
#' @param ax whether to plot axes

#' @param ... other arguments to pass on to paleoDiv::viol() and plot()
#' @export violins
#' @importFrom paleoDiv viol
#' @examples 
#' data.frame(p=rnorm(50), cat=rep(c("A","B","B","B","B"),10))->d
#' violins(p~cat,d)

violins<-function(x, data=NULL, group=NULL, horiz=FALSE, order=NULL, xlab="", ylab="", col="black",fill="grey", lwd=1, lty=1,dscale=1,xlim=NULL, ylim=NULL,add=FALSE, ax=TRUE,...){
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
if(length(fill)<ncat) rep(fill,ncat)[1:ncat]->fill
if(length(lwd)<ncat) rep(lwd,ncat)[1:ncat]->lwd
if(length(lty)<ncat) rep(lty,ncat)[1:ncat]->lty
if(length(dscale)<ncat) rep(dscale,ncat)[1:ncat]->dscale##further dscale modifications here if scale-setting is introduced

## debugging
#print(data.frame(x,group))
#print(c(rx,ncat))
#print(levels(factor(group)))

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

##add viols
if(horiz==T){#horizontal viols
for(i in 1:ncat){#loop
paleoDiv::viol(x=x[group==cat[i]], pos=i, horiz=TRUE, fill=fill[i], col=col[i], lwd=lwd[i], lty=lty[i],dscale=dscale[i],...)
}#end loop

if(ax){
axis(1)
#axis(2,at=c(1:ncat), lwd=0, lab=NA)
mtext(side=2, at=c(1:ncat), text=cat, col=col)

}

}else{#vertical viols
for(i in 1:ncat){#loop
paleoDiv::viol(x=x[group==cat[i]], pos=i, horiz=FALSE, fill=fill[i], col=col[i], lwd=lwd[i], lty=lty[i],dscale=dscale[i],...)
}#end loop
if(ax){
axis(2)
mtext(side=1, at=c(1:ncat), text=cat, col=col)

}}


out<-1:ncat
names(out)<-cat
invisible(out)

}
