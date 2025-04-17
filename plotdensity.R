plotdensity<-function(x,vertical=TRUE,max=NULL, md=NULL, factor=NULL,...){
if(!inherits(x,"density")) density(x)->d
if(inherits(x,"density")) x->d

sum(d$y)*abs(diff(range(d$x)))->a0 #initial area

if(is.null(max)) max<-par("usr")[4]
if(is.null(md)) md<-max(d$y)

if(is.null(factor)) max/md->factor
d$y*factor->d$y

if(vertical) lines(x=d$x,y=d$y,...)
if(!vertical) lines(y=d$x,x=d$y,...)

invisible(list(d=d,area0=a0,fac=max/md,area1=sum(d$y)*abs(diff(range(d$x))),maxy0=md
,maxy1=max))
} ##XXX
