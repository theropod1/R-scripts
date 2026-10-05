##outlex()
#' return a truncated vector with the largest and smallest n values cropped off in order to exclude outliers
outlex<-function(x,n=1,x1=NULL,na.rm=TRUE){
if(na.rm){if(!is.null(x1)) x1<-x1[!is.na(x)]
x1<-x1[!is.na(x)]
}

if(!is.null(x1)) x1<-x1[order(x)]
sort(x)->x
seq_along(x)->ind
keep<-which(ind>n & ind<=(max(ind)-n))
if(!is.null(x1)) x1[keep] else x[keep]
}##

##EB()
#'plot error bars around a point in two dimensions, xrange and yrange.
EB<-function(xrange, yrange, centerpoint=NULL, angle=90, xtransform=identity, ytransform=xtransform, length=0.1, X=TRUE, Y=TRUE, code=3,...){
xrange0<-xrange
yrange0<-yrange
xtransform(xrange)->xrange
ytransform(yrange)->yrange

if(is.null(centerpoint) || !is.numeric(centerpoint) || length(centerpoint)<2){
c(mean(xrange0,na.rm=TRUE),mean(yrange0,na.rm=TRUE))->centerpoint
}

if(Y) arrows(xtransform(centerpoint[1]),min(yrange,na.rm=TRUE),xtransform(centerpoint[1]),max(yrange,na.rm=TRUE),angle=angle,length=length,code=code,...)
if(X) arrows(min(xrange,na.rm=TRUE),ytransform(centerpoint[2]),max(xrange,na.rm=TRUE),ytransform(centerpoint[2]),angle=angle,length=length,code=code,...)
}##


##filtersplit()
#' split up a data.frame containing 
filtersplit<-function(x,filtervar){ #define function to split the dataframe into family-wise data.frames in a list():
list()->out
if(is.character(filtervar) & length(filtervar)==1) x[,filtervar]->filtervar

for(i in levels(factor((filtervar)))){
if(is.data.frame(x) | is.matrix(x)) x[filtervar==i,]->out[[as.character(i)]] else x[filtervar==i]->out[[as.character(i)]]
}
out
}##

##function: bbplot (BetterBarplot)
#this function works much like the regular barplot() and takes the same matrix as input with groups to be stacked as rows and the time series as columns

bbplot<-function(data,names.arg=NULL,groups.arg=rownames(data),x=c(1:ncol(data)), width=1, xmin=x-width/2, xmax=x+width/2, horiz=TRUE, col=ggcol(nrow(data)), ax=FALSE,v=FALSE,add=FALSE,...){

#now extract and drop those rows from the data that contain x, xmax and xmin
if(is.character(x) & length(x)==1){
which(rownames(data)==x)->i
if(length(i)>0){
as.numeric(data[i,])->x
data[-i,]->data}
}

if(is.character(xmax) & length(xmax)==1){
which(rownames(data)==xmax)->i
if(length(i)>0){
as.numeric(data[i,])->xmax
data[-i,]->data}
}

if(is.character(xmin) & length(xmin)==1){
which(rownames(data)==xmin)->i
if(length(i)>0){
as.numeric(data[i,])->xmin
data[-i,]->data}
}

#order and cycle colors if needed
#if(length(col)<nrow(data)) rep(col,nrow(data))[1:nrow(data)]->col #cycle colors
if(is.null(names.arg) & !is.null(rownames(data))) rownames(data)->names.arg
if(is.null(names.arg)) names.arg<-x

if( !is.null(names(col)) && any(names.arg%in%names(col)) ){
namecols<-col[names(col)%in%names.arg]
nonamescols<-col[!(names(col)%in%names.arg)]
col_<-col
n0<-1
for(i in 1:length(names.arg)){

if(names.arg[i]%in%names(col_)) col_[names.arg[i]]->col[i] else{
nonamescols[n0]->col[i]
n0+1->n0
if(n0>length(nonamescols)) n0<-1
}
}
names(col)<-names.arg
rm(n0)
}

maxheights<-apply(data,2,FUN=sum, na.rm=TRUE)
	
if(add==FALSE && horiz) plot(1,1,type="n",axes=FALSE,xlab="",ylab="",xlim=range(c(xmin,xmax)),ylim=c(0,max(maxheights,na.rm=TRUE)))
if(add==FALSE && !horiz) plot(1,1,type="n",axes=FALSE,xlab="",ylab="",ylim=range(c(xmin,xmax)),xlim=c(0,max(maxheights,na.rm=TRUE)))

y0<-rep(0,nrow(data))
for(i in 1:nrow(data)){ #now loop over groups and stack bars
if(v) message("plotting ", groups.arg[i], " in ", col[i]," from ")
if(v) print(y0)
if(nrow(data)>1) coli<-col[i] else coli<-col
if(horiz) rect(xmin, y0, xmax, y0+as.numeric(data[i,]),col=coli,...) else rect(y0, xmin, y0+as.numeric(data[i,]), xmax, col=coli,...)

y0+as.numeric(data[i,])->y0
if(v) message("to")
if(v) print(y0)
}
if(ax==TRUE){ axis(1,at=x,labels=names.arg)
axis(2)
}

invisible(list(x=x, names=names.arg, col=col, groups=groups.arg, data=data)) #return invisible object with plot information
}##
