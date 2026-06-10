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
#' @param v verbosity setting
#' @param ... other graphical parameters to pass on to polygon()
#' @return nothing (adds convex hull to plot)
#' @importFrom grDevices chull
#' @export draw_chull

draw_chull<-function(x,y=NULL, subset=NULL, fac=NULL, color=ggcol, border=color, lwd=1, lty=1, alpha=0.5, legend=FALSE,v=FALSE,...){
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

###main function definition

if(is.null(fac)){

if(is.function(color)) color<-color(1)

if(is.function(border)) border<-border(1)

if(length(alpha)<length(color)) rep(alpha,length(color))[1:length(color)]->alpha
if(v) print("A")
draw_chull_(x=x,y=y,subset=subset,col=add.alpha(color,alpha), border=border, lwd=lwd, lty=lty,...)

}else{
if(v) print("B")

fac<-rep(fac,n(x))[1:n(x)]
factor(fac)->fac

names(which(table(fac)>1))->fac2
factor(fac2)->fac2

#generate or cycle graphical settings
if(is.function(color)){color<-color(n(levels(fac2)))}else{

if(is.data.frame(color)){ #read from col_ass output
col2<-color
color<-character(n(levels(fac2)))

for( i in 1:n(levels(fac2)) ){

if(!is.na(levels(fac2)[i])) col2$FUN[col2$lev==levels(fac2)[i]]->color[i]

}
print(color)
}

color<-rep(color,n(levels(fac2)))[1:n(levels(fac2))]}


if(is.function(border)){border<-border(length(levels(fac2)))}else{border<-rep(border,n(levels(fac2)))[1:n(levels(fac2))]}
lwd<-rep(lwd,n(levels(fac2)))[1:n(levels(fac2))]
lty<-rep(lwd,n(levels(fac2)))[1:n(levels(fac2))]

if(length(alpha)<length(color)) rep(alpha,length(color))[1:length(color)]->alpha

fillcol<-character()

for(i in 1:length(levels(fac2))){
fillcol[i]<-add.alpha(color[i],alpha[i])

if(length(which(fac==levels(fac2)[i]))>2) draw_chull_(x=x,y=y,subset=which(fac==levels(fac2)[i]), col=add.alpha(color[i],alpha[i]), border=border[i], lwd=lwd, lty=lty,...)

}


if(length(legend)==1){
if(legend!=FALSE) list(x=as.character(legend),bty="n")->legend
}

if(inherits(legend,"list")){

if(!exists("fillcol")) fillcol<-add.alpha(color,alpha[1])

legend$legend<-levels(fac2)
legend$fill<-fillcol
legend$border<-border
legend$text.col<-border
legend$col<-border
if(!("lty"%in%names(legend))) legend$lty<-NA
legend$lwd<-lwd
#print(legend)
do.call(graphics::legend, args=legend)

}


}

}##




##function: col_ass()
# assign colors or other values using a desired function (e.g. viridis(), ggcol()) or a vector of values based on unique values or factor levels in a variable x
#' @param x variable used to generate color vector
#' @param FUN Function used to generate a color vector using FUN(length(unique(x))) or equivalent, or vector with same length as unique values of x
#' @param na what to do about NA values in x. Default value NA keeps them as they are and substitutes NA (usually interpreted downstream as absence). Can be a color name or hex code or any other unique value to substitute for NA values in the result.
#' @param nam optional vector of names to assign to the output (defaults to names of x, if present)
#' @param order should unique values of x be odered or unordered? if a numeric or character vector is given here, it is applied as indices to factor levels of x in an attempt to order them.
#' @param overview return an overview table giving unique values of x and assigned color (or other) values
#' @param plus value to add to the number of unique x values if FUN is a function (useful if some "spares" are desired in color vector)
#' @param v verbosity setting (logical)
#' @export col_ass
#' @return a vector with values assigned to each element in x (if overview==FALSE), or a data.frame with each unique value of x and each assigned value.

col_ass<-function(x,FUN=ggcol,na=NA, nam=names(x), order=FALSE, overview=FALSE, plus=0, v=FALSE){

is_numeric_like<-function(x){#to test if endresult is numeric
as.numeric(x)->x_
if(any(!is.na(x_))) return(TRUE) else return(FALSE)
}

if(is.null(na)){
x[is.na(x)]<-"<NA>"
}

if(is.logical(order) && !order){

unique(x)->lev
if(any(is.na(lev))) c(lev[which(!is.na(lev))],NA)->lev

}else{
levels(factor(x))->lev
if(any(is.na(unique(x)))) lev<-c(lev,NA)
}


if(!is.logical(order)){

if(is.character(order)){
whichorderin(order,as.character(lev))->or
}else{order->or
}
c(or,which(!(1:length(lev))%in%or))->or

lev[or]->lev
}

if(is.function(FUN)) FUN2<-FUN(sum(!is.na(lev))+plus)[1:sum(!is.na(lev))] else FUN2<-FUN[1:sum(!is.na(lev))]

if(v) print(lev)
if(any(is.na(lev))) FUN2<-c(FUN2,na)
names(FUN2)<-lev
if(v) print(FUN2)

if(is_numeric_like(FUN2)) as.numeric(FUN2)->FUN2
if(overview){
names(FUN2)<-NULL
return(data.frame(lev=lev,FUN=FUN2))
}else{

out<-character(length(x))
for(i in 1:length(x)){
if(!is.na(x[i])) out[i]<-FUN2[which(lev==x[i])]
if(v) print(x[i])
if(is.na(x[i])) out[i]<-na
}
names(out)<-nam
if(is_numeric_like(out)) as.numeric(out)->out

return(out)

}

}
##

