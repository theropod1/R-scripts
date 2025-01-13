##function regres()
#' plot the result of a regression as a labeled rectangle
#' @param model bivariate model to use to estimate y from x. If NULL, 
#' @param x values for independent variable (x)
#' @param y optional values for y. If model is not NULL, model overrides any setting for y
#' @param labelx Logical indicating whether to label the values for x (defaults to FALSE)
#' @param labely Logical indicating whether to label the values for y (defaults to TRUE)
#' @param CI Logical indicating whether to plot confidence interval
#' @param PI Logical indicating whether to plot prediction interval
#' @param suppress.center Logical indicating whether to suppress plotting the point estimate
#' @param vertical Logical indicating whether to plot the vertical line(s)
#' @param horizontal Logical indicating whether to plot the vertical line(s)

#' @param retransform Function containing transformation to apply to y values (e.g. exp() if regression was log-transformed)
#' @param level Coverage of the confidence/prediction intervals
#' @param col color for lines and labels
#' @param cex font size for labels
#' @param adj.y y label adjustment (numeric of length 1 or 2)
#' @param adj.x x label adjustment (numeric of length 1 or 2)
#' @param digits number of significant digits for labels
#' @param ... additional parameters to pass on to lines()
#' @return Nothing (plots the y values or regression results on the current plotting device)
#' @export regres
#' @importFrom stats approx lm


regres<-function(model=NULL,x,y=NULL,labelx=FALSE,labely=TRUE, CI=FALSE, PI=FALSE, suppress.center=FALSE, vertical=TRUE, horizontal=TRUE, retransform=NULL, level=0.9, col="black", cex=0.8, adj.y=c(0,-0.15), adj.x=c(1,-0.15), digits=4,...){

if(is.null(model) & is.null(y)){
stop("model or y values must be specified")
}

if(!is.null(model)){

if(length(names(model$coefficients))==1) names(model$coefficients)[1]->vname
if(length(names(model$coefficients))>1) names(model$coefficients)[2]->vname

length(x)->n
data.frame(x)->df
colnames(df)<-vname

as.numeric(predict(model, df))->y

if(CI){
c(y,as.numeric(predict(model, df, interval="confidence", level=level)[,2]),as.numeric(predict(model, df, interval="confidence", level=level)[,3]))->y

c(x,rep(df[,1],2))->x

}

if(PI){
c(y,as.numeric(predict(model, df, interval="prediction", level=level)[,2]),as.numeric(predict(model, df, interval="prediction", level=level)[,3]))->y

c(x,rep(df[,1],2))->x
}

}

if(!is.null(retransform)){
retransform(y)->y
}

if(suppress.center){
x<-x[-c(1:n)]
y<-y[-c(1:n)]
}

##draw lines

for(i in 1:length(x)){

#if(vertical & horizontal){
#lines(x=c(x[i],x[i],par("usr")[1]), y=c(par("usr")[3],y[i],y[i]), col=col,...)
#}else 
if(vertical){
lines(x=c(x[i],x[i]), y=c(par("usr")[3],y[i]), col=col,...)
if(labelx){

if(length(adj.x)<2){
adj.x[2]<-adj.x[1]
adj.x[1]<-0.5

}

text(x=x[i], y=approx(x=c(1,0), y=range(c(par("usr")[3],y[i])), xout=adj.x[1])$y, signif(x[i],digits), col=col, cex=cex, adj=adj.x,srt=270)

print(c(x[i],approx(x=c(1,0), y=range(c(par("usr")[3],y[i])), xout=adj.x[1])$y))

}
}

if(horizontal){
lines(x=c(x[i],par("usr")[1]), y=c(y[i],y[i]), col=col,...)
##draw labels

if(labely){
text(y=y[i], x=approx(x=c(0,1), y=range(c(x[i],par("usr")[1])), xout=adj.y[1])$y, signif(y[i],digits),col=col, cex=cex, adj=adj.y)
}
}





}

}##
