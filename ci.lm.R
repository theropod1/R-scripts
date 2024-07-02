##function ci.lm()
#' Plot a linear model with confidence or prediction interval
#'
#' @param lm Model (output of lm()) to be plotted
#' @param varname Name of the predictor variable (defaults to using the second coefficient name of the model).
#' @param predvar variable(s) to use as predictor variable, if distinct from plotting variable. Can be a vector (bivariate regression) or data.frame. In the case ofa data.frame, colnames are assumed to correspond to the variable names used for the model supplied in lm. If NULL (default), the x variable of the plot is assumed to correspond to the predictor variable for the regression.
#' @param plotvar plotting variable corresponding to each predictor variable in predvar, if not NULL
#' @param transformation Transformation to apply. Defaults to NUll (i.e. no transformation), can also be "log", "log10", or any function to be used on the output of predict to (re)transform the predicted values. If "log" or "log10", exp() and 10^ are automatically used to transform the output as appropriate for a model fitted using a call of the type lm(log(y)~log(x)) or equivalent.
#' @param limits X axis/independent variable limits within which to plot
#' @param col Color for main regression line
#' @param fill Color for confidence or prediction polygon (defaults to col). If set to NA, plotting of polygon is suppressed.
#' @param border Color for confidence or prediction range If set to NA (default), no border is drawn
#' @param alpha Alpha value for polygon fill (defaults to 0,5). Set to 0 to suppress polygon.
#' @param interval Type of interval to draw. Can be "confidence" or "prediction". See ?predict for details
#' @param level Confidence level for interval, defaults to 0.9 (i.e. a 90% confidence or prediction interval)
#' @param lt.border Line type for border
#' @param lw.border Line width for border
#' @param subdivisions The number of linear segments into which to split the interval for plotting
#' @param type type of input data to be used when converting the independent variable (defaults to as.numeric(), can also for example be as.Date)
#' @param ... Other named parameters to be passed on to lines()
#' @return Adds the graph for the model to the active plotting device, including a polygon showing the uncertainty range
#' @importFrom stats predict
#' @importFrom grDevices dev.cur
#' @importFrom graphics polygon
#' @importFrom graphics lines
#' @export ci.lm
#' @examples
#' dd<-data.frame(x=c(1,2,3,4,5),y=c(2,3.3,4,4.7,8))
#' lm(y~x,data=dd)->m1
#' lm(log(y)~log(x),data=dd)->m2
#' #plot data:
#' plot(dd) 
#' #add log-transformed model:
#' ci.lm(m1,alpha=0.5,lty=1,col="red",lwd=2,limits=c(1.5,4.5),lt.border=2,border="red")
#' #add log-transformed model:
#'ci.lm(m2,alpha=0.5,transformation="log",lty=1,col="blue",lwd=2,limits=c(1.5,4.5),lt.border=2,border="blue")


ci.lm<-function(lm,varname=NULL, plotvar=NULL, predvar=NULL, transformation=NULL, limits=NULL, col="black",fill=col,border=NA,alpha=0.5,interval="confidence",level=0.9,lt.border=2,lw.border=1,subdivisions=200,type=as.numeric,...){
if(dev.cur()==1){stop("ERROR: No open plotting device to add to")}
vnset<-FALSE

if(!is.null(limits)){
range(limits)->xl
addmar<-0
}else{
if(grepl("loess",summary(lm)$call[1])==TRUE){#if loess model is given
range(summary(lm)$x)->xl
addmar<-0}else{
range(par("usr")[1:2])->xl
addmar<-5}
}
diff(xl)->dl
dl/subdivisions->inc
seq(xl[1]-addmar*inc,xl[2]+addmar*inc,inc)->x

if(is.null(plotvar) | is.null(predvar)){
data.frame(x=type(x))->df
}else{##if third variable is to be used for prediction ##experimental
if(is.data.frame(predvar)){#check length predvar
lpv<-nrow(predvar)
}else{
lpv<-length(predvar)
}

if(lpv!=length(plotvar)){
stop("plotvar and predvar need to have the same length() or nrow()!")
}


if(is.data.frame(predvar)){###
predvar->df
vnset<-TRUE#indicate that df already has correct varnames
}else{data.frame(x=type(predvar))->df}


x<-plotvar

#print(head(df),10)
#print(tail(df),10)

}

if(is.null(varname)){
if(grepl("loess",summary(lm)$call[1])==TRUE){#if loess model is given
as.character(lm$xnames[1])->varname ##if adapted to plot multivariate models, remove indices here
}else{#is assuming lm model
as.character(names(lm$coefficients)[2])->varname##if adapted to plot multivariate models, remove indices here
}
}

if(is.character(transformation)){#if log or log10, automatically adjust varname
gsub(transformation,"",varname)->varname
gsub("\\(","",varname)->varname
gsub("\\)","",varname)->varname
}

##set varnames for input data
if(vnset==FALSE){
colnames(df)<-varname}###
#print(head(df))

if(grepl("loess",summary(lm)$call[1])==TRUE){#if loess model is given

predict(lm,df, se=T)->preds

preds$fit->point

preds$fit-qt(level,preds$df)*preds$se.fit->lwr
preds$fit+qt(level,preds$df)*preds$se.fit->upr

if(interval=="prediction"){
message("Prediction interval currently unavailable for loess model, plotting confidence interval instead.")
}
}else{#assume lm()

if(is.null(transformation)){
predict(lm,df,interval=interval,level=level)->preds
}else if(is.character(transformation)){
if(transformation=="log10"){
10^predict(lm,df,interval=interval,level=level)->preds
}else if(transformation=="log"){
exp(predict(lm,df,interval=interval,level=level))->preds
}}else{
transformation(predict(lm,df,interval=interval,level=level))->preds
}

preds[,1]->point
preds[,2]->lwr
preds[,3]->upr
}#end estimation

#throw out NA values
keep<-which(!is.na(point) & !is.na(lwr) & !is.na(upr))
if(!is.null(limits)){
if(is.numeric(limits) & length(limits)>1){
keep<-which(!is.na(point) & !is.na(lwr) & !is.na(upr) & x>=min(limits) & x<=max(limits))
}
}
point[keep]->point
lwr[keep]->lwr
upr[keep]->upr
x[keep]->x
df[keep,]->df

#print(head(data.frame(df,x,point,lwr,upr)))
#print(tail(data.frame(df,x,point,lwr,upr)))

#add polygon (if alpha is not 0 and fill is not NA
if(alpha>0 & !is.na(fill)){
polygon(c(x,rev(x)), c(lwr,rev(upr)), col=add.alpha(fill,alpha),border=NA)
}

#add top and bottom lines (if border is not NA)
if(!is.na(border)){
lines(lwr~x,col=border, lty=lt.border,lwd=lw.border)
lines(upr~x,col=border, lty=lt.border,lwd=lw.border)
}

#add regression line
lines(point~x,col=col,...)

}
