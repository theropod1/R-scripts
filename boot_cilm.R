##function boot_cilm()
#' construct a linear model confidence interval via bootstrapping
#'
#' @param x x values
#' @param y y values
#' @param data (optional) data.frame from which to draw x and y
#' @param reps number of bootstrap repetitions (default 1000)
#' @param range (optional) range across which to calculate confidence or prediction band (defaults to the width of current plot if xout is not specified)
#' @param steps (optional) number of steps across range
#' @param xout (optional) x value at which to calculate confidence or prediction band for y
#' @param quantiles quantiles at which to calculate  at which to calculate confidence or prediction band margins
#' @param level desired confidence level (overrides quantiles)
#' @param plot logical indicating whether to plot results
#' @param interval "confidence" | "prediction", defaults to "confidence", for construction of bootstrap confidence interval. if == "prediction", a randomly sampled residual is added to the prediction from each resampled model as an error term.
#' @param ... additional plotting arguments to be passed on to ebar()
#' @importFrom stats residuals
#' @importFrom stats quantile
#' @export boot_cilm
#' @examples
#' rnorm(70)->all
#' data.frame(tl=all,ujp=rnorm(n=70,mean=450, sd=0.1))->all
#' boot_cilm(x=log(all$tl), y=log(all$ujp), xout=log(seq(1000,7000,500)), plot=FALSE)->b
#' ebar(x=exp(b$interval$x), upper=exp(b$interval$upr), lower=exp(b$interval$lwr),col="red",polygon=TRUE)
#'
#' t<-data.frame(x=rnorm(100))
#' t$y<-rnorm(100,sd=0.3,mean=t$x)
#' predict(lm(y~x,t), data.frame(x=seq(-3,3,0.1)),interval="confidence")->t2
#' plot(t)
#' ci.lm(lm(y~x,t))
#' ebar(x=seq(-3,3,0.1), upper=t2[,3], lower=t2[,2],polygon=TRUE)
#' boot_cilm(x,y,data=t, xout=0.5,plot=FALSE,interval="prediction")->a
#' abline(a$lm)
#' if(!exists("preds_")){
#' predsCI<-matrix(ncol=length(a$lms),nrow=length(seq(par("usr")[1],par("usr")[2], length=100)))
#' predsPI<-matrix(ncol=length(a$lms),nrow=length(seq(par("usr")[1],par("usr")[2], length=100)))
#' 
#' preds_<-data.frame(x=seq(par("usr")[1],par("usr")[2], length=100))
#' 
#' for(i in 1:length(a$lms)){
#' predict(a$lms[[i]], preds_)+sample(residuals(a$lms[[i]]),1)->predsPI[,i]#for PI
#' predict(a$lms[[i]], preds_)->predsCI[,i]#for CI
#' }
#' 
#' apply(predsCI[,], 1, FUN=quantile, na.rm=TRUE, probs=0.05)->preds_$lwr
#' apply(predsCI[,], 1, FUN=quantile, na.rm=TRUE, probs=0.95)->preds_$upr
#' apply(predsPI[,], 1, FUN=quantile, na.rm=TRUE, probs=0.05)->preds_$lwr_PI
#' apply(predsPI[,], 1, FUN=quantile, na.rm=TRUE, probs=0.95)->preds_$upr_PI
#' }
#' lines(preds_$lwr_PI~preds_$x, lty=2,col=add.alpha("black"))
#' lines(preds_$upr_PI~preds_$x, lty=2,col=add.alpha("black"))
#' ebar(preds_,polygon=TRUE,alpha=0.2)

##
boot_cilm<-function(x,y,data=NULL, reps=1000,range=NULL,steps=101,xout=NULL,quantiles=c(0.05,0.95),level=0.9, plot=TRUE, interval="confidence",...){

if(is.null(range) & is.null(xout)) range<-par("usr")[1:2] #if range and xout are both null, use current plotting device for range

list()->out
if(!is.null(data)){##pull variables from data if possible
if(inherits(data,c("data.frame","list"))){
x<-data[[paste(substitute(x))]]
y<-data[[paste(substitute(y))]]
}}

if(length(x)!=length(y)) stop("y and x not same length!")
#prepare xout
if(is.null(xout)) xout<-seq(range[1], range[2],diff(range)/steps)

if(is.null(quantiles) & is.null(level)) stop("level or quantiles must be specified")
if(!is.null(level)) quantiles<-range(c(1-(1-level)/2,lwr<-0+(1-level)/2))
quantiles<-range(quantiles)

#prepare list
out$preds<-data.frame(x=xout)
out$lm<-lm(y~x)

out$lms<-list()

##loop and sample
for(i in 1:reps){
sample(c(1:length(x)),replace=TRUE)->ind
x[ind]->x_
y[ind]->y_
dat<-data.frame(x=x_,y=y_)
lm(y~x, data=dat)->lm_

out$lms[[i]]<-lm_


if(interval=="confidence") predict(lm_, out$preds)->out$preds[,i+1]
if(interval=="prediction") predict(lm_, out$preds)+sample(residuals(lm_),1)->out$preds[,i+1]


}#end loop and sample
#print(out$preds)
#return(out$preds)
if(1==1){
##aggregate interval values
out$interval<-data.frame(x=xout)
predict(out$lm, out$interval)->out$interval$fit

apply(out$preds[,-1], 1, FUN=quantile, na.rm=TRUE, probs=quantiles[1])->out$interval$lwr
apply(out$preds[,-1], 1, FUN=quantile, na.rm=TRUE, probs=quantiles[2])->out$interval$upr

if(plot){
ebar(x=xout,lower=out$interval$lwr, upper=out$interval$upr, polygon=TRUE,...)
}
if(nrow(out$interval)<20) print(out$interval)

invisible(out)
}

}
##



##function bootCI()
#' basic bootstrap confidence interval 
#'
#' @param x univariate dataset for which to calculate CI
#' @param fun function to apply across reps
#' @param level desired confidence level
#' @param reps number of bootstrap repetitions
#' @param CI logical whether to return CI (otherwise all results are returned as a vector)
#' @param wt optional vector of weights (defaults to equal weighting of all values in x), must be same length as x
#' @param ... other arguments to pass on to fun
#' @details computes a bootstrap confidence interval for the result of any function that can be applied to a resampling of x
#' @return either a numeric of length 2 giving the confidence interval at the desired confidence level, or a numeric vector of length reps containing every individual result of fun
#' @export bootCI
##
bootCI<-function(x,fun=NULL,level=0.9,reps=1000, CI=TRUE,wt=1,...){
upr<-1-(1-level)/2
lwr<-0+(1-level)/2

if(length(wt)!=length(x)) rep(wt,length(x))[1:length(x)]->wt

x[which(!is.na(x))]->x

if(!is.null(fun)) replicate(reps,fun(sample(x,length(x),replace=TRUE,prob=wt),...))->x_
if(is.null(fun)) replicate(reps,sample(x,length(x),replace=TRUE,prob=wt),...)->x_

if(CI==FALSE){return(x_)
}else{quantile(x_,c(lwr,upr))->ci
return(ci)}
}
##
