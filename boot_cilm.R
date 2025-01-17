##function boot_cilm()
#' construct a linear model confidence interval via bootstrapping
#'
#' @param x
#' @param y
#' @param data
#' @param reps
#' @param range
#' @param steps
#' @param xout
#' @param quantiles
#' @param level desired confidence level
#' @param plot logical indicating whether to plot results
#' @param ... additional plotting arguments to be passed on to ebar()
#' @export boot_cilm

boot_cilm<-function(x,y,data=NULL, reps=1000,range=par("usr")[1:2],steps=101,xout=NULL,quantiles=c(0.05,0.95),level=0.9, plot=T,...){
list()->out
##pull variables from data if possible
if(length(x)!=length(y)) stop("y and x not same length!")
#prepare xout
if(is.null(xout)) xout<-seq(range[1], range[2],diff(range)/steps)
quantiles<-range(quantiles)

#prepare list
out$preds<-data.frame(x=xout)
out$lm<-lm(y~x)

out$lms<-list()

##loop and sampe
for(i in 1:reps){
sample(c(1:length(x)),replace=T)->ind
x[ind]->x_
y[ind]->y_
dat<-data.frame(x=x_,y=y_)
lm(y~x, data=dat)->lm_

out$lms[[i]]<-lm_

predict(lm_, out$preds)->out$preds[,i+1]
}#end loop and sample
#print(out$preds)
#return(out$preds)
if(1==1){
##aggregate interval values
out$interval<-data.frame(x=xout)
predict(out$lm, out$interval)->out$interval$fit

apply(out$preds[,-1], 1, FUN=quantile, na.rm=T, probs=quantiles[1])->out$interval$lwr
apply(out$preds[,-1], 1, FUN=quantile, na.rm=T, probs=quantiles[2])->out$interval$upr

if(plot){
ebar(x=xout,lower=out$interval$lwr, upper=out$interval$upr, polygon=TRUE,...)
}

invisible(out)
}

}


##testing
#
#t<-data.frame(x=rnorm(100))
#t$y<-rnorm(100,sd=0.3,mean=t$x)
#plot(t)
#ci.lm(lm(y~x,t))
#predict(lm(y~x,t), data.frame(x=seq(-3,3,0.1)),interval="confidence")->t2
#ebar(x=seq(-3,3,0.1), upper=t2[,3], lower=t2[,2],polygon=T)
##boot_cilm(t$x, t$y,reps=500,col="red")->tp
##
