##README: various functions related to the estimation of non-parametric (e.g. bootrstap) or parametric confidence intervals or uncertainty measures

##function:
#simple boostrapping for the result of any function fun on vector x. If CI==F, all results are returned, otherwise output a confidence interval at the specified level.
bootCI<-function(x,fun,level=0.9,reps=1000, CI=TRUE){
upr<-1-(1-level)/2
lwr<-0+(1-level)/2

x[which(!is.na(x))]->x

replicate(reps,fun(sample(x,length(x),repl=T)))->x_

if(CI==FALSE){return(x_)
}else{quantile(x_,c(lwr,upr))->ci
return(ci)}
}
##

##function:
#"pseudobootstrap", randomly samples n entries reps times and returns the results of fun, or the quantiles for a confidence interval at the given level for them. Useful for demonstrating the results of small samples on conclusions or to artificially "downsample" larger sample for comparative purposes.
p_bootCI<-function(x,fun,level=0.9, n=10,reps=1000, CI=TRUE){
upr<-1-(1-level)/2
lwr<-0+(1-level)/2
x[which(!is.na(x))]->x

replicate(reps,fun(sample(x,n,repl=T)))->x_

if(CI==FALSE){return(x_)
}else{quantile(x_,c(lwr,upr))->ci
return(ci)}
}
##

##function:
#simple jackknife
jackCI<-function(x,fun,level=0.9, CI=TRUE){
upr<-1-(1-level)/2
lwr<-0+(1-level)/2
x[which(!is.na(x))]->x

length(x)->reps
x_<-numeric(reps)
for(i in 1:reps){
fun(x[-i])->x_[i]}

quantile(x_,c(lwr,upr))->ci
if(CI==FALSE){return(x_)
}else{return(ci)}
}
##

##function:
#confidence interval for the variance of a sample x at a given confidence level
varCI<-function(x, level=0.9){
n<-length(x)
v<-var(x)
upr<-1-(1-level)/2
lwr<-0+(1-level)/2

varCI<-(n-1)*v/qchisq(c(upr,lwr), n-1)
vars<-c(v,varCI)
names(vars)<-c("variance",paste0("p_",lwr),paste0("p_",upr))
return(vars)
}
##

##function:
#sum of variances for columns of a data-frame or matrix x.
varsum<-function(x,n=ncol(x)){
vars<-numeric(ncol(x))
for(i in 1:ncol(x)){var(x[,i])->vars[i]}
return(sum(vars))
}
##

##function distr.sample()
#' resample datapoints based on a distribution given by previously known data
#' param n number of points to sample
#' param data data to inform the density distribution
#' param n.density number of points for density to return and to then sample from
#' param ... additional arguments to pass on to density()
#' return a numeric vector with n points drawn from the simulated population based on the input data
#' details note that the x values output by density() are used as the sample to be drawn from, so the precision depends on the setting for n in density(), a higher n will result in a greater number of discrete points to sample from, approximating a continuous distribution

distr.sample<-function(n,data,n.density=2000,...){
density(data, n=n.density,...)->ddat

sample(x=ddat$x, size=n, replace=T, prob=ddat$y)

}
##
