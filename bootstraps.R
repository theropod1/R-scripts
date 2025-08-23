##README: functions related to the estimation of non-parametric (e.g. bootstrap) confidence intervals

##function: bootCI() and p_bootCI() ##migrate to boot_cilm



##function: jackCI()
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


##function distr.sample()
#' resample n datapoints from a distribution as defined by the density curve of a known sample
#' @param n number of points to sample
#' @param data data to inform the density distribution
#' @param n.density number of points for density to return and to then sample from (defaults to 10000)
#' @param ... additional arguments to pass on to density()
#' @return a numeric vector with n points drawn from the simulated population based on the input data
#' @details note that the x values output by density() are used as the sample to be drawn from, so the precision depends on the setting for n in density(), a higher n will result in a greater number of discrete points to sample from, approximating a continuous distribution
#' @export distr.sample

distr.sample<-function(n,data,n.density=10000,...){
density(data, n=n.density,...)->ddat

sample(x=ddat$x, size=n, replace=T, prob=ddat$y)

}
##

##function pAgreaterB()
#' non-parametric probability estimate for comparison between two samples
#' @param A sample of observation for which to test if the observed parameter is greater than in sample B
#' @param B the sample to use for comparison
#' @param reps number of resampling points and repetitions
#' @param ... additional arguments to pass on to distr.sample() and density()
#' @return the probability of the observation for sample A being larger than sample B
#' @details for example A and B can be probability distributions of two estimates, and the desired p value is the probability
#' @export pAgreaterB

pAgreaterB<-function(A,B,reps=1000,...){

distr.sample(n=reps, data=A,...)->A
distr.sample(n=reps, data=B,...)->B
counter<-0

for(i in 1:reps){
sample(x=A, size=1,replace=T)->A_
sample(x=B, size=1,replace=T)->B_
if(A_>B_) counter<-counter+1

#if(mean(A_)>mean(B_)) counter<-counter+1

}

message("probability that parameter value for group A is higher than value for group B:")
return(counter/reps)

}

##example
#pnorm(1/sqrt(1^2+1^2)) #probability for the distance between two normal distributions with a difference in means of 1 assuming perfect normality
#A<-rnorm(1000, mean=1, sd=1)
#B<-rnorm(1000, mean=0, sd=1)
#pAgreaterB(A=A,B=B,reps=10000)
##should approximate pnorm(1/sqrt(1^2+1^2)) at high n
#application: e.g. hypothesized estimate probability distributions; use estimates as input for constructing probability densities, resample from those densities, then compare to determine probability of A>B (or vice versa)
