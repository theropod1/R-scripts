## function inverseQ
#' compute the inverse quantile for value x_i based on the distribution of x
#' @param x sample
#' @param x_i critical value for which to compute quantile. If a vector with more than one value is specified, the mean is used, but different values of x_i are resampled during the bootstrap to quantify uncertainty of the estimated quantile.
#' @param bootstrap Logical, default FALSE. Should bootstrap be run to estimate quantile uncertainty?
#' @param reps number of bootstrap reps
#' @param return_reps logical whether to return quantile result for each bootstrap repetition (TRUE) or just summary statistics (FALSE, default)
#' @param w weights for x_i (defaults to 1, i.e. equal or no weighting across entire sample of x). If different weights are desired, a vector of same length as x must be provided.
#' @param w_i weights for x_i (defaults to 1, i.e. equal or no weighting across entire sample of x_i). If different weights are desired, a vector of same length as x_i must be provided.
#' @param errorterms_x Optional: vector of error terms (standard deviations/randomly distributed standard errors) from which to resample a random error term to add to each x value in the bootstrap
#' @return a list() containing the weighted number of observations in x, estimated quantile for x_i and, if bootstrap==TRUE, the results of the bootstrap (optionally results for each rep, plus quantiles, mean and median for the quantile of x_i)
#' @details inverseQ is a simple function to determine at which quantile of a sample a specific value sits. The function counts all values of x that are smaller or equal to x_i, and divides by sample size. An optional weighting factor (w) can be specified, in which case values are not simply counted but the sum of the weighting factor computed as a measure of the number of cases instead. An optional bootstrap procedure to quantify confidence intervals is provided
#' @importFrom stats quantile
#' @importFrom stats weighted.mean
#' @export inverseQ

inverseQ<-function(x,x_i,w=1, w_i=1, bootstrap=FALSE, reps=1000, return_reps=FALSE,errorterms_x=0){
list()->out
rep(w,length(x))[1:length(x)]->w
rep(w_i,length(x_i))[1:length(x_i)]->w_i #make sure weighting factors are same length as x and x_i

sum(w)->out$n
sum(w[x<=weighted.mean(x_i,w=w_i)])/out$n->out$quantile

if(bootstrap){
iq<-function(x,w){
sample(c(1:length(x)),replace=TRUE,prob=w)->indices
x_<-x[indices]+rnorm(n=length(indices),mean=0,sd=errorterms_x[indices])
w_<-w[indices]
x_i_<-sample(1:length(x_i),size=1,prob=w_i,replace=TRUE)
x_i_<-x_i[x_i_]
sum(w_[x_<=x_i_])/sum(w_)->q
return(q)
}
replicate(reps,iq(x,w)) -> out$bootstrap_res #perform steps in iq reps times and save result

quantile(out$bootstrap_res,c(0.005,0.01,0.025,0.05,0.1,0.9,0.95,0.975,0.99,0.995))->out$bootstrap_CIs #compute confidence interval quantiles

median(out$bootstrap_res)->out$bootstrap_median
mean(out$bootstrap_res)->out$bootstrap_mean

if(!return_reps) out$bootstrap_res<-NULL
}

return(out)
}
