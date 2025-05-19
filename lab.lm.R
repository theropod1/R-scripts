##function: modelp() 
#' extract the p value for a linear model
#' @param x object of class lm to extract p value from
#' @param signif number of significant digits to which to round
#' @return a numeric containing the p value for the model
#' @importFrom stats pf
#' @export modelp
#' @examples
#' a<-rnorm(20)
#' b<-rnorm(n=20, mean=a, sd=0.1)
#' modelp(lm(b~a))

modelp<-function(x, signif=10){
summary(x)$fstatistic->f

if(!is.null(signif)){
signif(1-pf(f[1],f[2],f[3]),signif)->pval
#if(pval==0){return(paste0("<0.",paste(rep(0,signif-2), collapse=""),1))}else{return(pval)}
}else{
1-pf(f[1],f[2],f[3])->pval
}
#if(pf(f[1],f[2],f[3])==0){return(2.2e-16)}else{
return(pval)#}

}
##

 
##function lab.lm()
#' Plot a linear model with confidence or prediction interval
#'
#' @param lm Model object (output of lm())
#' @param at Positional value (as numeric containing x and y coordinates or character string specifying "topleft","topright","bottomleft" or "bottomright"). Defaults to topleft.
#' @param adj Text alignment as a numeric of length 2. Defaults to c(0,1) (left top aligned)
#' @param digits Number of significant digits to which to round parameters of model formula 
#' @param transformation Transformation used by model (can be "LOG" or "log" for natural logarithm, "LOG10" or "log10" for base-10-logarithm
#' @param spacing Line spacing to use between first and second line
#' @param pvalue Logical indicating whether to plot p value of model in second line. If FALSE (default), then multiple R-squared is shown.
#' @param RMSE Logical indicating whether to plot root mean square error of model in second line. If FALSE (default), then multiple R-squared is shown.
#' @param ... Other parameters to be passed on to text()
#' @return Adds the model formula and R-squared or p-value to the current plot in the specified location
#' @importFrom graphics text
#' @importFrom graphics par
#' @importFrom stats residuals
#' @export lab.lm
#' @examples
#' dd<-data.frame(x=c(1,2,3,4,5),y=c(2,3.3,4,4.7,8))
#' lm(y~x,data=dd)->m1
#' lm(log(y)~log(x),data=dd)->m2
#' plot(dd) 
#' #add log-transformed model:
#' ci.lm(m1,alpha=0.5,lty=1,col="red",lwd=2,limits=c(1.5,4.5),lt.border=2,border="red")
#add log-transformed model:
#' ci.lm(m2,alpha=0.5,transformation="log",lty=1,col="blue",lwd=2,limits=c(1.5,4.5),lt.border=2,border="blue")
#' lab.lm(m2,transformation="LOG",col="blue",at="bottomright",digits=3,spacing=1)
#' lab.lm(m1,col="red",at="topleft",digits=3,spacing=1.5)


lab.lm<-function(lm, at="topleft", adj=c(0,1), digits=4, transformation=NULL,spacing=1.5, pvalue=FALSE, RMSE=FALSE,...){
par("usr")->bb
diff(range(bb[1:2]))/50->xu
diff(range(bb[3:4]))/50->yu


if(is.numeric(at)){
if(length(at)>1){
xtxt<-at[1]
ytxt<-at[2]

}else if(length(at)==1){
xtxt<-bb[1]+xu
ytxt<-at
}

}else{

if(at=="topright"){
xtxt<-bb[2]-xu
ytxt<-bb[4]-yu
adj<-c(1,1)
}else if(at=="bottomright"){
xtxt<-bb[2]-xu
ytxt<-bb[3]+yu
adj<-c(1,0-spacing)
}else if(at=="bottomleft"){
xtxt<-bb[1]+xu
ytxt<-bb[3]+yu
adj<-c(0,0-spacing)
}else{
xtxt<-bb[1]+xu
ytxt<-bb[4]-yu
adj<-c(0,1)
}

}

if("(Intercept)" %in% names(lm$coefficients)) intercept<-TRUE
if(!("(Intercept)" %in% names(lm$coefficients))) intercept<-FALSE

if(is.null(transformation)){

if(length(coef(lm))==2){
bquote(y == .(signif(coef(lm)[1],digits)) + .(signif(coef(lm)[2],digits)) * x)->eq
}else{

if(length(coef(lm))==1) bquote(y == .(signif(coef(lm)[1],digits)) * x)->eq ##regression w/o intercept term
##XXX

if(length(coef(lm))>2){##run accross all coefficients if n>2

if(intercept) bquote(y == .(signif(coef(lm)[1],digits)) + .(signif(coef(lm)[2],digits)) * x[1])->eq
if(!intercept) bquote(y == .(signif(coef(lm)[1],digits)) * x[1])->eq

for(i in ifelse(intercept,3,2):length(coef(lm))){
ind<-i-1

bquote(.(eq)+.(signif(coef(lm)[i],digits)) * x[.(ind)])->eq
}
}
}

if(!intercept) bquote(R[pseudocentered]^2  == .(signif(1-sum(residuals(lm)^2)/sum((predict(lm)+residuals(lm)-mean(predict(lm)+residuals(lm)))^2),digits)))->rsq ##to force a pseudo-centered R², even if no intercept is included (still more useful for comparison than uncentered R²!)
if(intercept) bquote(R^2 == .(signif(summary(lm)$r.squared,digits)))->rsq

bquote(RMSE == .(sqrt(mean(residuals(lm)^2))))->rmse

}else if(transformation=="log" | transformation=="LOG"){
bquote(y == .(signif(exp(coef(lm)[1]),digits))*x^.(signif(coef(lm)[2],digits)))->eq

bquote(R[log-transformed]^2 == .(signif(summary(lm)$r.squared,digits)))->rsq

bquote(RMSE == .(sqrt(mean(residuals(lm)^2))))->rmse ##XXX TODO instead of log-residuals, use untransformed residuals to permit direct comparisons

}else if(transformation=="log10" | transformation=="LOG10"){
bquote(y == .(signif(10^(coef(lm)[1]),digits))*x^.(signif(coef(lm)[2],digits)))->eq

bquote(R[log-transformed]^2 == .(signif(summary(lm)$r.squared,digits)))->rsq

bquote(RMSE == .(sqrt(mean(residuals(lm)^2))))->rmse ##XXX TODO instead of log-residuals, use untransformed residuals to permit direct comparisons

}

if(exists("modelp")){
modelp(lm)->pval

if(is.na(pval)){
pval<-"NA"
}else if(pval==0){
pval<-"<2.2E-16"
}

if(is.numeric(pval)){
bquote(p == .(signif(pval,digits)))->mp}else{
bquote(p == .(pval))->mp
}

}

message("inserting label at x = ", xtxt, ", y = ",ytxt)

text(x=xtxt,y=ytxt, adj=adj, eq,...)

if(pvalue & exists("mp")){text(x=xtxt,y=ytxt, adj=c(adj[1],adj[2]+spacing), mp,...)}else{
if(!RMSE) text(x=xtxt,y=ytxt, adj=c(adj[1],adj[2]+spacing), rsq,...)##plot R²
if(RMSE) text(x=xtxt,y=ytxt, adj=c(adj[1],adj[2]+spacing), rmse,...)##plot root mean square error
}

}##


#' Calculate the percent prediction error of a model or two sets of data
#' @param yfitted Either a model of class "lm", or a numeric vector with fitted values to be compared to observed values
#' @param yobserved observed y values. If NULL, observed values are calculated based on fitted and residual values from supplied model
#' @param testingx predictor variables for testing data (if desired) as either a numeric vector or data.frame containing data for which to make predictions
#' @param predvars names of predictor variables, if not already present in testingx
#' @param verbose logical indicating whether to return mean PPE (if FALSE, default) or dataframe with all fitted and predicted values
#' @param transformation Transformation to use on yfitted and yobserved
#' @return The mean PPE or a data.frame() containing all fitted and observed values and their respective PPEs
#' @export PPE
#' @examples
#' set.seed(42)
#' rnorm(20,mean=10, sd=2)->x
#' rnorm(n=20,mean=x, sd=1)->y
#' lm(y~x)->m
#' plot(y~x)
#' ci.lm(m)
#' PPE(m)

PPE<-function(yfitted=NULL,yobserved=NULL, testingx=NULL,predvars=NULL,verbose=FALSE, transformation=NULL){
yfitted->yfit_backup

if(inherits(yfitted,"lm")){

if(is.null(testingx)){ #run on original data
yfitted$fitted.values->yfitted

}else{ #run on testing data

if(is.null(predvars)) names(yfitted$coefficients)->predvars
if("(Intercept)" %in% predvars) predvars<-predvars[which(predvars!="(Intercept)")]
if(is.data.frame(testingx)){
if(any(!(predvars%in%colnames(testingx)))) colnames(testingx)<-predvars[1:ncol(testingx)]
}else if(is.numeric(testingx)){
data.frame(x=testingx)->testingx
colnames(testingx)<-predvars
}else{stop("testingx must be NULL, dataframe or numeric")}

predict(yfitted, testingx)->yfitted

}

yfit_backup$fitted.values+yfit_backup$residuals->yobserved
}

#PPE = (Predicted Value - Actual Value) / Actual Value| * 100

if(!is.null(transformation)){
if(is.function(transformation)){
yfitted <- transformation(yfitted)
yobserved <- transformation(yobserved)
}}

(yfitted-yobserved)/yobserved*100 -> PPE
abs(PPE) -> PPE

if(verbose){return(data.frame(yfitted,yobserved,PPE))}else{
return(mean(PPE))}

}##
