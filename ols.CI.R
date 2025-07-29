##ols.CI function
#' Calculate parametric confidence and prediction interval formulas using ordinary least squares
#' @param x x values
#' @param y y values
#' @param x_ (optional) values at which confidence and prediction bands should be estimated
#' @param level desired confidence level (defaults to 0.9=90%)
#' @param digits (optional) desired significant digits to round to
#' @importFrom stats var
#' @importFrom stats residuals
#' @export ols.CI
#' @return either a data.frame with estimated upper and lower confidence and prediction bands (if x_!=NULL), or a list() object containing all the information necessary for the parametric construction of confidence intervals: the model formula, standard error of the estimate, critical t value at the chosen confidence level and degrees of freedom of the model, mean and variance of the independent variable, sample size and the complete formulas for the confidence and prediction intervals


ols.CI<-function(x,y=NULL,x_=NULL,level=0.9,digits=NULL){
if(inherits(x,"lm")){
y<-x$model[,1]
x<-x$model[,2]
}

lm(y~x)->model

t_crit<-qt(1-((1-level)/2),summary(model)$df[2]) # critical t value
see<-summary(model)$sigma #residual standard error (SEE)

mean(x)->xmean
var(x)->xvar
length(residuals(model))->n

if(!is.null(digits) & is.null(x_)){ #round figures to signif significant digits
t_crit<-signif(t_crit,digits)
see<-signif(see,digits)
xmean<-signif(xmean,digits)
xvar<-signif(xvar,digits)

}

if(!is.null(x_)){ ##return fitted values at x_
est_p<-coef(model)[1]+x_*coef(model)[2] # point estimates

etc_CI<-(1/n+(x_-xmean)^2/((n-1)*xvar))^0.5 #multiplier for CI
etc_PI<-(1+1/n+(x_-xmean)^2/((n-1)*xvar))^0.5 #multiplier for PI

est_p+t_crit*see*etc_CI->upr
est_p-t_crit*see*etc_CI->lwr

est_p+t_crit*see*etc_PI->uprPI
est_p-t_crit*see*etc_PI->lwrPI

return(data.frame(x_,y_=est_p,lwr,upr,lwrPI,uprPI))

}else{ ##return formulas and parameters

modf<-bquote(.(coef(model)[[1]])+x*.(coef(model)[[2]])) #model formula
if(!is.null(digits)) modf<-bquote(.(signif(coef(model)[[1]],digits))+x*.(signif(coef(model)[[2]],digits))) #model formula


add<-bquote(.(t_crit)%*%.(see)) #SEE and tcrit
CI<-bquote(sqrt(1/.(n)+(x-.(xmean))^2/((.(n)-1)*.(xvar)))) #multiplier for CI
PI<-bquote(sqrt(1+1/.(n)+(x-.(xmean))^2/((.(n)-1)*.(xvar)))) #multiplier for PI


out<-list(model=modf,t_critical=t_crit,SEE=see,x_mean=xmean, x_var=xvar, n=n,CI=bquote(y_%+-%.(add)*.(CI)),PI=bquote(y_%+-%.(add)*.(PI)))

return(out)
}

}



#' output the formula for the parametric confidence and prediction intervals of a linear model
#'
#' @param y independent variable, can be a model object
#' @param x 
#' @param interval "both"
#' @param cl confidence level, default 0.9
#' @return a character vector containing the formulas for the confidence and prediction intervals
#' @export cipi_form

cipi_form<-function(x,y=NULL,interval="both", cl=0.9,transformation="n"){
if(inherits(x,"lm")){
y<-x$model[,1]
x<-x$model[,2]
}

lm(y~x)->model
##CI and PI
est_p<-paste(coef(model)[1],"+ x *",coef(model)[2]) # point estimate
if(transformation=="log") est_p<-paste(coef(model)[1],"+ log(x) *",coef(model)[2]) # point estimate


t_crit_upr<-qt(1-(1-cl)/2,summary(model)$df[2]) # critical t value

see<-summary(model)$sigma #residual standard error (SEE)

mean(x)->xmean
var(x)->xvar
length(resid(model))->n

if(transformation!="log"){
etc_CI<-paste0("(1/",n,"+(x-",xmean,")^2/((",n,"-1)*",xvar,"))^0.5")
etc_PI<-paste0("(1+1/",n,"+(x-",xmean,")^2/((",n,"-1)*",xvar,"))^0.5")
}else{
etc_CI<-paste0("(1/",n,"+(log(x)-",xmean,")^2/((",n,"-1)*",xvar,"))^0.5")
etc_PI<-paste0("(1+1/",n,"+(log(x)-",xmean,")^2/((",n,"-1)*",xvar,"))^0.5")
}




paste0(est_p," ± ",t_crit_upr," * ", see, " * ", etc_CI)->CI
paste0(est_p," ± ",t_crit_upr," * ", see, " * ", etc_PI)->PI

if(transformation=="log"){
paste("exp(",CI,")")->CI
paste("exp(",PI,")")->PI
}
c(CI,PI)->out
names(out)<-c(paste0("CI",cl),paste0("PI",cl))
return(out)

}

