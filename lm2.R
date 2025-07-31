##lm2()
#' Function for fitting linear model II regressions
#'
#' @param formula model formula
#' @param data data for model
#' @param v verbosity setting (logical)
#' @param method method for fitting model, defaults to Impartial Least Squares or Covariance-Matrix-based Model II Regression sensu Tofallis 2023, alternative method "PCA" uses Principal Component–based Reduced Major Axis Regression that uses prcomp() to find slopes (for the bivariate case, both are identical and the slope is simply sign(cor/)
#' @references Tofallis, C. 2023. Fitting an Equation to Data Impartially. Mathematics: 11:3957. https://doi.org/10.3390/math11183957.
#' https://pjbartlein.github.io/GeogDataAnalysis/lec16.html
#' @return a list() type object with the class "lm2" and containing the model parameters, fitted values, dataset, call and formula⎄
#' @export predict.lm2
#' @examples
#' x<-rnorm(10)
#' y<-rnorm(10,mean=x,sd=0.2)
#' lm2(y~x,data=data.frame(x,y))->rmcf

lm2 <- function(formula, data,v=FALSE, method="tofallis") {
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  if(v) print(mf)
  if(v) print(X)
  X <- X[, -1, drop = FALSE]  # remove intercept column
  if(v) print(x)
  # Standard deviations for back-transform
  sy <- sd(y)
  sx <- apply(X, 2, sd)
  
  if(method%in%c("PCA","pca","prcomp","princomp","PC","PC1")){
  # scale variables for PCA
  mat_std <- scale(cbind(y, X))
  
  # run PCA
  pc <- prcomp(mat_std, scale. = FALSE)
  loadings <- pc$rotation[, 1] #extract PC1 loadings
  
  # Slopes in standardized space
  slopes_std <- loadings[-1] / loadings[1]
  
  # Back-transform to original units
  slopes <- slopes_std * (sy / sx)
  names(slopes) <- colnames(X)
  
  # Compute intercept
  yhat_no_int <- as.matrix(X) %*% slopes
  intercept <- mean(y) - mean(yhat_no_int)
  
  # Predictions and residuals
  fitted <- intercept + yhat_no_int
  }else{##WIP
  #Tofallis, C. 2023. Fitting an Equation to Data Impartially. Mathematics: 11:3957. https://doi.org/10.3390/math11183957.
  cbind(y,mf)->mf_
  cov(mf)->cov_mf_
  sqrt(diag(solve(cov_mf_)))->slopes
  slopes[-1]/slopes[1]->slopes
  
  yhat_no_int <- as.matrix(X) %*% slopes
  intercept <- mean(y) - mean(yhat_no_int)
  
  fitted <- intercept + yhat_no_int
  }
  
  residuals <- y - fitted
  
  fit <- list(
    coefficients = c("(Intercept)" = intercept, slopes),
    residuals = residuals,
    fitted.values = fitted,
    call = match.call(),
    terms = terms(mf),
    model = mf,
    xlevels = .getXlevels(terms(mf), mf),
    method = method,
    formula = formula
  )
  
if(method%in%c("PCA","pca","prcomp","princomp","PC","PC1")){
fit$PCA<-pc
fit$loadings<-loadings
fit$slopes_std<-slopes_std
}else{
cov_mf_->fit$cov_matrix
}
  
  class(fit) <- c("lm2", "lm")
  return(fit)
}##


##predict.lm2
#' Predict method for lm2 objects output by lm2()
#'
#' @param model model object of class lm2, fitted using the lm2() function
#' @param newdata New data for which to make predictions (if NULL, the training data of the model are used). transformations are not applied automatically
#' @param level 0.9 confidence level
#' @param reps 1000 repetitions
#' @param include.mean whether to include mean (main model) as first model in the bootstrap process (useful for comparisons)
#' @param make.predictions
#' @param ... other arguments to pass on to lm2
#' @return a list() object containing bootstrapped models, coefficients, residuals, and predicted values for newdata, as well as confidence intervals for newdata and the model coefficients, and prediction intervals for newdata
#' @export predict.lm2
#' @method predict lm2
#' @importFrom stats predict
#' @examples
#' x<-rnorm(10)
#' y<-rnorm(10,mean=x,sd=0.2)
#' lm2(y~x,data=data.frame(x,y))->rmcf
#' predict.lm2(rmcf)->co
#' plot(rmcf$model[,c(2,1)])
#' ebar(lower=co$CI0.9[1,],upper=co$CI0.9[2,],x=co$newdata[,1],polygon=TRUE)
#' ebar(lower=co$PI0.9[1,],upper=co$PI0.9[2,],x=co$newdata[,1],polygon=TRUE)
#' abline(rmcf)

predict.lm2<-function(model, newdata=NULL, level=0.9, reps=1000, include.mean=FALSE, make.predictions=TRUE,...){

model$model->model_vars
model$formula->model_formula
all.vars(model_formula)->colnames(model_vars)
if(is.null(newdata)) reformulate(all.vars(model_formula)[-1], response = all.vars(model_formula)[1])->model_formula #XXX

if(is.null(newdata)) newdata<-model$model

matrix(NA,nrow=reps, ncol=ncol(model_vars))->coefficients #determine number of variables/coefficients and number of reps
colnames(coefficients)<-c("(intercept)",colnames(model$model)[-1])
randres<-numeric()
rm.tmp<-list()
for(i in 1:reps){
sample(c(1:nrow(model_vars)), replace=TRUE)->indices
#now fit lm2 on model_vars[indices] and save coefficients to matrix
lm2(model$formula,data=model_vars[indices,],...)->rm.tmp[[i]]
coef(rm.tmp[[i]])->coefficients[i,]
sample(rm.tmp[[i]]$residuals,1)->randres[i]
}
if(include.mean){
lm2(model$formula,data=model_vars,...)->rm.tmp[[1]] #optional: replace first value with the mean model for easier comparisons
coef(rm.tmp[[1]])->coefficients[1,]
sample(rm.tmp[[1]]$residuals,1)->randres[1]
}
numeric()->fitted
matrix(ncol=nrow(newdata), nrow=reps)->fittedCI
matrix(ncol=nrow(newdata), nrow=reps)->fittedPI

as.data.frame(newdata[,colnames(coefficients)[-1]])->newdata
colnames(newdata)<-colnames(coefficients)[-1]

if(make.predictions){
for(rep in 1:reps){
for(i in 1:nrow(newdata)){
lm2(model_formula,data=model_vars,...)->rm.tmp
fitted[i]<-coef(rm.tmp)[1]+sum(newdata[i,]*coef(rm.tmp)[-1])
fittedCI[rep,i]<-coefficients[rep,1]+sum(newdata[i,]*coefficients[rep,-1])
fittedPI[rep,i]<-coefficients[rep,1]+sum(newdata[i,]*coefficients[rep,-1])+randres[rep]
}}}

list(fit=fitted,fitted_values_mean=fittedCI, fitted_values_observation=fittedCI+randres,coefficients=coefficients,random_residual=randres, models=rm.tmp,newdata=newdata)->out

ci<-c((1-level)/2,level+(1-level)/2)
apply(X=out$fitted_values_mean,MAR=2,FUN=quantile,probs=ci)->out[[paste0("CI",level)]]
apply(X=out$fitted_values_observation,MAR=2,FUN=quantile,probs=ci)->out[[paste0("PI",level)]]

out[[paste0("Coefficient_CIs_",level)]]<-apply(X=out$coefficients,MAR=2,FUN=quantile,probs=ci)

return(out)
}##


##summary.lm2
#' Summary method for lm2 objects output by lm2()
#'
#' @param model lm2-class model
#' @export summary.lm2
#' @method summary lm2
#' @importFrom base summary
#' @examples
#' 


summary.lm2<-function(model){
list()->out
out$call<-model$call
out$coefficients<-model$coefficients
1-var(model$residuals)/var(model$model[,1])->out$r.squared
out$residuals<-model$residuals
out$fitted.values<-model$fitted.values
class(out)<-c("summary_lm2")
return(out)
}##

##print.summary_lm2
#' Print method for summary_lm2 objects output by summary.lm2()
#'
#' @param model_summary summary_lm2-class model
#' @export print.summary_lm2
#' @method print summary_lm2
#' @importFrom base summary
#' @examples
print.summary_lm2<-function(model_summary){
print(model_summary$call)
cat("\nCoefficients:\n")
print(model_summary$coefficients)
cat("\nmodel_summary$r.squared =",model_summary$r.squared,"\n")
cat("\nAll contents:\n")
print(paste0(substitute(model_summary),"$",names(model_summary)))
}
