##functions for fitting and working with type II regression models

##lm2()
#' Function for fitting linear model II regressions
#'
#' @param formula model formula
#' @param data data for model
#' @param v verbosity setting (logical)
#' @param method method for fitting model, defaults to Impartial Least Squares or Covariance-Matrix-based Model II Regression sensu Tofallis 2023, alternative method "PCA" uses Principal Component–based Reduced Major Axis Regression that uses prcomp() to find slopes (for the bivariate case, both are identical and the slope is simply \code{sign(cor())*sd(y)/sd(x)}
#' @param w (optional) vector of weights, if model should be weighed (defaults to 1, i.e. equal weighting. Currently only works for bivariate model.
#' @return An object of class \code{"lm2"} containing:
#' \itemize{
#'   \item \code{coefficients} — Model coefficients (intercept and slopes).
#'   \item \code{residuals} — Residuals.
#'   \item \code{fitted.values} — Fitted values.
#'   \item \code{model} — Model frame.
#'   \item \code{method} — Method used.
#'   \item \code{formula} — Original model formula.
#'   \item \code{transformations} — Transformation functions applied to predictors.
#' }
#'
#' @references
#' Tofallis, C. (2023). Fitting an Equation to Data Impartially.
#' \emph{Mathematics}, 11(18), 3957. \doi{10.3390/math11183957}
#'
#' See also: \url{https://pjbartlein.github.io/GeogDataAnalysis/lec16.html}
#'
#' @export lm2
#' @importFrom weights wtd.cor
#' @examples
#' x<-rnorm(10)
#' y<-rnorm(10,mean=x,sd=0.2)
#' lm2(y~x,data=data.frame(x,y))->rmcf

lm2 <- function(formula, data,v=FALSE, method="tofallis",w=1) {
  
  if(length(w)!=nrow(data)) rep(w,nrow(data))[1:nrow(data)]->w
  
  mf <- model.frame(formula, data)
  w<-w[complete.cases(mf)]
  mf<-mf[complete.cases(mf),]
  
  y <- model.response(mf)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  if(v) print(mf)
  if(v) print(X)
  X <- X[, -1, drop = FALSE]  # remove intercept column
  if(v) print(X)
  # Standard deviations for back-transform
  sy <- sd(y)
  sx <- apply(X, 2, sd)
    if(v) cat(sy,sx)

  if(ncol(X)==1){ #bivariate case
  sy <- wsd(y,w=w)
  sx <- wsd(as.numeric(X),w=w)
  weights::wtd.cor(as.numeric(X),y,weight=w)[1]->r_pearson
  slopes<-sy/sx*sign(r_pearson)
  names(slopes) <- colnames(X)
  
  intercept<-weighted.mean(y,w=w)-slopes*weighted.mean(as.numeric(X),w=w)
  names(intercept)<-NULL
  fitted <- intercept+slopes*as.numeric(X)
  
  method<-"bivariate RMA"
  }else{
  
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
  }else{
  #Tofallis, C. 2023. Fitting an Equation to Data Impartially. Mathematics: 11:3957. https://doi.org/10.3390/math11183957.
  cbind(y,mf)->mf_
  cov(mf)->cov_mf_
  sqrt(diag(solve(cov_mf_)))->slopes
  slopes[-1]/slopes[1]->slopes
  
  yhat_no_int <- as.matrix(X) %*% slopes
  intercept <- mean(y) - mean(yhat_no_int)
  
  fitted <- intercept + yhat_no_int
  }
  }#end fitting stages
  
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
    formula = formula,
    weights = w
  )

  if(ncol(X)>1){#additional data for multivariate case
if(method%in%c("PCA","pca","prcomp","princomp","PC","PC1")){
fit$PCA<-pc
fit$loadings<-loadings
fit$slopes_std<-slopes_std
}else{
cov_mf_->fit$cov_matrix
}
}
  
  class(fit) <- c("lm2", "lm")
  return(fit)
}##


##predict.lm2
#' Predict method for lm2 objects output by lm2()
#'
#' @param model model object of class lm2, fitted using the lm2() function
#' @param newdata New data for which to make predictions (if NULL, the training data of the model are used). Transformations are not applied automatically unless autotransform==TRUE
#' @param autotransform logical indicating whether to automatically transform the predictors according to the transformations used in the model formula
#' @param retransform optional, function to apply to results of fitted values and confidence intervals. Defaults to identity function (unchanged).
#' @param level 0.9 confidence level
#' @param reps 1000 repetitions
#' @param bootstrap should predictions be bootstrapped to build confidence and prediction intervals (skip to make function run faster)
#' @param ... other arguments to pass on to lm2
#' @return a list() object containing the original model and predicted values for newdata, as well as confidence intervals for newdata and (if bootstrap==TRUE) the bootstrapped model coefficients, residuals and confidence and prediction intervals for newdata
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

predict.lm2<-function(model, newdata=NULL, autotransform=TRUE, retransform=identity, bootstrap=TRUE, level=0.9, reps=1000, v=FALSE,...){
#preparatory steps
model$model->transformed_vars
model$formula->model_formula
reformulate(all.vars(model_formula)[-1], response = all.vars(model_formula)[1])->model_formula_
if(v) print(model_formula)
if(v) print(model_formula_)

all.vars(model_formula)->raw_vars
term_labels <- attr(model$terms, "term.labels")

model_transformations <- setNames( #extract transformations applied to variables from model object
  lapply(term_labels, function(lbl) {
    expr <- parse(text = lbl)[[1]]
    if (is.call(expr)) {
      get(as.character(expr[[1]]), mode = "function", inherits = TRUE)
    } else {
      identity
    }
  }),
  term_labels
)

##adjust newdata transformations and names
if(!is.null(newdata)){#transform newdata
if(!autotransform){
as.data.frame(newdata[,names(model$coefficients)[-1]])->newdata
}
if(autotransform){
as.data.frame(newdata[,raw_vars[-1]])->newdata
for(i in 1:ncol(newdata)){newdata[,i]<-model_transformations[[i]](newdata[,i])}#apply transformation functions
}

}else{
newdata<-as.data.frame(transformed_vars[,-1])
}
colnames(newdata)<-names(model$coefficients)[-1]
if(v) print(head(newdata))

##predict best-fit values
fit<-numeric()
for(i in 1:nrow(newdata)) fit[i]<-model$coefficients[1]+sum(model$coefficients[-1]*newdata[i,])

##bootstrap for confidence and prediction intervals
if(bootstrap){
boot_coefs<-matrix(NA,nrow=reps, ncol=length(names(model$coefficients)))
colnames(boot_coefs)<-c("(Intercept)",names(model$coefficients)[-1])
randres<-numeric()
boot_models<-list()

fittedCI<-matrix(NA,nrow=reps, ncol=nrow(newdata))
fittedPI<-fittedCI

for(i in 1:reps){#bootstrap repetitions
if(v & i/50>0 & i%%50==0) cat("fitting and predicting for ", i, " in ", reps,"\n")
sample(c(1:nrow(transformed_vars)), replace=TRUE)->indices
transformed_vars[indices,]->training_input
colnames(training_input)<-raw_vars
if("weights" %in% names(model)){
w<-model$weights[indices]
}else{w<-1}
#resampled model fitting
lm2(model_formula_,data=training_input,w=w,...)->boot_models[[i]]
boot_models[[i]]$indices<-indices
boot_models[[i]]$coefficients->boot_coefs[i,]
sample(boot_models[[i]]$residuals,1)->randres[i]

for(j in 1:nrow(newdata)){#make predictions from bootstrapped model
fittedCI[i,j]<-boot_coefs[i,1]+sum(newdata[j,]*boot_coefs[i,-1])
fittedPI[i,j]<-boot_coefs[i,1]+sum(newdata[j,]*boot_coefs[i,-1])+randres[i]
}
}

#construct confidence intervals:
ci<-c((1-level)/2,level+(1-level)/2)
apply(X=fittedCI,MAR=2,FUN=quantile,probs=ci)->CI
apply(X=fittedPI,MAR=2,FUN=quantile,probs=ci)->PI

fit<-data.frame(fit=fit,lwr_CI=CI[1,],upr_CI=CI[2,],lwr_PI=PI[1,],upr_PI=PI[2,])
retransform(fit)->fit
}

list(original_model=model,newdata=newdata,model_transformations=model_transformations)->out
if(bootstrap){
out$boot_models<-boot_models
out$boot_coefficients<-boot_coefs
out$random_residual<-randres
out$fittedCI<-fittedCI
out$fittedPI<-fittedPI
}
out$fit<-fit
class(out)<-c("preds_lm2")

return(out)
}##


##numericify()
#' make all columns of a data.frame that can be coerced to numeric numeric
#' @param df an input data.frame
#' @return a data.frame with columns coerced to numeric wherever possible
#' @export numericify
#' @examples
#' 
numericify <- function(df) {
  df[] <- lapply(df, function(col) {
    if (suppressWarnings(!any(is.na(as.numeric(as.character(col)))))) {
      as.numeric(as.character(col))} else { col } } )
  df }


##function confint.preds_lm2()
#' Estimate parameter confidence intervals for preds_lm2 objects
#' @param preds_lm2 an object of class preds_lm2, output of predict.lm2 with bootstrap=TRUE
#' @param level desired confidence level
#' @param p_null null hypothesis for two-sided test to determine parameter p values
#' @return a list() object containing the call, confidence level, parameter confidence estimates, parameter p values, parameter standard deviations and number of bootstrap repetitions
#' @export confint.preds_lm2
#' @method confint lm2
#' @examples
#' 
confint.preds_lm2<-function(preds_lm2, level=0.9, p_null=0){
if("boot_coefficients"%in%names(preds_lm2)){
c((1-level)/2,level+(1-level)/2)->ci
nrow(preds_lm2$boot_coefficients)->n_reps_bootstrap

apply(X=preds_lm2$boot_coefficients,2,FUN=quantile, probs=ci)->parameter_CI

p_values<-apply(preds_lm2$boot_coefficients,2,FUN=function(x) sum(x<=p_null)/n_reps_bootstrap)
p_values<-ifelse(p_values>0.5,(1-p_values)*2,p_values*2)
p_values<-ifelse(p_values<1/n_reps_bootstrap,paste0("<",1/n_reps_bootstrap),p_values)
numericify(as.data.frame(t(data.frame(p_values))))->p_values

apply(X=preds_lm2$boot_coefficients,2,FUN=sd)->parameter_SD
out<-list(call=match.call(),level=level, CIs=parameter_CI,SEs=parameter_SD,p=p_values,p_null=p_null,n_reps=n_reps_bootstrap)

}else{stop("You need to run predict.lm2() with bootstrap=TRUE!")}

return(out)
}

## function wsd()
#' compute the weighted sample standard deviation or variance
#' @param x vector of values for which to compute weighted parameter
#' @param w vector of weights
#' @param na.rm default TRUE
#' @export wsd
#' @return the weighted standard deviation (or variance, or mean)
wsd <- function(x, w=rep(1,length(x)), na.rm=TRUE) {
if(na.rm){which(!is.na(x) & !is.na(w))->indices
x[indices]->x
w[indices]->w}
	wm <- sum(w * x) / sum(w) #wtd.mean
	variance <- sum(w * (x - wm)^2) / (sum(w) - sum(w^2) / sum(w)) #wtd.var
	sqrt(variance) #wtd.st
}

## function rsq()
#' compute the weighted r.squared of a model
#' @param model model for which r.squared should be calculated
#' @param weighted logical indicating whether weights should be taken into account, if available in model$weights
#' @export wsd
#' @return the (weighted) r.squared of the model
rsq<-function(model,weighted=TRUE){
if("weights"%in%names(model) & weighted){
1-wsd(model$residuals,model$weights)^2/wsd(model$model[,1],model$weights)^2
}else{
1-wsd(model$residuals)^2/wsd(model$model[,1])^2
}
}##

##summary.lm2
#' Summary method for lm2 objects output by lm2()
#'
#' @param model lm2-class model
#' @export summary.lm2
#' @method summary lm2
#' @examples
#' 

summary.lm2<-function(model){
list()->out
out$call<-model$call
out$coefficients<-model$coefficients
rsq(model)->out$r.squared

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
#' @examples
print.summary_lm2<-function(model_summary){
print(model_summary$call)
cat("\nCoefficients:\n")
print(model_summary$coefficients)
cat("\nmodel_summary$r.squared =",model_summary$r.squared,"\n")

cat("\nAll contents:\n")
print(paste0(substitute(model_summary),"$",names(model_summary)))
}


