##functions for fitting and working with type II regression models

##lm2()
#' Function for fitting linear model II regressions
#'
#' @param formula model formula (to suppress/set to 0 intercept in bivariate model, add +0 to the predictor variable)
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

lm2 <- function(formula, data=NULL, v=FALSE, method="tofallis",w=1) {
  if(!is.null(data)){mf <- model.frame(formula, data, na.action=na.pass)
  }else{
  mf <- model.frame(formula, na.action=na.pass)
  }
if(v) print(mf) #for troubleshooting
  
  has_intercept <- attr(terms(mf), "intercept") == 1
	if(v)  message("Has intercept?", has_intercept)
  
  if(length(w)!=nrow(mf)) rep(w,nrow(mf))[1:nrow(mf)]->w
  if(any(is.na(w))) w[which(is.na(w))]<-0
 
	w<-w[complete.cases(mf)]
	mf<-mf[complete.cases(mf),]
  #  print(mf)
  y <- model.response(mf)
  X <- model.matrix(attr(mf, "terms"), data = mf)
  if(v) print(mf)
  if(v) print(X)
if(has_intercept)  X <- X[, -1, drop = FALSE]  # remove intercept column
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
  if(!has_intercept) slopes<-sign(r_pearson) * sqrt(sum(w * y^2) / sum(w * as.numeric(X)^2))
  names(slopes) <- colnames(X)

	if(has_intercept) intercept<-weighted.mean(y,w=w)-slopes*weighted.mean(as.numeric(X),w=w)
	
	if(!has_intercept) intercept<-0

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
#' @param sample.weights whether to resample the training data using weights specified in the model (i.e. resampling with unequal probabilities, more highly weighted points are samples more commonly), default FALSE
#' @param v verbosity setting
#' @param fit.fun model fitting function, defaults to lm2. If specifying another model fitting function and calling the method explicitly, e.g. using \code{predict.lm2(lm(y~x)...))}, predict.lm2 can also be used as a general bootstrap function for various models given that they contain an intercept term and at least one slope. This functionality hasn’t been thoroughly tested, however!
#' @param smearing.corr logical indicating whether Duan’s correction ("smearing factor") should be applied (only when regression uses transformed values).
#' @param isometry.Null For cases where the bootstrap returns a dataset with a single effective observation, should isometry be used for the prediction (defaults to FALSE, which replaces these cases with NA).
#' @param ... other arguments to pass on to lm2 (or other fitting function, if overridden)
#' @return if bootstrap==TRUE, a list() object containing the original model and predicted values for newdata, as well as confidence intervals for newdata and the bootstrapped model coefficients, residuals and confidence and prediction intervals for newdata. If bootstrap==FALSE, a numeric vector of fitted values for newdata.
#' @export predict.lm2
#' @method predict lm2
#' @importFrom stats predict weighted.mean
#' @examples
#' x<-rnorm(10)
#' y<-rnorm(10,mean=x,sd=0.2)
#' lm2(y~x,data=data.frame(x,y))->rmcf
#' predict.lm2(rmcf)->co
#' plot(rmcf$model[,c(2,1)])
#' ebar(lower=co$CI0.9[1,],upper=co$CI0.9[2,],x=co$newdata[,1],polygon=TRUE)
#' ebar(lower=co$PI0.9[1,],upper=co$PI0.9[2,],x=co$newdata[,1],polygon=TRUE)
#' abline(rmcf)

predict.lm2<-function(model, newdata=NULL, autotransform=TRUE, retransform=identity, bootstrap=FALSE, level=0.9, reps=1000, sample.weights=FALSE, v=FALSE, fit.fun=lm2, smearing.corr=FALSE,isometry.Null=FALSE,...){
#preparatory steps
match.call()->call
model$model->transformed_vars
if("formula"%in%names(model)){ model$formula->model_formula }else if("formula"%in%names(model$call)) { model$call$formula->model_formula }

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

smearing_factor_main<-1
if(smearing.corr & "weights"%in%names(model)) smearing_factor_main<-weighted.mean(retransform(model$residuals),model$weights)
if(smearing.corr & !("weights"%in%names(model))) smearing_factor_main<-mean(retransform(model$residuals))

model_<-model

if(!any(names(model$coefficients) == "(Intercept)")){ model_$coefficients<-c("(Intercept)"=0, model$coefficients) #make sure first model parameter is intercept, even if it is not returned by default
}else if(which(names(model$coefficients) == "(Intercept)")!=1){ model_$coefficients<-c("(Intercept)"=model$coefficients[which(names(model$coefficients) == "(Intercept)")], model$coefficients[-which(names(model$coefficients) == "(Intercept)")]) #make sure first model parameter is the intercept, even if it is not returned as the first parameter
}

for(i in 1:nrow(newdata)) fit[i]<-model_$coefficients[1]+sum(model_$coefficients[-1]*newdata[i,])

##bootstrap for confidence and prediction intervals
if(bootstrap){
boot_coefs<-matrix(NA,nrow=reps, ncol=length(names(model$coefficients)))
colnames(boot_coefs)<-c("(Intercept)",names(model$coefficients)[-1])
randres<-numeric()
boot_models<-list()

fittedCI<-matrix(NA,nrow=reps, ncol=nrow(newdata))
fittedPI<-fittedCI
smearing_factors<-rep(1,reps)

some_identical<-FALSE
##bootstrap reps
for(i in 1:reps){
if(v & i/50>0 & i%%50==0) cat("fitting and predicting for ", i, " in ", reps,"\n")

if(!sample.weights) sample(c(1:nrow(transformed_vars)), replace=TRUE)->indices
if(sample.weights & "weights" %in% names(model)) sample(c(1:nrow(transformed_vars)), replace=TRUE,prob=model$weights)->indices

transformed_vars[indices,]->training_input
colnames(training_input)<-raw_vars

if("weights" %in% names(model)){
w<-model$weights[indices]
}else{w<-rep(1,nrow(model$model))}
#print(knitr::kable(training_input))
#resampled model fitting
if(!all(apply(training_input,2,FUN=function(x){length(unique(x))})==1)){#safeguard for samplings with all values being identical
fit.fun(model_formula_,data=training_input,w=w,...)->bm
bm->boot_models[[i]] #save original model fitt

#modify temporary model object to reflect presence of an intercept, if not present
if(!any(names(bm$coefficients) == "(Intercept)")){ bm$coefficients<-c("(Intercept)"=0, model$coefficients) #make sure first model parameter is intercept, even if it is not returned by default
}else if(which(names(bm$coefficients) == "(Intercept)")!=1){ bm$coefficients<-c("(Intercept)"=bm$coefficients[which(names(bm$coefficients) == "(Intercept)")], bm$coefficients[-which(names(model$coefficients) == "(Intercept)")]) #make sure first model parameter is the intercept, even if it is not returned as the first parameter
} 

boot_models[[i]]$indices<-indices
bm$coefficients->boot_coefs[i,] #save coefficients used, including added intercept term of 0, as needed
sample(boot_models[[i]]$residuals,1,prob=w)->randres[i]

#calculate smearing factor
if(smearing.corr & exists("w"))  smearing_factors[i]<-weighted.mean(retransform(boot_models[[i]]$residuals),w)

for(j in 1:nrow(newdata)){#make predictions from bootstrapped model
fittedCI[i,j]<-boot_coefs[i,1]+sum(newdata[j,]*boot_coefs[i,-1])
fittedPI[i,j]<-boot_coefs[i,1]+sum(newdata[j,]*boot_coefs[i,-1])+randres[i]
}

}else{
boot_models[[i]]<-0
boot_coefs[i,]<-NA #save coefficients used, including added intercept term of 0, as needed
randres[i]<-0

if(isometry.Null){
fittedCI[i,]<-rowmeans(newdata[,colnames(training_input)[-1]])/mean(training_input[1,-1])*training_input[1,1] #isometric predictions
fittedPI[i,]<-fittedCI[i,]}else{
fittedPI[i,]<-NA
fittedCI[i,]<-NA
}

some_identical<-TRUE
}

}##end bootstrap
if(some_identical & isometry.Null) message("Some bootstrap repetitions resulted in resampled datasets with only a single effective observation, corresponding predictions are generated isometrically")
if(some_identical & !isometry.Null) message("Some bootstrap repetitions resulted in resampled datasets with only a single effective observation, corresponding predictions are replaced with NAs")

#construct confidence intervals:
ci<-c((1-level)/2,level+(1-level)/2)

#retransform and apply smearing_factor
if(v) print(fit)
if(v) print(retransform(fit))
retransform(fit)*smearing_factor_main->fit
retransform(fittedCI)*smearing_factors->fittedCI
retransform(fittedPI)*smearing_factors->fittedPI

apply(X=fittedCI,MAR=2,FUN=quantile,probs=ci,na.rm=TRUE)->CI
apply(X=fittedPI,MAR=2,FUN=quantile,probs=ci,na.rm=TRUE)->PI

fit<-data.frame(fit=fit,lwr_CI=CI[1,],upr_CI=CI[2,],lwr_PI=PI[1,],upr_PI=PI[2,])
}else{
retransform(fit)*smearing_factor_main->fit
}

list(original_model=model,newdata=newdata,model_transformations=model_transformations,smearing_factor_main=smearing_factor_main)->out
if(bootstrap){
out$boot_models<-boot_models
out$boot_coefficients<-boot_coefs
out$random_residual<-randres
out$boot_smearing_factors<-smearing_factors
out$fittedCI<-fittedCI
out$fittedPI<-fittedPI
out$retransformations_applied<-retransform
}
out$fit<-fit
class(out)<-c("preds_lm2")
if(bootstrap) return(out)
if(!bootstrap) return(list(fit=fit))
}##


##plot.lm2()
#' plot a model of class lm2 from the output of lm2()
#' @param linmod2 an object of class "preds_lm2"
#' @param transform function to transform predictor variable. If not a function, model is assumed to be linear in current plotting space and is plotted using abline() instead of curve(), for greater speed
#' @param retransform function to back-transform predicted variable. If function is identity, model is assumed to be linear in current plotting space and is plotted using abline() instead of curve(), for greater speed
#' @param other.predictors named list() or data.frame() with values for other predictors (either constant or same number as n parameter used with curve.
#' @param varname name for raw predictor variable (if transformations were applied so it does not match name of coefficient)
#' @param predvar predictor variable, defaults to 2 (=slope of the bivariate intercept model)
#' @param smearing.corr should smearing factor be applied (see predict.lm2)
#' @param ... additional parameters to pass on to curve
#' @return nothing, but adds lines for all bootstrapped model to the current plot
#' @export plot.lm2
#' @method plot lm2
#' @importFrom stats predict weighted.mean
#' @examples
plot.lm2<-function(m,varname=NULL,transform=identity,retransform=identity,other.predictors=NULL, predvar=2,smearing.corr=FALSE,...){
match.call()->call
#calculate smearing factor
corr<-1
if(smearing.corr && "retransform" %in% names(call) && call$retransform!=substitute(identity)){
if("weights"%in%names(m)){corr<-weighted.mean(retransform(m$residuals),m$weights)}else{corr<-mean(retransform(m$residuals))}
}

#coefficient names
nam<-names(m$coefficients)
if(!is.null(varname)){nam[predvar]<-varname}
#message("preds for",nam[predvar],"in",nam)
##plot models
if(is.null(other.predictors)){ # retransformed bivariate model
predict(m, newdata=setNames(data.frame(transform(1)),nam[predvar]) , bootstrap=FALSE)->ctrl

if(!is.null(names(ctrl)) && "fit"%in%names(ctrl)){ 
	curve( retransform( predict(m, newdata=setNames(data.frame(transform(x)),nam[predvar]) , bootstrap=FALSE)$fit)*corr, add=TRUE,...)
}else{
	curve( retransform( predict(m, newdata=setNames(data.frame(transform(x)),nam[predvar]) , bootstrap=FALSE))*corr, add=TRUE,...)
}

}else{ # retransformed multivariate model
predict(m, newdata=setNames(data.frame(transform(1),transform(other.predictors[1,])),c(nam[predvar],names(other.predictors))) ,bootstrap=FALSE)->ctrl

if(!is.null(names(ctrl)) && "fit"%in%names(ctrl)){ 
curve( retransform( predict(m, newdata=setNames(data.frame(transform(x),transform(other.predictors)),c(nam[predvar],names(other.predictors))) ,bootstrap=FALSE)$fit)*corr, add=TRUE,...)
}else{
curve( retransform( predict(m, newdata=setNames(data.frame(transform(x),transform(other.predictors)),c(nam[predvar],names(other.predictors))) ,bootstrap=FALSE) )*corr, add=TRUE,...)
}

}
}##



##plot.preds_lm2()
#' plot the bootstrapped models from the output of predict.preds_lm2
#' @param preds_lm2 an object of class "preds_lm2"
#' @param transform function to transform predictor variable. If not a function, model is assumed to be linear in current plotting space and is plotted using abline() instead of curve(), for greater speed. Note that the function does not automatically transform the predictor, so you may need to specify the transformation used for the model here, even if you already fitted the model transformations inside the model formula. XXX
#' @param varname name for raw predictor variable (if transformations were applied so it does not match name of coefficient)
#' @param retransform function to back-transform predicted variable. If function is identity, model is assumed to be linear in current plotting space and is plotted using abline() instead of curve(), for greater speed.
#' @param nmodel number of first nmodel models to plot, if NULL (default) all models are plotted
#' @param other.predictors named list() or data.frame() with values for other predictors (either constant or same number as n parameter used with curve.
#' @param predvar predictor variable, defaults to 2 (=slope of the bivariate intercept model)
#' @param ... additional parameters to pass on to curve
#' @return nothing, but adds lines for all bootstrapped model to the current plot
#' @export plot.preds_lm2
#' @method plot preds_lm2
#' @importFrom stats predict
#' @examples
plot.preds_lm2<-function(preds_lm2,varname=NULL,transform=identity,retransform=identity, nmodel=NULL,other.predictors=NULL, predvar=2,sample.randres=FALSE,...){
match.call()->call
if(is.null(nmodel)) length(preds_lm2$boot_models)->nmodel

#coefficient names
nam<-names(preds_lm2$original_model$coefficients)
if(!is.null(varname)){nam[predvar]<-varname}

if((("transform" %in%names(call) && call$transform==substitute(identity)) | !is.function(transform)) && ((("retransform" %in%names(call) && call$retransform==substitute(identity)) | !is.function(retransform))) && is.null(other.predictors) && !sample.randres && preds_lm2$smearing_factor_main==1){ #simplest linear case, for speed of plotting

for(i in 1:nmodel){
if(!is.numeric(preds_lm2$boot_models[[i]]) && !is.logical(preds_lm2$boot_models[[i]])) abline(preds_lm2$boot_models[[i]],...)
}

}else if(is.null(other.predictors)){ # retransformed bivariate model

if(!sample.randres) { #if no random residuals should be added
for(i in 1:nmodel){
preds_lm2$boot_models[[i]]->m
if(!is.numeric(m) && !is.logical(m)) curve( retransform( predict(m, newdata=setNames(data.frame(transform(x)),nam[predvar]),bootstrap=FALSE)$fit)*preds_lm2$boot_smearing_factors[i], add=TRUE,...)
}}else{ #if random residuals should be added
for(i in 1:nmodel){
preds_lm2$boot_models[[i]]->m
if(!is.numeric(m) && !is.logical(m)) curve( retransform( predict(m, newdata=setNames(data.frame(transform(x)),nam[predvar]) ,bootstrap=FALSE)$fit+preds_lm2$random_residual[i])*preds_lm2$boot_smearing_factors[i], add=TRUE,...)
}}

}else{ # retransformed multivariate model

if(!sample.randres) { #if no random residuals should be added
for(i in 1:nmodel){
preds_lm2$boot_models[[i]]->m
if(!is.numeric(m) && !is.logical(m)) curve( retransform( predict(m, newdata=setNames(data.frame(transform(x,other.predictors)),c(nam[predvar],names(other.predictors))) ,bootstrap=FALSE)$fit)*preds_lm2$boot_smearing_factors[i], add=TRUE,...)
}}else{ #if random residuals should be added
preds_lm2$boot_models[[i]]->m
if(!is.numeric(m) && !is.logical(m)) curve( retransform( predict(m, newdata=setNames(data.frame(transform(x,other.predictors)),c(nam[predvar],names(other.predictors))) ,bootstrap=FALSE)$fit+preds_lm2$random_residual[i])*preds_lm2$boot_smearing_factors[i], add=TRUE,...)
}
}
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

apply(X=preds_lm2$boot_coefficients,2,FUN=quantile, probs=ci,na.rm=TRUE)->parameter_CI

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
#' @param s compute sample standard deviation, default TRUE
#' @export wsd
#' @return the weighted standard deviation (or variance, or mean)
wsd <- function(x, w=rep(1,length(x)), na.rm=TRUE,s=TRUE) {
if(na.rm){which(!is.na(x) & !is.na(w))->indices
x[indices]->x
w[indices]->w}
if(any(w<0)) warning("some weights were <0")
	wm <- sum(w * x) / sum(w) #wtd.mean
#	variance <- sum(w * (x - wm)^2) / (sum(w) - sum(w^2) / sum(w)) #wtd.var
variance <- sum(w * (x - wm)^2) / (sum(w) - 1) #wtd.var
if(!s) variance <- sum(w * (x - wm)^2) / sum(w) #wtd.var
	sqrt(variance) #wtd.stdev
}

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


