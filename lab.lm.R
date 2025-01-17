 
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
#' @param pvalue Logical indicating whether to plot root mean square error of model in second line. If FALSE (default), then multiple R-squared is shown.
#' @param ... Other parameters to be passed on to text()
#' @return Adds the model formula and R-squared or p-value to the current plot in the specified location
#' @importFrom graphics text
#' @importFrom graphics par
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

if(!intercept) bquote(R[pseudocentered]^2  == .(signif(1-sum(resid(lm.skulll0)^2)/sum((predict(lm.skulll0)+resid(lm.skulll0)-mean(predict(lm.skulll0)+resid(lm.skulll0)))^2),digits)))->rsq ##to force a pseudo-centered R², even if no intercept is included (still more useful for comparison than uncentered R²!)
if(intercept) bquote(R^2 == .(signif(summary(lm)$r.squared,digits)))->rsq

bquote(RMSE == .(sqrt(mean(resid(lm)^2))))->rmse

}else if(transformation=="log" | transformation=="LOG"){
bquote(y == .(signif(exp(coef(lm)[1]),digits))*x^.(signif(coef(lm)[2],digits)))->eq

bquote(R[log-transformed]^2 == .(signif(summary(lm)$r.squared,digits)))->rsq

bquote(RMSE == .(sqrt(mean(resid(lm)^2))))->rmse ##XXX TODO instead of log-residuals, use untransformed residuals to permit direct comparisons

}else if(transformation=="log10" | transformation=="LOG10"){
bquote(y == .(signif(10^(coef(lm)[1]),digits))*x^.(signif(coef(lm)[2],digits)))->eq

bquote(R[log-transformed]^2 == .(signif(summary(lm)$r.squared,digits)))->rsq

bquote(RMSE == .(sqrt(mean(resid(lm)^2))))->rmse ##XXX TODO instead of log-residuals, use untransformed residuals to permit direct comparisons

}

if(exists("modelp")){
modelp(lm)->pval
if(pval==0){
pval<-"<2.2E-16"
}
if(is.numeric(pval)){
bquote(p == .(signif(pval,digits)))->mp}else{
bquote(p == .(pval,digits))->mp
}

}
message("inserting label at x = ", xtxt, ", y = ",ytxt)

text(x=xtxt,y=ytxt, adj=adj, eq,...)

if(pvalue & exists("mp")){text(x=xtxt,y=ytxt, adj=c(adj[1],adj[2]+spacing), mp,...)}else{
if(!RMSE) text(x=xtxt,y=ytxt, adj=c(adj[1],adj[2]+spacing), rsq,...)##plot R²
if(RMSE) text(x=xtxt,y=ytxt, adj=c(adj[1],adj[2]+spacing), rmse,...)##plot root mean square error
}

}
