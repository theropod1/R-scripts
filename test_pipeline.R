##function test_pipeline
#' performs a sequence of statistical tests for significance of differences between levels of a categorical variable, evaluates whether ANOVA assumptions are met, performs robust anova and non-parametric testing and runs post-hoc tests for all models if categorical variable is not binary.
#' @param x formula for test (e.g. y~x) or value for the independent (categorical) variable
#' @param data optional data.frame object in which to look for x and y
#' @param y optional value for the response variable, if not already provided as a formula in x
#' @param na.rm logical indicating whether NA rows in x,y should be removed (default TRUE)
#' @param v verbosity setting (default TRUE)
#' @param p.crit critical p value to determine significance (defaults to 0.05)
#' @param robust logical indicating whether robust anova (t1way from package WRS2) and pos hoc testing (lincon from from package WRS2). Default TRUE. Always reverts to TRUE if anova prerequisites (variance homogeneity and residual normality) are not met at p.crit
#' @param nonpara logical indicating whether non-parametric test should be applied (Kruskal-Wallis rank sum test) and pos hoc testing (dunn_test from from package rstatic). Default TRUE. Always reverts to TRUE if anova prerequisites (variance homogeneity and residual normality) are not met at p.crit.
#' @param plot logical whether qqplot of anova residuals should be plotted
#' @return a list object containing the output of all tests that were performed
#' @importFrom car leveneTest
#' @importFrom WRS2 lincon t1way
#' @importFrom rstatix dunn_test
#' @importFrom stats kruskal.test shapiro.test aov TukeyHSD

test_pipeline<-function(x,data=NULL,y=NULL,na.rm=TRUE,v=TRUE,p.crit=0.05,robust=TRUE,nonpara=TRUE,plot=FALSE){
out<-list()
MODEL_FORMULA<-NA

if(inherits(x,"formula")){ 
MODEL_FORMULA<-deparse(x)

#extract formula sides
lhs_expr <- x[[2]]
rhs_expr <- x[[3]]

#extract functions
lhs_fun <- if (is.call(lhs_expr)) as.character(lhs_expr[[1]]) else identity
rhs_fun <- if (is.call(rhs_expr)) as.character(lhs_expr[[1]]) else identity

#determine environment
env <- if (is.null(data)) parent.frame() else data

#eval and build model frame
y <- eval(lhs_expr, envir = env)
x <- eval(rhs_expr, envir = env)

mf<-data.frame(x = x, y = y)
  
}else if(length(x)==1 & is.character(x) & (is.data.frame(data) | is.matrix(data))){
mf<-as.data.frame(data[,c(x,y)]) #get named variables from df
colnames(mf)<-c("x","y")
}else{
data.frame(x=x,y=y)->mf
} #get raw variables from global env
if(is.na(MODEL_FORMULA)) MODEL_FORMULA<-paste0(deparse(substitute(y)),"~",deparse(substitute(x)))
if(is.character(mf$x)) factor(mf$x)->mf$x #make factor, in case needed

if(na.rm) mf<-mf[complete.cases(mf),]

message("\n============= \n", MODEL_FORMULA, "\n=============")
#if(v) print(mf)
#above code saves evaluated expression of model or evaluated variables of model as data.frame named mf, containing fully evaluated variables for the formula. if x is a formula, model functions (e.g. log) are also retained, specifically as lhs_fun and rhs_fun


##conduct tests
aov(y~x,mf)->out$ANOVA_res

summary(out$ANOVA_res)->out$summary_ANOVA_res

anova_p<-unclass(out$summary_ANOVA_res)[[1]]["x","Pr(>F)"]
anova_df<-unclass(out$summary_ANOVA_res)[[1]]["x","Df"]

if(anova_p<p.crit) message("ANOVA: significant differences were found in anova (p=",anova_p,")")
if(anova_p>p.crit) message("no significant differences were found in anova (p=",anova_p,")")
if(anova_p>p.crit && anova_p<(2*p.crit)) message("…but p is less than twice the critical value, indicating possible strong tendencies")

if( anova_df>1 && anova_p<(2*p.crit) ) {
message("more than 1 degree of freedom in aov(y~x), proceeding to post hoc testing")
TukeyHSD(out$ANOVA_res)->out$Tukey
as.data.frame(out$Tukey$x)->out$Tukey$x

if(any(out$Tukey$x$"p adj"<p.crit)) message("TukeyHSD post hoc test: significant differences found for: ", paste(rownames(out$Tukey$x)[which(out$Tukey$x$"p adj"<p.crit)],SC(out$Tukey$x$"p adj"[which(out$Tukey$x$"p adj"<p.crit)]),collapse=", "))

if(any(out$Tukey$x$"p adj">p.crit & out$Tukey$x$"p adj"<(2*p.crit))) message("TukeyHSD post hoc test: strong tendencies found for: ", paste(rownames(out$Tukey$x)[which(out$Tukey$x$"p adj">p.crit & out$Tukey$x$"p adj"<(2*p.crit))],SC(out$Tukey$x$"p adj"[which(out$Tukey$x$"p adj">p.crit & out$Tukey$x$"p adj"<(2*p.crit))]),collapse=", "))
}

#verify regular anova appropriateness and assumptions
if(plot){qqnorm(residuals(out$ANOVA_res))
qqline(residuals(out$ANOVA_res))
mtext(side=3,line=0.2,paste("Residuals of anova model:",MODEL_FORMULA))}

car::leveneTest(y~x,data=mf)->out$leveneTest # see if variances are approx equal
shapiro.test(residuals(out$ANOVA_res))->out$shapiro.test #see if residuals are approx. normal

if(out$leveneTest$"Pr(>F)"[1]>p.crit) message("variances are approximately homogeneous at p=",out$leveneTest$"Pr(>F)"[1])
if(out$leveneTest$"Pr(>F)"[1]<p.crit) message("! variances are inhomogeneous at p=",out$leveneTest$"Pr(>F)"[1])

if(out$shapiro.test$p.value>p.crit) message("residuals are approximately normal at p=", out$shapiro.test$p.value)
if(out$shapiro.test$p.value<p.crit) message("! residuals diverge significantly from normality at p=", out$shapiro.test$p.value)


if(out$leveneTest$"Pr(>F)"[1]>p.crit | out$shapiro.test$p.value>p.crit){robust<-TRUE
nonpara<-TRUE} #always perform nonparametric and robust tests in cases of variance inhomogeneity or non-normality of anova residuals

if(robust){
WRS2::t1way(y~x,data=mf)->out$Robust_ANOVA_res

robust_anova_p<-out$Robust_ANOVA_res$p.value

if(robust_anova_p<p.crit) message("ROBUST ANOVA: significant differences were found in robust anova (p=",robust_anova_p,")")
if(robust_anova_p>p.crit) message("no significant differences were found in robust anova (p=",robust_anova_p,")")
if(robust_anova_p>p.crit && robust_anova_p<(2*p.crit)) message("…but p is less than twice the critical value, indicating possible strong tendencies")

if( anova_df>1 && robust_anova_p<(2*p.crit) ) {

WRS2::lincon(y~x,data=mf)->out$lincon
as.data.frame(out$lincon$comp)->out$summary_lincon

#make results more legible:
paste(out$lincon$fnames[out$lincon$comp[,1]],"vs.",out$lincon$fnames[out$lincon$comp[,2]])->out$summary_lincon$comparison
SC(out$summary_lincon$p.value)->out$summary_lincon$signif_code

if(any(out$summary_lincon$p.value<p.crit)) message("Lincon post hoc test: significant differences found for: ", paste(out$summary_lincon$comparison[which(out$summary_lincon$p.value<p.crit)],out$summary_lincon$signif_code[which(out$summary_lincon$p.value<p.crit)],collapse=", "))
if(any(out$summary_lincon$p.value>p.crit & out$summary_lincon$p.value<(2*p.crit))) message("Lincon post hoc test: strong tendencies found for: ", paste(out$summary_lincon$comparison[which(out$summary_lincon$p.value>p.crit & out$summary_lincon$p.value<(2*p.crit))],out$summary_lincon$signif_code[which(out$summary_lincon$p.value>p.crit & out$summary_lincon$p.value<(2*p.crit))],collapse=", "))

}

}

if(nonpara){
##non-parametric test
kruskal.test(y~x,data=mf)->out$kruskal

kruskal_p<-out$kruskal$p.value

if(kruskal_p<p.crit) message("Kruskal-Wallis test: significant differences were found in kruskal-wallis test (p=",kruskal_p,")")
if(kruskal_p>p.crit) message("Kruskal-Wallis test: no significant differences were found in kruskal-wallis test (p=",kruskal_p,")")
if(kruskal_p>p.crit && kruskal_p<(2*p.crit)) message("…but p is less than twice the critical value, indicating possible strong tendencies")

if( anova_df>1 && kruskal_p<(2*p.crit) ) {
rstatix::dunn_test(data=mf, y~x, p.adjust.method = "holm")->out$dunn
as.data.frame(out$dunn)->out$dunn

if(any(out$dunn$p.adj<p.crit)) message("dunn post hoc test: significant differences found for: ", paste(out$dunn$group1[which(out$dunn$p.adj<p.crit)],"vs",out$dunn$group2[which(out$dunn$p.adj<p.crit)],out$dunn$p.adj.signif[which(out$dunn$p.adj<p.crit)],collapse=", "))

if(any(out$dunn$p.adj>p.crit&out$dunn$p.adj<(2*p.crit))) message("dunn post hoc test: strong tendencies found for: ", paste(out$dunn$group1[which(out$dunn$p.adj>p.crit&out$dunn$p.adj<(2*p.crit))],"vs",out$dunn$group3[which(out$dunn$p.adj>p.crit&out$dunn$p.adj<(2*p.crit))],out$dunn$p.adj.signif[which(out$dunn$p.adj>p.crit&out$dunn$p.adj<(2*p.crit))],collapse=", "))

}
}

return(out)
}##
