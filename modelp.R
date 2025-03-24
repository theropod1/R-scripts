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
