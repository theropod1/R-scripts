##function: 
#automatically extract the p value for a model fitted using lm(). signif gives number of significant digits, of desired.


modelp<-function(x, signif=NULL){
summary(x)$fstatistic->f

if(!is.null(signif)){
signif(1-pf(f[1],f[2],f[3]),signif)->pval
#if(pval==0){return(paste0("<0.",paste(rep(0,signif-2), collapse=""),1))}else{return(pval)}
}else{
1-pf(f[1],f[2],f[3])->pval
}
if(pval==0){return("<2.2e-16")}else{return(pval)}

}
##
