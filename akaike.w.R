##function akaike.w()
#' Calculate the akaike weights or bayesian information criterion weights for a vector of information criteria of different models
#' @param ICs numeric vector containing the Akaike information criterion or Bayesian information criterion for each model
#' @param names (optional) a character vector of the same length as ICs containing the names of the models
#' @param return_all Whether to return data.frame with ICs, delta_ICs, relative likelihood and weight (if TRUE), or just the weights (if FALSE, default)
#' @return A dataframe containing AIC, delta_AIC, relative likelihood and akaike weight for each supplied AIC, or a numeric vector containing the akaike weights
#' @export akaike.w
#' @examples
#' akaike.w(c(200,202,205), c("modelA","modelB","modelC"))

#AIC= -2*log(likelihood)+2(K)*n/(n-K-1) #where K is n_parameters and likelihood is the probability of the data given a model (see stats::logLik())

akaike.w<-function(ICs,names=NULL, return_all=FALSE){

delta_ICs<-ICs-min(ICs)

tot_rel_likelihood<-sum(exp(-0.5*delta_ICs))

rel_likelihood<-exp(-0.5*delta_ICs)
a.w <- rel_likelihood/tot_rel_likelihood

a.w.df <- data.frame(AIC=ICs, delta=delta_ICs,  relative_likelihood=rel_likelihood, weight=a.w)

if(!is.null(names)){
if(length(names)==length(ICs) & is.character(names)){
names(a.w)<-names
rownames(a.w.df)<-names
}}

if(return_all==FALSE){
return(a.w)}else{
return(a.w.df)}

}
##
