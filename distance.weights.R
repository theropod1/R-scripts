##distance.weights()
#' return weights based on phylogenetic divergence from a focal taxon in a calibrated phylogeny for use in phylogenetically weighted regression
#'
#' @param
#' @param
#' @param method method by which weights should be calculated. Options:
#' \itemize{
#'   \item \code{patristic linear} — (default) returns the difference between the tree height and the average distance from the focal taxon to its most recent common ancestor with each reference taxon, representing the expected correlation of two taxa in a brownian motion model of trait evolution (Hansen and Martins 1996). In an ultrametric tree, this corresponds to the shared time on the tree, but will differ from it in branches that are not ultrametric and where half the patristic distance is not equal to the \code{tree_height - shared_time}.
#'   \item \code{patristic} — Returns the raw patristic distance between the focal taxon and each reference taxon
#'   \item \code{patristic inverse} — Returns the inverse of the patristic distance between the focal taxon and each reference taxon
#'   \item \code{OU} — Returns the Ornstein-Uhlenbeck weight based on the patristic distance to each reference taxon, given as \code{weight=exp(-alpha*distance)} following Davies et al. (2019). Decay constant alpha should be determined via optimization.
#' \item \code{OU} — Returns the Ornstein-Uhlenbeck weight based on the patristic distance to each reference taxon, given as \code{weight=exp(-alpha*distance)} following Davies et al. (2019). Decay constant alpha should be determined via optimization.
#' \item \code{shared time} — Returns the absolute shared evolutionary time of the focal and reference taxa on the tree (i.e. the distance of the most recent common ancestor with each reference taxon). Not corrected for non-ultrametricity.
#' \item \code{shared relative} — Returns the shared evolutionary time of the focal and reference taxa on the tree relative to total tree height. Not corrected for non-ultrametricity.
#' }
#' @references
#' Davies, T.J., Regetz, J., Wolkovich, E.M., and McGill, B.J. 2019. Phylogenetically weighted regression: A method for modelling non-stationarity on evolutionary trees. \emph{Global Ecology and Biogeography}: 28:275––285. \doi{10.1111/geb.12841}.
#' Hansen, T.F. and Martins, E.P. 1996. Translating Between Microevolutionary Process and Macroevolutionary Patterns: The Correlation Structure of Interspecific Data. \emph{Evolution}: 50:1404––1417. \doi{10.1111/j.1558-5646.1996.tb03914.x}.
#'
#' @param alpha decay constant (or inverse of bandwidth) for Ornstein-Uhlenbeck model of trait weighting
#' @param exclude.focal logical indicating whether focal taxon should be set to NA and ignored in calculating the weights
#' @return a vector of weights calculated according to the specified method
#' @export distance.weights
#' @importFrom ape cophenetic.phylo node.depth.edgelength mrca
#' @examples
#'

distance.weights<-function(phy,focal,method="patristic linear", alpha=1, exclude.focal=TRUE){

if(method%in%c("patristic inverse","distance inverse")){# patristic distance(focal-reference)^(-1)
ape::cophenetic.phylo(phy)->D
D[,focal]->D
if(exclude.focal) NA->D[which(names(D)==focal)]

return(D^(-1))
}


if(method%in%c("patristic linear","distance linear")){# maximum tree length - mean distance to last common ancestor (=0.5*patristic distance(focal-reference), i.e. equivalent to linear decrease of correlation with increasing patristic distance or (for ultrametric trees) with shared time to common ancestor
ape::cophenetic.phylo(phy)->D
D[,focal]->D
if(exclude.focal) NA->D[which(names(D)==focal)]
max(ape::node.depth.edgelength(phy))-D*0.5->scaled_D

return(scaled_D)
}


if(method%in%c("patristic absolute","distance absolute","patristic")){#raw patristic distance(focal-reference)
ape::cophenetic.phylo(phy)->D
D[,focal]->D
if(exclude.focal) NA->D[which(names(D)==focal)]

return(D)
}

if(method%in%c("ornstein uhlenbeck","OU","O/U")){#Ornstein-Uhlenbeck as described by Davies et al. 2019; weights exponentially decaying with phylogenetic distance
ape::cophenetic.phylo(phy)->D
D[,focal]->D
if(exclude.focal) NA->D[which(names(D)==focal)]

return(exp(-alpha*D))
}

if(method%in%c("shared absolute","shared time","mrca absolute")){#shared evolutionary time between focal and reference from root of tree in ma
ape::mrca(phy)->anc
ape::node.depth.edgelength(phy)[anc[,focal]]->anc_
names(anc_)<-rownames(anc)
if(exclude.focal) NA->anc_[which(names(anc_)==focal)]

return(anc_)
}

if(method%in%c("shared relative","mrca relative")){#shared evolutionary time between focal and reference from root of tree as proportion of tree length
ape::mrca(phy)->anc
ape::node.depth.edgelength(phy)[anc[,focal]]->anc_
names(anc_)<-rownames(anc)
if(exclude.focal) NA->anc_[which(names(anc_)==focal)]

return(anc_/max(ape::node.depth.edgelength(phy)))
}

}##



##alpha_error()
#' Return the mean squared error or the predictions and error terms from a given value for the alpha constant in an Ornstein-Uhlenbeck model of phylogenetic weighting, using predictions from an lm() or lm2() model
#' 
#' @param alpha decay constant for Ornstein-Uhlenbeck model
#' @param formula model formula to fit to the data
#' @param data dataset from which to pull values
#' @param phy scaled phylogeny containing all the taxa in the dataset
#' @param taxon vector of taxon name (or column of data containing it)
#' @param retransform back-transformation to apply to values before calculating mean square error. Defaults to 10^x (for retransforming log10()-transformed data), if no retransformation should happen, specify identity here.
#' @param v verbosity setting. if v==TRUE, function returns a data frame with all original data, fitted values from the base and OU-weighted model, and error terms for each estimate.
#' @param ... additional arguments to pass on to predict(), e.g. retransform and smearing.corr, if desired
#' @return if v==FALSE (default), the mean square error at the chosen alpha value
#' @export alpha_error
#' @examples
#' \dontrun{ alpha_error(alpha=0.08,formula=log10_fl~log10_tl, data=CLP, phy=tree0, taxa=CLP$taxon,v=FALSE) }

##calculate the mean square error for a given alpha value
alpha_error<-function( alpha, formula, data, phy, taxa=taxon, fit.fun=lm2, retransform=function(x){10^x}, v=FALSE, smearing.corr=FALSE) {
match.call()->call

  if(!is.null(data)){mf <- model.frame(formula, data, na.action=na.pass)
  }else{
  mf <- model.frame(formula, na.action=na.pass)
  }
  
fit.fun(log10_fl~log10_tl, data=CLP)->base_model #lm2_fltl

retransform(mf[,1])->response#look up and retransform data from model to find response variable

eval(call$taxa, data, parent.frame())->taxa #look for taxa

if(all(taxa%in%phy$tip.label) & !is.null(phy$edge.length)){
if(v) message("Edge length and taxa in phylogeny: OK")

smearing_factor_main<-1
if(smearing.corr) smearing_factor_main<-mean(retransform(base_model$residuals))

retransform(predict(base_model, data, bootstrap=F)$fit)*smearing_factor_main->no_focal

focal_preds<-numeric()
for(i in 1:length(unique(taxa))){

distance.weights(tree0,unique(taxa)[i],method="OU",alpha=alpha, exclude.focal=TRUE)[taxa]->rweight #distance weights with given alpha, focal taxon excluded (=NA)

fit.fun(log10_fl~log10_tl, data=data[taxa!=unique(taxa)[i],], w=rweight[taxa!=unique(taxa)[i]])->focal_model #excluding focal taxon again, in case model fitting function can’t handle NA

smearing_factor_focal<-1
if(smearing.corr) smearing_factor_focal<-mean(retransform(focal_model$residuals))
if(smearing.corr & "weights"%in%names(focal_model)) smearing_factor_focal<-weighted.mean(retransform(focal_model$residuals),focal_model$weights)

retransform(predict(focal_model,newdata=data,bootstrap=F)$fit)*smearing_factor_focal->focal
focal[which(taxa==unique(taxa)[i])]->focal_preds[which(taxa==unique(taxa)[i])]
}

results<-data.frame( taxa,retransform(mf[,1]),retransform(mf[,-1]),no_focal,focal_preds,err_nofocal=(no_focal-response),squerr_nofocal=(no_focal-response)^2,err_focal=(focal_preds-response),squerr_focal=(focal_preds-response)^2 ) #for testing purposes

MSQ<-mean(results$squerr_focal,na.rm=TRUE)
message("alpha =", alpha,"--> MSQE =", MSQ) #to give update during optimization procedures

if(any(is.na(results$squerr_focal)) & v) message("NOTE: some square errors were NA. This is normal if your data contained rows lacking predictors")

if(v){return(results)}else{
return(MSQ) 

}}else{stop("Not all taxa could be found in phylogeny!")}

}##

