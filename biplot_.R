##function biplot_()
#' Plot a customized biplot of a PCA
#' @param PCA PCA-class object containing PC scores in PCA$x and rotation matrix in PCA$rotation, or PC scores
#' @param PCs which principal components to plot (numeric of length 2 containing the x and y component indices)
#' @param loadings PCA rotation matrix (if not already in PCA). If FALSE or NULL and not in PCA$loadings, no arrows are plotted
#' @param alab arrow labels (if NULL/default, no labels are plotted)
#' @param PC.lab labels for principal components (default NULL, in which case their share of variance is shown)
#' @param arrow.settings list() object containing named arguments to be passed to arrows()
#' @param adjust factor by which to adjust default arrow length
#' @param v verbosity setting for troubleshooting (default FALSE)
#' @param plot Logical plotting setting (default TRUE)
#' @return invisible object containing the arrow coordinates and settings and the x and y coordinates of all points.
#' @export biplot_

biplot_<-function(PCA, PCs=c(1,2), loadings=NULL,alab=NULL, PC.lab=NULL, arrow.settings=list(angle=30),adjust=1,v=FALSE,plot=TRUE,...){

plot.arrows<-TRUE
if(!is.null(loadings) && loadings==FALSE) plot.arrows<-FALSE

if(v) print(1)

if(inherits(PCA,"prcomp")){
scores<-PCA$x
loadings<-PCA$rotation
if(is.null(PC.lab)) round(PCA$sdev^2/sum(PCA$sdev^2)*100,1)->tmp else PC.lab->tmp
if(v) print(2)

}else{
if(v) print(3)

scores<-PCA
loadings
if(is.null(loadings)) plot.arrows<-FALSE
warning("no loadings found or supplied, not plotting arrows!")
}

if(v) print(4)

if(plot==TRUE){
plot(scores[,PCs[1]], scores[,PCs[2]], xlab=paste0("PC",PCs[1]," ",tmp[PCs[1]],"%"), ylab=paste0("PC",PCs[2]," ",tmp[PCs[2]],"%"), ...)  # Adjust limits as needed
#lines(scores[,1], scores[,2], col = add.alpha("lightgreen",0.3))  # Adjust limits as needed

if (plot.arrows) {
if(v) print(5)

  arrow.args <- c(
    list(x0 = 0, y0 = 0,
      x1 = loadings[,PCs[1]] * max(scores[,PCs[1]]) * adjust,
      y1 = loadings[,PCs[2]] * max(scores[,PCs[2]]) * adjust),
    arrow.settings)
    
  do.call(arrows, arrow.args) #draw arrows
  
  if(!is.null(alab)){ 
  adjx<-ifelse(arrow.args$x1>0,0,1)
  adjy<-ifelse(arrow.args$y1>0,0,1)
  txtcol<-par("fg")
  if("col"%in%names(arrow.args)) txtcol<-arrow.args$col
  if(length(txtcol)<length(alab)) rep(txtcol,length(alab))[1:length(alab)]->txtcol
  
    if(v) print(txtcol)

  for(i in 1:length(alab)){
  if(v) print(c(alab[i],txtcol[i]))
  
  text(arrow.args$x1[i],arrow.args$y1[i],labels=alab[i], adj=c(adjx[i],adjy[i]),col=txtcol[i])
  }
  }
}
}
invisible(list(pc_scores=data.frame(x=scores[,PCs[1]],y=scores[,PCs[2]]),arrow.args=arrow.args))

}
