##function
#redraws the lines on the current phylogenetic tree or (optionally) a saved tree
redraw.phylo<-function(saved_plot=NULL,col="black",lwd=1,lty=1,lend=2,arrow.l=0, arrow.angle=45, arrow.code=2, indices=NULL){
#load data
if(is.null(saved_plot)){
lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
}else(lastPP<-saved_plot)

if(!is.null(indices)){
lastPP$edge[indices,]->lastPP$edge
}
#now loop through edges
for(i in 1:nrow(lastPP$edge)){
lastPP$edge[i,1]->starti
lastPP$edge[i,2]->endi
if(lastPP$type=="cladogram"){
arrows(lastPP$xx[starti],lastPP$yy[starti],lastPP$xx[endi],lastPP$yy[endi],col=col,lwd=lwd, lty=lty,lend=lend,length=arrow.l,angle=arrow.angle, code=arrow.code)

}else if(lastPP$type=="phylogram"){
arrows(lastPP$xx[starti],lastPP$yy[starti],lastPP$xx[starti],lastPP$yy[endi],length=0,col=col,lwd=lwd, lty=lty,lend=lend)
arrows(lastPP$xx[starti],lastPP$yy[endi],lastPP$xx[endi],lastPP$yy[endi],col=col,lwd=lwd, lty=lty,lend=lend,length=arrow.l,angle=arrow.angle, code=arrow.code)
}else{
stop("Only phylogram and cladogram supported so far")
}
}

}
##
