##phylo_coords() function
#' Take a phylogenetic tree and return node x y coordinates of nodes, optionally plot tree
#' @param tree phylo-class object
#' @param ages optional age matrix for time calibration following the "FAD"-"LAD" scheme of strap::DatePhylo, with taxon names as rownames
#' @param add.terminal logical whether terminal branch should be added during time calibration (if it is performed)
#' @param plot logical indicating whether to plot phylogeny. If TRUE, function will first attempt to add plot to the active plotting device, unless add==FALSE
#' @param type Plotting method (can be "cladogram" or "phylogram", see ape::plot.phylo for details
#' @param add logical indicating whether to try adding tree to current plotting device
#' @param length length parameter for arrow tips (passed to arrows())
#' @param yscale optional numeric vector of length two giving as its first element the scale and as its second element the offset to add to the branching coordinate (y in horizontal plot). Default c(1,1) scales values so that terminal branches are centered on integers starting at 1.
#' @param root.edge add root edge? Can be logical or numeric giving the length of the root edge to plot. If TRUE, attempts to use tree$root.edge to set root edge length
#' @param horiz logical; plot horizontally (default TRUE)
#' @return a list() object with the (time-calibrated) tree, raw and time-converted x and y coordinates of all nodes, edge matrix and lineage min and max ages
#' @importFrom strap DatePhylo
#' @export phylo_coords

phylo_coords <- function(tree,ages=NULL,add.terminal=TRUE, plot=FALSE, type="cladogram",add=TRUE,yscale=c(1,1),root.edge=FALSE, horiz=TRUE,length=0, ...) {

if(root.edge==TRUE & "root.edge"%in% names(tree)) root.edge<-tree$root.edge #try to pull root.edge from tree
if(root.edge==TRUE & !"root.edge"%in% names(tree)) root.edge<-0 #fallback if no number provided

if(!is.null(ages) && is.matrix(ages) && all(tree$tip.label%in%rownames(ages))) strap::DatePhylo(tree,ages[tree$tip.label,],method="equal",rlen=1,add.terminal=add.terminal)->tree


  tree <- ape::reorder.phylo(tree, "cladewise")

  Ntip  <- length(tree$tip.label)
  Nnode <- tree$Nnode
  Ntot  <- Ntip + Nnode

  ## ---------- x coordinates ----------

  x <- numeric(Ntot)

  root <- setdiff(tree$edge[,1], tree$edge[,2])

  repeat {
    changed <- FALSE

    for(i in seq_len(nrow(tree$edge))) {

      parent <- tree$edge[i,1]
      child  <- tree$edge[i,2]

      if(parent == root || x[parent] > 0 || parent == root) {

        newx <- x[parent] + tree$edge.length[i]

        if(x[child] != newx) {
          x[child] <- newx
          changed <- TRUE
        }
      }
    }

    if(!changed) break
  }

  ## ---------- y coordinates ----------

  y <- numeric(Ntot)

  ## tip positions
  y[1:Ntip] <- rev(seq_len(Ntip))

  ## internal nodes from tips upward
  internal <- rev(unique(tree$edge[,1]))

  for(node in internal) {

    children <- tree$edge[
      tree$edge[,1] == node,
      2
    ]

    y[node] <- mean(y[children])
  }
  
  y<-(y-1)*yscale[1]+yscale[2] #scale y intervals
  
#generate lineage start and end points
xx<-data.frame( max=x[tree$edge[,1]], min=x[tree$edge[,2]] )
xx[which(xx[,1]!=xx[,2]),]->xx #drop zero length branches
tree$root.time-xx->xx_conv
colnames(xx)<-c("min","max")

#reverse y for ape compatibility
y<-max(y)-y+1

#plotting x and y:
x_<-tree$root.time-x
y_<-y

##construct output
out <-  list(
    tree = tree,
    x_raw = x,
    x_ma = x_,
    y = y,
    edge = tree$edge,
    lineages_raw = xx,
    lineages_ma = xx_conv,
    root.time = tree$root.time
  )

if(!horiz){
tmp<-x_
y_->x_
tmp->y_

tmp<-x
y->x
tmp->y
}

if(plot){ ##plot tree

if(!add & horiz) plot(0,0,type="n",xlim=rev(range(x_)),ylim=range(y_),xlab="",ylab="",axes=FALSE)
if(!add & !horiz) plot(0,0,type="n",xlim=range(x_),ylim=rev(range(y_)),xlab="",ylab="",axes=FALSE)

if( dev.cur()<2 | (dev.cur()>1 && par("usr")[1]>par("usr")[2] & horiz) | (dev.cur()>1 && par("usr")[3]>par("usr")[1] & !horiz) ){ #plotting in "normal space" (node ages)

if(dev.cur()<2 & horiz) plot(0,0,type="n",xlim=rev(range(x_)),ylim=range(y_),xlab="",ylab="",axes=FALSE)
if(dev.cur()<2 & !horiz) plot(0,0,type="n",xlim=range(x_),ylim=rev(range(y_)),xlab="",ylab="",axes=FALSE)
if(type!="cladogram"){
if(horiz){
arrows(x0=x_[tree$edge[,1]], y0=y_[tree$edge[,1]], x1=x_[tree$edge[,1]], y1=y_[tree$edge[,2]],length=length,...)
arrows(x0=x_[tree$edge[,1]], y0=y_[tree$edge[,2]], x1=x_[tree$edge[,2]], y1=y_[tree$edge[,2]],length=length,...)}else{

arrows(x0=x_[tree$edge[,1]], y0=y_[tree$edge[,1]], x1=x_[tree$edge[,2]], y1=y_[tree$edge[,1]],length=length,...)
arrows(x0=x_[tree$edge[,2]], y0=y_[tree$edge[,1]], x1=x_[tree$edge[,2]], y1=y_[tree$edge[,2]],length=length,...)
}

}else if(type=="cladogram") arrows(x_[tree$edge[,1]], y_[tree$edge[,1]], x_[tree$edge[,2]], y_[tree$edge[,2]],length=length,...)

if(root.edge!=FALSE & is.numeric(root.edge)) if(horiz) arrows(x_[root]+root.edge, y_[root], x_[root], y_[root],length=length,...) else arrows(x_[root], y_[root]+root.edge, x_[root], y_[root],...)

}else{#if plotting in "ape space" (tree length)

if(type!="cladogram"){ 
if(horiz){
arrows(x[tree$edge[,1]], y[tree$edge[,1]], x[tree$edge[,1]], y[tree$edge[,2]],length=length,...)
arrows(x[tree$edge[,1]], y[tree$edge[,2]], x[tree$edge[,2]], y[tree$edge[,2]],length=length,...)}else{

arrows(x[tree$edge[,1]], y[tree$edge[,1]], x[tree$edge[,2]], y[tree$edge[,1]],length=length, ...)
arrows(x[tree$edge[,2]], y[tree$edge[,1]],x[tree$edge[,2]], y[tree$edge[,2]],length=length, ...)}

}else if(type=="cladogram") arrows(x0=x[tree$edge[,1]], y0=y[tree$edge[,1]], x1=x[tree$edge[,2]], y1=y[tree$edge[,2]],length=length,...)

if(root.edge!=FALSE & is.numeric(root.edge)) if(horiz) arrows(x[root]-root.edge, y[root], x[root], y[root],length=length,...) else arrows(x[root], y[root]-root.edge, x[root], y[root],length=length,...)

}

}#end plot



  
  return(out)
}
