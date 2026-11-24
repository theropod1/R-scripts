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
}##

##function descendants()
#' find all descendants of a numbered node
#' @param node node number in tree
#' @param tree a phylo-class object
#' @param tree indices Logical indicating whether to return indices Default TRUE, otherwise tip.labels are returned
#' @return the indices or tip.labels of all descendants of the specified node
#' @export descendants
descendants <- function(node,tree,indices=TRUE) { #find indices of all descendants of a node
    if (node <= length(tree$tip.label)){ out<-as.integer(node)
    }else{
    ape::extract.clade(tree, node)$tip.label |> match(tree$tip.label)->out
    } #pipe taxon names into indices
    if(indices!=TRUE) tree$tip.label[out]->out

    return(out)
}##

##function cladenum()
#' find node and edge labels for minimum and maximum clades on a tree
#' @param tree a phylo-class object (minimum requirements Nnode, edge and tip.label)
#' @param include taxon or taxa to include in clade
#' @param exclude (optional) taxon or taxa to exclude
#' @param strict strict mode? (only return nodes if all c(include, exclude) found in tree)
#' @return node or edge number for chosen minimum or maximum clade definition in tree
#' @export cladenum

cladenum <- function(tree, include, exclude = NULL, strict=FALSE, v=TRUE, ignore.case=TRUE) {
if(ignore.case){ tolower->argfun #if not case-sensitive, transform all to lowercase
tree$tip.label<-tolower(tree$tip.label)
}else identity->argfun

d <-tryCatch({match.arg(argfun(c(include,exclude)), argfun(tree$tip.label),several.ok=TRUE)}, error=function(e){return("match.arg() RETURNED ERROR: no taxa found")}) #see if any taxa are found, if not return NA to avoid error
if(length(d)==1 && all(d=="match.arg() RETURNED ERROR: no taxa found")){
message(d)
return(structure(NA, def_type=d)) }else{

if(!strict) match.arg(argfun(include),argfun(tree$tip.label),several.ok=TRUE)->include
#if(v) message("positive constraints: ", paste(include,collapse=", "))

if(!is.null(exclude)){
if(!strict) match.arg(argfun(exclude),argfun(tree$tip.label),several.ok=TRUE)->exclude
#if(v) message("negative constraints: ", paste(exclude,collapse=", "))
if(v) message("max. clade: largest clade including ",paste(include,collapse=", "), " but not ", paste(exclude,collapse=", "))
definition<-paste0("max. clade: largest clade including ",paste(include,collapse=", "), " but not ", paste(exclude,collapse=", "))
}else{
definition<- paste0("min. clade: smallest clade including ",paste(include,collapse=", "))
if(v) message("min. clade: smallest clade including ",paste(include,collapse=", "))
}

if(strict==TRUE && !all(c(include,exclude)%in%tree$tip.label)){ #strict mode returns NA if not all taxa are found in the tree
message("Clade undefined because ",paste(c(include,exclude)[which(!(c(include,exclude)%in%tree$tip.label))],collapse=", ")," not found in tree")
clade<-NA
attr(clade,"def_type")<-"clade undefined"
attr(clade,"definition")<-definition
}else{#non-strict mode returns the taxon as defined by all those taxa found in the tree

minclade<-ape::getMRCA(tree, include) #find clade including all positive constraints
if(is.null(minclade)) minclade<-which(tree$tip.label==include) #if only one included taxon is given, return tip number

if(is.null(minclade)){ minclade<-NA
attr(minclade,"def_type")<-"clade undefined"
attr(minclade,"definition")<-NA
}#if minclade is still NULL, return undefined clade

if(!is.na(minclade) && !is.null(exclude) && length(exclude)>0){ ##maxclade
  
parent <- integer(tree$Nnode + length(tree$tip.label))
  #print(parent)

parent[tree$edge[, 2]] <- tree$edge[, 1] #define parent node for each end node

p<- best <- minclade
descendants(p,tree)->pdesc
if(any(exclude%in%tree$tip.label[pdesc])){
message("Clade undefined because ",paste(exclude[which(exclude%in%tree$tip.label[pdesc])],collapse=", ")," are part of minimum clade defined by ", paste(include,collapse=", "))

clade<-NA # determine parent edge of best node recovered
attr(clade,"def_type")<-"clade undefined"
attr(clade,"definition")<-paste0(message("Clade undefined because ",paste(exclude[which(exclude%in%tree$tip.label[pdesc])],collapse=", ")," are part of minimum clade defined by ", paste(include,collapse=", ")))

}else{

maxruns<-0
while(!any(exclude%in%tree$tip.label[pdesc]) && maxruns<1E6){
#message("parent of node ", p,":")
p<-parent[p] #update parent
descendants(p,tree)->pdesc #find descendants of parent node
if(!any(exclude%in%tree$tip.label[pdesc])) p->best #only save parent if exclusion condition still met
#message("minclade at node ", p, " includes excluded taxa? ",any(exclude%in%tree$tip.label[pdesc]))
#if(!any(exclude%in%tree$tip.label[pdesc])) message("saving p (",p,"):", best) #only save parent if condition still met

}
    
clade<-which(tree$edge[, 2] == best)# determine parent edge of best node recovered
attr(clade,"def_type")<-"edge"

}
return(clade)

}else{
attr(minclade,"def_type")<-"node"
minclade}}

}}##

