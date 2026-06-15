phylo_coords <- function(tree,ages=NULL,add.terminal=TRUE, plot=FALSE, type="cladogram", ...) {

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

y<-max(y)-y+1

x_<-tree$root.time-x


if(plot){
if(dev.cur()<2){
plot(0,0,type="n",xlim=rev(range(x_)),ylim=rev(range(y_)),xlab="",ylab="",axes=FALSE)
if(type!="cladogram"){
arrows(x_[tree$edge[,1]], y[tree$edge[,1]], x_[tree$edge[,1]], y[tree$edge[,2]],...)
arrows(x_[tree$edge[,1]], y[tree$edge[,2]], x_[tree$edge[,2]], y[tree$edge[,1]],...)
}else if(type=="cladogram") arrows(x_[tree$edge[,1]], y[tree$edge[,1]], x_[tree$edge[,2]], y[tree$edge[,2]],...)


}
if(type!="cladogram"){ 
arrows(x[tree$edge[,1]], y[tree$edge[,1]], x[tree$edge[,1]], y[tree$edge[,2]],...)
arrows(x[tree$edge[,1]], y[tree$edge[,2]], x[tree$edge[,2]], y[tree$edge[,2]],...)

}else if(type=="cladogram") arrows(x[tree$edge[,1]], y[tree$edge[,1]], x[tree$edge[,2]], y[tree$edge[,2]],...)

}

xx<-data.frame( max=x[tree$edge[,1]], min=x[tree$edge[,2]] )
xx[which(xx[,1]!=xx[,2]),]->xx #drop zero length branches (future-proofing)
tree$root.time-xx->xx_conv
colnames(xx)<-c("min","max")

  list(
    tree=tree,
    x = x,
    x_ma = x_,
    y = y,
    edge = tree$edge,
    xranges = xx,
    xx_ma = xx_conv,
    root.time = tree$root.time
  )
}
