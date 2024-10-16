##function arbl()
#' Assigns arbitrary uniform branch lengths to a phylogenetic tree 
#' @param phy a phylogeny
#' @param val value to repeat for branch lengths
#' @return an object of class phylo with all branch lengths equal to val
#' @export arbl

    arbl<-function(phy,val=0.1){
    if(!inherits(phy,"phylo")){error("object not of class phylo")}else{
    if(!is.null(phy$edge.length)){warning("Phylogeny already has edge-lengths, overwriting them now.")}
    
    l<-length(phy$tip.label)+phy$Nnode-1    
    
    phy$edge.length<-rep(val,l)
	return(phy)

    }}
