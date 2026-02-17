##function phylo2nwk()
#' convert from a phylo-class object to a newick format string
#' @param tree a phylo object
#' @return Tree in newick format
#' @export phylo2nwk
#' @examples
#' read.tree(text="((A,B),C);")->t
#' plot(t)
#' nodelabels()
#' tiplabels()
#' phylo2nwk(t)

phylo2nwk <- function(tree) {
  # Ensure correct class
  if (!inherits(tree, "phylo")) {
    stop("Input must be a phylo object.")
  }
  
  # Number of tips
  n_tips <- length(tree$tip.label)
  
  # Build adjacency list from edge matrix
  children <- split(tree$edge[, 2], tree$edge[, 1])
  
  # Recursive function to build Newick string
  build_subtree <- function(node) {
    if (node <= n_tips) {
      # Tip node
      label <- tree$tip.label[node]
    } else {
      # Internal node
      desc <- children[[as.character(node)]]
      label <- paste0("(", paste(sapply(desc, build_subtree), collapse=","), ")") #this builds a subtree for each child element and pastes the label
    }
    
    # Add branch length if present
    edge_index <- which(tree$edge[, 2] == node)
    if (!is.null(tree$edge.length) && length(edge_index) == 1) {
      bl <- tree$edge.length[edge_index]
      label <- paste0(label, ":", bl)
    }
    
    return(label)
  }
  
  # Root node is a parent not appearing as child
  root <- setdiff(tree$edge[,1], tree$edge[,2])[1]
  
  paste0(build_subtree(root), ";")
}##


##function tnt2nwk()
#'convert between tnt and newick formats
#' @param treestring single character string containing tree or filename from which to read tree to convert
#' @param return.phylo whether to return the result as a phylo-class object, default TRUE
#' @param sep.chars vector of separator characters to replace with commas, defaults to NULL, which gets converted to space (c(" ")) if treestring is in tnt format or comma c(",") if it is in newick format
#' @param replacewith character to replace all instances in sep.char with, defaults to NULL, which gets converted to comma (",") if treestring is in tnt format, space (" ") if it is in newick format
#' @param v verbosity setting
#' @param ... additional arguments to be passed on to gsub
#' @return Converted tree string or phylo-class object
#' @details The function also automatically recognizes a newick string, in which case it converts in the opposite direction to a tnt-readable space-separated notation. in this case, return.phylo is automatically switched to FALSE.
#' @export tnt2nwk
#' @importFrom ape read.tree

tnt2nwk<-function(treestring,return.phylo=TRUE,sep.chars=NULL, replacewith=NULL,v=FALSE,...){
if( is.character(treestring) && file.exists(treestring) ) readChar(treestring,nchars=file.info(treestring)$size)->treestring

if( (grepl(",", treestring)) ){ #potentially convert newick to tnt

if( is.null(sep.chars) && is.null(replacewith) ){#alter default settings
sep.chars<-c(",")
replacewith<-" "
return.phylo<-FALSE
}

out<-treestring
for(i in sep.chars) out<-gsub(i,replacewith,out,...)
out<-gsub("\\)\\(","\\) \\(",out)

}else{#convert tnt to newick

if(is.null(sep.chars)) sep.chars<-c(" ")
if(is.null(replacewith)) replacewith<-","
if(v) print(replacewith)
if(v) print(sep.chars)

out<-treestring
for(i in sep.chars) out<-gsub(i,replacewith,out,...)
out<-gsub("\\)\\(","\\),\\(",out)
}

while(grepl(",,",out)) out<-gsub(",,",",",out)
while(grepl("  ",out)) out<-gsub("  "," ",out)
while(grepl(",\\)",out)) out<-gsub(",\\)","\\)",out)



if(return.phylo) ape::read.tree(text=out)->out
return(out)
}##




##function treenum()
#' convert from a numbered to a named or a named to a numbered phylogeny string based on a TNT character matrix
#' @param treestring single character string containing tree or filename from which to read tree in which to perform replacements
#' @param charmatr character matrix as matrix or file name
#' @param return.phylo Logical indicating whether to return phylogeny or parenthetical string
#' @param v verbosity setting
#' @return Tree with replacements based on order of taxa in tnt character matrix
#' @export treenum
#' @importFrom TreeTools ReadTntCharacters
#' @importFrom ape read.tree
treenum<-function(treestring, charmatr,return.phylo=FALSE,v=TRUE,...){

is_numeric_like<-function(x){
as.numeric(x)->x_
if(any(!is.na(x_))) return(TRUE) else return(FALSE)
}


freplace<-function(x,values,replacements=NA){

if(length(values)>length(replacements)) rep(replacements,length(values))[1:length(values)]

for(j in 1:length(values)){if(values[j]%in%x) x[x==values[j]]<-replacements[j]}
return(x)
}

if( is.character(treestring) && file.exists(treestring) ) readChar(treestring,nchars=file.info(treestring)$size)->treestring

if( is.character(charmatr) && length(charmatr)==1 && file.exists(charmatr) ) TreeTools::ReadTntCharacters(charmatr)->charmatr

#make equivalence table
nrow(charmatr)->l
c(0:(l-1))->taxnum
row.names(charmatr)->taxnam
names(taxnum)<-taxnam
names(taxnam)<-taxnum

##see what kind of tree we have
if(inherits(treestring,"phylo")){
treestring->tree0
original_is_newick<-FALSE
}else{

original_is_newick<-TRUE

tree0 <- tryCatch({
        ape::read.tree(text=treestring)
    }, error = function(e) {
    original_is_newick<<-FALSE
if(v) message("tree not in newick format, trying tnt2nwk()")
return(tnt2nwk(treestring,return.phylo=TRUE))

    }, warning = function(w) {
        original_is_newick<<-FALSE
if(v) message("tree not in newick format, trying tnt2nwk()")
return(tnt2nwk(treestring,return.phylo=TRUE))
    })
    
    if(is.null(tree0$tip.label) || length(tree0$tip.label<2) || !grepl(",",treestring)){tnt2nwk(treestring,return.phylo=TRUE)->tree0
    original_is_newick<-FALSE
    if(v) message("tree not in clean newick format, trying tnt2nwk()")
    }
        if(v) print(original_is_newick)
}
        
##perform replacements
    
if(all(is_numeric_like(tree0$tip.label))){
freplace(tree0$tip.label,taxnum,taxnam)->tree0$tip.label
if(v) message("converting numbered phylogeny to named phylogeny")}else{
freplace(tree0$tip.label,taxnam,taxnum)->tree0$tip.label
if(v) message("converting named phylogeny to numbered phylogeny")
}

phylo2nwk(tree0)->tree_nwk
if(!original_is_newick) tnt2nwk(tree_nwk,return.phylo=FALSE)->tree1 else tree_nwk->tree1

if(!return.phylo){return(tree1)}else return(tree0)

}##
