#this file contains utility functions for reading and manipulating phylogenetic trees and other output of tnt, specifically converting trees from tnt to newick or ape::phylo-class formats (and back) and reading character matrices

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

phylo2nwk_ <- function(tree) {
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
    
    if (!is.null(tree$node.label)) { #preserve internal node labels if present
    node_label <- tree$node.label[node - n_tips]
    if (!is.na(node_label)) {
      label <- paste0(label, node_label)
    }
  }
    
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
} #end internal function definition

if(inherits(tree,"phylo")) phylo2nwk_(tree) else if (inherits(tree,"multiPhylo")) { 

out_multi<-character()
for(i in 1:length(tree)){
paste0(out_multi,phylo2nwk_(tree[[i]]),"\n")->out_multi
}

return(out_multi)

} else{stop("tree must be phylo or multiPhylo class")}




}##


##function tnt2nwk()
#'convert between tnt and newick formats
#' @param treestring single character string containing tree or filename from which to read tree to convert
#' @param return.phylo whether to return the result as a phylo-class object, default TRUE
#' @param sep.chars vector of separator characters to replace with commas, defaults to NULL, which gets converted to space c(" ") if treestring is in tnt format or comma c(",") if it is in newick format
#' @param replacewith character to replace all instances in sep.char with, defaults to NULL, which gets converted to comma (",") if treestring is in tnt format, space (" ") if it is in newick format
#' @param v verbosity setting
#' @param ... additional arguments to be passed on to gsub
#' @return Converted tree string or phylo-class object
#' @details The function also automatically recognizes a newick string, in which case it converts in the opposite direction to a tnt-readable space-separated notation. in this case, return.phylo is automatically switched to FALSE.
#' @export tnt2nwk
#' @importFrom ape read.tree

tnt2nwk<-function(treestring,return.phylo=TRUE,sep.chars=NULL, replacewith=NULL,v=FALSE,...){

if( is.character(treestring) && file.exists(treestring) ){ readChar(treestring,nchars=file.info(treestring)$size)->treestring
#readLines(treestring)->treestring #would read lines with individual trees separately
if(v) print(treestring)
}

#check for balanced parentheses:
treestring_chars <- strsplit(treestring, "")[[1]]
sum(treestring_chars == "(") == sum(treestring_chars == ")")->para_balanced
if(!para_balanced){
if(sum(treestring_chars == "(") > sum(treestring_chars == ")")) warning(paste("too many left parentheses (",sum(treestring_chars == "("),">",sum(treestring_chars == ")") ))
if(sum(treestring_chars == "(") < sum(treestring_chars == ")")) warning(paste("too many right parentheses )",sum(treestring_chars == "("),"<",sum(treestring_chars == ")") ))
if(return.phylo) warning("no valid parenthetical format, conversion to phylo class not possible")
}

grepl(",", treestring)->is.newick ##is the origin a newick string?

treestring<-gsub("\r","\n",treestring)


## detect and convert
if( is.newick ){ ##newick to tnt

if( is.null(sep.chars) && is.null(replacewith) ){#alter default settings if target is tnt
sep.chars<-c(",")
replacewith<-" "
if(return.phylo & v) message("origin is newick and target is tnt format, deactivating return.phylo")
return.phylo<-FALSE
}

out<-treestring

for(i in sep.chars) out<-gsub(i,replacewith,out,...)
out<-gsub("\\)\\(","\\) \\(",out)

}else{##tnt to newick

if(is.null(sep.chars)) sep.chars<-c(" ")
if(is.null(replacewith)) replacewith<-","
out<-treestring
for(i in sep.chars) out<-gsub(i,replacewith,out,...)
out<-gsub("\\)\\(","\\),\\(",out)

out<-gsub("\\*\n",";\n",out)
out<-gsub("\\*\r\n",";\r\n",out)

out<-gsub("proc-;","",out)

}

#status messages
if(v) print(replacewith)
if(v) print(sep.chars)
if(v) message("para_balanced = ",para_balanced)
if(v) message("return.phylo = ",return.phylo)

##clean up doubles
while(grepl(",,",out)) out<-gsub(",,",",",out)
while(grepl("  ",out)) out<-gsub("  "," ",out)
while(grepl(",\\)",out)) out<-gsub(",\\)","\\)",out)

if(return.phylo & para_balanced) ape::read.tree(text=out)->out
return(out)
}##


##function treenum()
#' convert from a numbered to a named or a named to a numbered phylogeny string based on a TNT character matrix
#' @param treestring single character string containing tree or filename from which to read tree in which to perform replacements
#' @param charmatr character matrix as matrix or file name
#' @param return.phylo Logical indicating whether to return phylogeny or parenthetical string
#' @param force.newick Logical indicating whether to force character-output to conform to newick format, otherwise it is returned in original format

#' @param v verbosity setting
#' @param outfile output file to write converted trees to
#' @return Tree with replacements based on order of taxa in tnt character matrix
#' @export treenum
#' @importFrom TreeTools ReadTntCharacters
#' @importFrom ape read.tree
treenum<-function(treestring, charmatr, return.phylo=FALSE, force.newick=TRUE, v=TRUE, outfile=NULL,...){

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
if(inherits(treestring,"phylo") | inherits(treestring,"multiPhylo")){
treestring->tree0
original_is_newick<-FALSE
}else{
gsub("\n","",treestring)->treestring
#if(v) print(treestring)

if(grepl(",",treestring) & grepl(";",treestring)) original_is_newick<-TRUE else original_is_newick<-FALSE #test if valid newick string seems to be present

if(original_is_newick) tree0 <- ape::read.tree(text=treestring) else tree0 <- tnt2nwk(treestring,return.phylo=TRUE)

}

#diagnostics
if(v & original_is_newick) message("original is newick, using ape::read.tree()") else if(v) message("original is not newick, using tnt2nwk converter")
if(v) message("class of original is ", class(treestring), " and class of read tree is ", class(tree0), " with length ", length(tree0))
if(!inherits(tree0,"phylo") & !inherits(tree0,"multiPhylo")) warning("read tree(s) not in correct phylo or multiPhylo class")


##apply algorithm to convert tree tip labels
if(inherits(tree0, "phylo")){ #single phylo

if(all(is_numeric_like(tree0$tip.label))){
freplace(tree0$tip.label,taxnum,taxnam)->tree0$tip.label
if(v) message("all tip.labels are numeric, converting numbered phylogeny to named phylogeny")}else{
freplace(tree0$tip.label,taxnam,taxnum)->tree0$tip.label
if(v) message("converting named phylogeny to numbered phylogeny")
}

}else if(inherits(tree0, "multiPhylo")) for(i in 1:length(tree0)){ #multiPhylo

if(all(is_numeric_like(tree0[[i]]$tip.label))){
freplace(tree0[[i]]$tip.label,taxnum,taxnam)->tree0[[i]]$tip.label
if(v) message("all tip.labels are numeric, converting numbered phylogeny to named phylogeny")}else{
freplace(tree0[[i]]$tip.label,taxnam,taxnum)->tree0[[i]]$tip.label
if(v) message("converting named phylogeny to numbered phylogeny")
}
}

phylo2nwk(tree0)->tree_nwk ##

if(!original_is_newick & !force.newick) tnt2nwk(tree_nwk,return.phylo=FALSE)->tree1 else tree_nwk->tree1

if(!is.null(outfile) && is.character(outfile)) write(tree1, file=outfile[1])

if(!return.phylo){return(tree1)}else return(tree0)
}##

####TntLabels()
#' read character names and descriptions from a tnt file
#' @param filename a valid file name to read tnt matrix from
#' @param return_matrix logical indicating whether entire matrix (with character labels as attribute "state.labels") should be returned
#' @return a list() object containing one element per character with the character number [1], name [2] and state labels [3ff]
#' importFrom TreeTools ReadTntCharacters
#' @export TntLabels
TntLabels<-function(filename,return_matrix=FALSE){
if( is.character(filename) && file.exists(filename) ) lines <- readLines(filename) else stop("file could not be found")

which(tolower(lines)=="cnames")+1->cstart
which(tolower(lines)==";" & seq_along(lines)>cstart)[1]-1->cend

c(cstart:cend)->keep

lines[keep]->charD

strsplit(charD," ")->out

charN<-character(length(out))
for(i in 1:length(out)){
as.numeric(gsub("{","",out[[i]][1],fixed=TRUE,perl=FALSE))->out[[i]][1]

out[[i]][2]->charN[i]
}
charN->names(out)

if(return_matrix){
TreeTools::ReadTntCharacters(filename)->charmatr0
attr(charmatr0,"state.labels")<-out
names(attr(charmatr0,"state.labels"))->colnames(charmatr0)

return(charmatr0)
}else{return(out)}
}##


##function phylo2wiki()
#' convert from a phylo-class object to a tree in wikipedia-source code
#' @param tree a phylo object
#' @param tip_format optional formatting function to apply to tip.label of tree
#' @param node_format optional formatting function to apply to node.label of tree
#' @param outfile optional output file name to write formatted phylogeny to
#' @return Tree in wikipedia format
#' @export phylo2wiki
#' @examples
#' read.tree(text="((A,B),C);")->t
#' plot(t)
#' nodelabels()
#' tiplabels()
#' phylo2wiki(t)

phylo2wiki <- function(tree, tip_format = NULL, node_format = NULL, outfile = NULL) {
  
  if(!inherits(tree,"phylo") && is.character(tree)) ape::read.tree(text=tree)->tree
  # Ensure correct class
  if (!inherits(tree, "phylo")) {
    stop("Input must be phylo-class or coercible to phylo-class.")
  }
  
  #define formatting macros for labels
    pformats<-list()
   pformats$italics<-function(x) paste0("''", x, "''")
   pformats$linked<-function(x) paste0("[[", x, "]]")
  ##XXX
  
  if(!is.null(tip_format)){  #apply tip label functions
  if(!is.function(tip_format) && is.character(tip_format)){
  match.arg(tolower(tip_format),names(pformats),several.ok=TRUE)->tip_format
  for(i in tip_format){
  tree$tip.label<-pformats[[i]](tree$tip.label)
  }
  }else if(is.function(tip_format)) tree$tip.label<-tip_format(tree$tip.label) 
  }
  
  if("node.label"%in%names(tree) && !is.null(node_format)){ #apply node label functions
  if(!is.function(node_format) && is.character(node_format)){
  match.arg(tolower(node_format),names(pformats),several.ok=TRUE)->node_format
  for(i in node_format){
  tree$node.label[which(tree$node.label!="")]<-pformats[[i]](tree$node.label[which(tree$node.label!="")])
  }
  }else if(is.function(node_format)) tree$node.label<-node_format(tree$node.label) 
  
  }
  
  # Number of tips
  n_tips <- length(tree$tip.label)
  
  # Build adjacency list
  children <- split(tree$edge[, 2], tree$edge[, 1])
  
  # Recursive function to build a Wikipedia clade
  build_clade <- function(node, depth = 0) {
    
    # Get descendants of current node
    desc <- children[[as.character(node)]]
    
    # Indentation
    indent <- function(d) paste0(strrep(" ", 3 * d))
    
    # Build entries for each child
    entries <- character()
    
    for (i in seq_along(desc)) {
      split(tree$edge[, 2], tree$edge[, 1])
      child <- desc[i]
      
      if (child <= n_tips) {
        
        # Tip node
        label <- tree$tip.label[child]
        
        entries <- c(
          entries,
          paste0(indent(depth + 1), "|", i, "=", label)
        )
        
      } else {
        
        # Internal node
        node_label <- NULL
        
        if (!is.null(tree$node.label)) {
          idx <- child - n_tips
          
          if (idx <= length(tree$node.label)) {
            node_label <- tree$node.label[idx]
          }
        }
        
        # Add internal node label if present
        if (!is.null(node_label) && !is.na(node_label) && nzchar(node_label)) {
          entries <- c(
            entries,
            paste0(indent(depth + 1), "|label", i, "=", node_label)
          )
        }
        
        # Recursively build nested clade
        entries <- c(
          entries,
          paste0(indent(depth + 1), "|", i, "=",
                 build_clade(child, depth + 1))
        )
      }
    }
    
    # Assemble clade
    paste0(
      "{{clade\n",
      paste(entries, collapse = "\n"),
      "\n", indent(depth), "}}"
    )
  }
  
  # Find root node
  root <- setdiff(tree$edge[, 1], tree$edge[, 2])[1]
  
  # Generate final Wikipedia clade
  build_clade(root)->out
  
  if(!is.null(outfile)){
  write(out,file=outfile) 
  message("written output to file ", outfile)}
  return(out)
}
