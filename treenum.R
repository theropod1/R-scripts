##function treenum()
#' convert from a numbered to a named or a named to a numbered phylogeny string based on a TNT character matrix
#' @param treestring single character string containing tree or filename from which to read tree in which to perform replacements
#' @param charmatr character matrix
#' @param sep.chars separator characters in treestring to remove to find
#' @param v verbosity setting
#' @return Tree string with replacements from character matrix
#' @export treenum
#' @importFrom TreeTools ReadTntCharacters
treenum<-function(treestring, charmatr, sep.chars=c(" ",","),v=FALSE){

if( is.character(treestring) && file.exists(treestring) ) readChar(treestring,nchars=file.info(treestring)$size)->treestring
if( is.character(charmatr) && length(charmatr)==1 && charmatr%in%list.files() ) TreeTools::ReadTntCharacters(charmatr)->charmatr

#clean up taxnames
gsub("\\(","",treestring)->t2
gsub("\\)","",t2)->t2
gsub(";","",t2)->t2
for(i in sep.chars) gsub(i," ",t2)->t2
while(grepl("  ",t2)==TRUE) gsub("  "," ",t2)->t2
words(t2)->t2

#names and length
length(rownames(charmatr))->l
c(0:(l-1))->taxnum
rownames(charmatr)->taxnam

#
if(v){
print(t2)
print(cbind(taxnum,taxnam))
}

is_numeric_like<-function(x){
as.numeric(x)->x_
if(any(!is.na(x_))) return(TRUE) else return(FALSE)
}

##do replacements
if(!is_numeric_like(t2)){
for(i in t2) sub(i,taxnum[which(taxnam==i)],treestring)->treestring
}else{
for(i in t2) sub(i,taxnam[which(taxnum==i)],treestring)->treestring
}

return(treestring)
}##


##function tnt2nwk()
#'convert from a tree in tnt format to a clean newick tree string or phylo-class object
#' @param treestring single character string containing tree or filename from which to read tree to convert
#' @param return.phylo whether to return the result as a phylo-class object, default TRUE
#' @param sep.chars vector of separator characters to replace with commas, defaults to space (c(" "))
#' @param ... additional arguments to be passed on to gsub
#' @return Converted tree string or phylo-class object
#' @details The function also automatically recognizes a newick string, in which case it converts in the opposite direction to a tnt-readable space-separated notation. in this case, return.phylo is automatically switched to FALSE.
#' @export tnt2nwk
#' @importFrom ape read.tree

tnt2nwk<-function(treestring,return.phylo=TRUE,sep.chars=c(" "), replacewith=",",...){
if( is.character(treestring) && file.exists(treestring) ) readChar(treestring,nchars=file.info(treestring)$size)->treestring

if( (grepl(",", treestring) && !grepl(" ", treestring)) ){ #potentially convert newick to tnt

if( is.null(sep.chars) && is.null(replacewith) ){#alter default settings
sep.chars<-c(",")
replacewith<-" "
return.phylo<-FALSE
}

out<-treestring
for(i in sep.chars) out<-gsub(i,replacewith,out,...)
out<-gsub(")(",") (",out,fixed=TRUE)

}else{#convert tnt to newick

if(is.null(sep.chars)) sep.chars<-c(" ")
if(is.null(sep.chars)) replacewith<-","

out<-treestring
for(i in sep.chars) out<-gsub(i,replacewith,out,...)

out<-gsub(")(","),(",out,fixed=TRUE)
}

out<-gsub(",,",",",out,fixed=TRUE)
out<-gsub("  "," ",out,fixed=TRUE)


if(return.phylo) ape::read.tree(text=out)->out
return(out)
}##
