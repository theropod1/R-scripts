##convert from a numbered to a named or a named to a numbered phylogeny string based on a TNT character matrix
#' @param treestring single character string containing tree or filename from which to read tree in which to perform replacements
#' @param charmatr character matrix
#' @param sep.chars separator characters in treestring to remove to find
#' @param v verbosity setting
#' @return Tree string with replacements from character matrix
#' @export treenum
#' @importFrom TreeTools ReadTntCharacters
treenum<-function(treestring, charmatr, sep.chars=c(" ",","),v=FALSE){

if( is.character(treestring) && treestring%in%list.files() ) readChar(treestring,nchars=file.info(treestring)$size)->treestring
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
