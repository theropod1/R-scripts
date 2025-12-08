##abind()
#' Concatenate 3D arrays of matching dimensions into a single, larger 3D array
#' @param ... any number of arrays to concatenate
#' @return an array containing all the arrays in ... whose first and second dimensions match those of the first array
#' @export abind


abind<-function(...){
list(...)->args
#only keep elements in ... that are arrays
list()->args_
for(i in 1:length(args)) if(is.array(args[[i]]) | is.matrix(args[[i]])) args_<-c(args_,list(args[[i]]))

#save dimensions for each array
dim1<-numeric()
dim2<-numeric()
dim3<-numeric()
for(i in 1:length(args_)){
as.numeric(dim(args_[[i]]))->dimensions
print(dimensions)
#message(length(dimensions))
dimensions[1]->dim1[i]
dimensions[2]->dim2[i]
if(length(dimensions)<3){1->dim3[i]
message("array has no third dimension→dim3=1")
}else{dimensions[3]->dim3[i]}
}

#determine which arrays have non-matching dimensions and drop those
which(dim1!=dim1[1])->drop1
which(dim2!=dim2[1])->drop2
intersect(drop1,drop2)->drop
which(!(c(1:length(args_)%in%drop)))->keep
message("keeping arrays: ",listout(keep),", dropping arrays: ",listout(drop))
#message("dim3:", paste(dim3, collapse=", "))

dim3[keep]->dim3_#updated third dimensions, with dropped arrays removed

message("making array with dim()==", paste(dim1[1],dim2[1],sum(dim3_),sep=", "))
#empty array
array(NA, dim=c(dim1[1],dim2[1],sum(dim3_)))->out_arr

##concatenation
message("writing array",1,"to", 1,":",dim3_[1])
message("dimensions of 1st array: ", paste(dim(args_[[1]]),collapse=", "))

out_arr[,,1:dim3_[1]]<-args_[[1]] #add first array

for(i in 2:length(args_)){ #now loop over all arrays in ...
sum(dim3_[1:(i-1)])+1->lwr
sum(dim3_[1:(i)])->upr
message("writing array",i,"to", lwr,":",upr)
out_arr[,,lwr:upr]<-args_[[i]]
}

return(out_arr)
}#

##listout()
#' return the contents of a vector as a legible string
#' @param x any vector
#' @param quotes Logical indicating whether to insert single quotes around elements of x
#' @param sep separator to place between contents of x
#' @return a character string containing the elements of x, separated by sep
#' @export listout
listout<-function(x,quotes=TRUE, sep=", "){
if(is.numeric(x) | quotes==FALSE){
x_<-x[1]
}else{
x_<-paste0("'",x[1],"'")}

if(length(x)>1){
for(i in 2:length(x)){
if(is.numeric(x)){
paste0(x_,sep,x[i])->x_
}else{
paste0(x_,sep, "'",x[i],"'")->x_}
}}

if(quotes==FALSE | is.numeric(x)){
gsub("'","",x_)->x_
#gsub(" ","",x_)->x_
}

return(x_)
}#

