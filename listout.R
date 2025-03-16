listout<-function(x,quotes=T, sep=", "){
if(is.numeric(x)){
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

if(quotes==F | is.numeric(x)){
gsub("'","",x_)->x_
gsub(" ","",x_)->x_
}

return(x_)
}
