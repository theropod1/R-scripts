##function:
#assign colours using a desired function (e.g. viridis(), ggcol()) based on factor levels in a variable x
col_asign<-function(x,FUN=ggcol,na=NA,nam=names(x),order=FALSE){
if(!order) unique(x)->lev
if(order) levels(factor(x))->lev

character()->out

for(i in 1:length(x)){

if(is.function(FUN)) out[i]<-FUN(length(lev))[which(lev==x[i])]

if(!is.function(FUN)){
if(x[i] %in% names(FUN)){
out[i]<-FUN[which(names(FUN)==x[i])]
}else{out[i]<-na}}
}
names(out)<-nam

return(out)
}
##
