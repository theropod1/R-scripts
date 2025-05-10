leading0<-function(x,prefix="",suffix=""){

max(nchar(x),na.rm=TRUE)->mx

x_<-x
for(i in 1:length(x)){
if(!is.na(x[i])){
len<-mx-nchar(x[i])
lead<-listout(rep(0,len),sep="")
if(is.na(lead)) lead<-""

x_[i]<-paste0(lead,x[i])
}
}
x_<-paste0(prefix,x_,suffix)
return(x_)
}

#prefix,
