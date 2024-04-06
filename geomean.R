##function: 
#calculate the geometric mean for a numeric() vector x.

geomean<-function(x,na.rm=TRUE){
if(!is.numeric(x)){stop(paste("input must be numeric, but is of class",class(x)))}
if(na.rm==TRUE){x<-x[!is.na(x)]}
prod(x)^(1/length(x))
}

