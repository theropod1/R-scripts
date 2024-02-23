##function: 
#simple rolling mean function
rmean<-function(x, width=11){
x_<-rep(NA,length(x))

for(i in round(width/2):length(x)){
i-(round(width/2)-1)->lwr
i+(round(width/2)-1)->upr

j<-seq(lwr,upr,1)
x_[i]<-mean(x[j])
}

return(x_)
}
##



##function: 
#rolling mean function that takes a rolling mean for a vector y0 based on the values in a second vector, x0, for either every entry in x0 and y0, or every entry for x1, if provided. All values within the distance of parameter plusminus are taken into account. If weighting==TRUE, the values are weighted based on a linear interpolation based on the distance in x.

rmeana<-function(x0, y0, x1=NULL, plusminus=5, weighting=FALSE,weightdiff=0){

if(is.null(x1)){
x_<-x0
}else{
x_<-x1}

y_<-rep(NA,length(x_))

for(i in 1:length(x_)){

indices<-which(abs(as.numeric(x0-x_[i]))<=plusminus)

if(weighting==TRUE){

weights<-abs(as.numeric(x0[indices]-x_[i]))

y_[i]<-weighted.mean(y0[indices],w=plusminus+weightdiff-weights)

}else{
y_[i]<-mean(y0[indices])}
}

return(y_)
}
##
