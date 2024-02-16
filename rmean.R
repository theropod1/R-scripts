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
#rolling mean function that takes means for a vector x based on the values in a second vector, var. All values within the distance of parameter plusminus are taken into account. If weighting==TRUE, the values are weighted based on a linear interpolation based on the distance in var.

rmeana<-function(x, var, plusminus=5, weighting=FALSE,weightdiff=0){
x_<-rep(NA,length(x))

for(i in 1:length(x)){

indices<-which(abs(as.numeric(var-var[i]))<=plusminus)

if(weighting==TRUE){
weights<-abs(as.numeric(var[indices]-var[i]))

x_[i]<-weighted.mean(x[indices],w=plusminus+weightdiff-weights)

}else{
x_[i]<-mean(x[indices])}
}

return(x_)
}
##
