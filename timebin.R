timebin<-function(x,strat=timescale$V2,lower=timescale$V4,upper=timescale$V5){
y<-rep(NA,length(x))

for(i in 1:length(x)){
y_<-strat[lower>=x[i] & upper<=x[i]]
if(length(y_)==1) y_->y[i]
if(length(y_)>1) y_[1]->y[i]
if(length(y_)==0) NA->y[i]
}
return(y)
}
