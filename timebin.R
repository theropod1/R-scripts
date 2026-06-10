timebin<-function(x,strat=timescale$V2,lower=timescale$V4,upper=timescale$V5){
  message("you are running a deprecated version of timebin, see newer version in file timescale_base.R at https://raw.githubusercontent.com/theropod1/R-scripts/refs/heads/main/timescale_base.R")
y<-rep(NA,length(x))

for(i in 1:length(x)){
y_<-strat[lower>=x[i] & upper<=x[i]]
if(length(y_)==1) y_->y[i]
if(length(y_)>1) y_[1]->y[i]
if(length(y_)==0) NA->y[i]
}
return(y)
}
