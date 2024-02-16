##function: 
#calculates the standard error and optionally the confidence interval for a population proportion given a sample proportion for a sample of size n, using the normal approximation of the binomial

se.prop<-function(prop,n,margin=TRUE,CI=FALSE,level=0.95){
p<-prop
se<-sqrt(p*(1-p)/n)

if(CI==TRUE){#add confidence interval and error margin
list()->out
out$SE<-se

out$SE*qnorm(c((1-level)/2,1-(1-level)/2))+p->out$CI
names(out$CI)<-c(paste0("lwr_",(1-level)/2),paste0("lwr_",1-(1-level)/2))

out$margin<-abs(diff(range(out$CI)))*c(-1,1)
return(out)
}else if(margin==FALSE){
#if(interactive()==TRUE){print(paste("standard error(s) of the proportion:"))}
return(se)}else{
qnorm(c(1-(1-level)/2))*se->margins
#if(interactive()==TRUE){print(paste0("error margin(s) at ", level,":"))}
return(margins)
}
}
##




#same thing, different syntax:
naprox<-function(prop,n,sig.level=0.05,CI=T, percent=F){

if(CI==F){return(sqrt(prop*(1-prop)/n))#return only standard error

}else{
prop+qnorm(1-sig.level/2)*sqrt(prop*(1-prop)/n)*c(-1,1)->res

names(res)<-c(paste0("p_",sig.level/2),paste0("p_",1-sig.level/2))

return(res)}#return confidence interval

}
