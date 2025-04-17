tapplyW<-function(x,cat,w,stat=weighted.mean,weights_arg="w",v=F,...){
list(...)->args
out<-rep(NA,length(levels(factor(cat))))

names(out)<-levels(factor(cat))
for(i in 1:length(out)){
which(cat==levels(factor(cat))[i])->indices
#stat(x[indices],w=w[indices])->out[i]
args[[weights_arg]]<-w[indices]

if(v) print(args)
if(v) print(x[indices])
do.call(stat,c(list(x[indices]),args))->out[i]
}
return(out)
}