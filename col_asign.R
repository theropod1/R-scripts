##function:
#assign colours using a desired function (e.g. viridis(), ggcol()) based on factor levels in a variable x
col_asign<-function(x,FUN=ggcol,na=NA,nam=names(x),order=FALSE,overview=FALSE){
if(!order) unique(x)->lev
if(order) levels(factor(x))->lev

if(!is.null(na)){lev[is.na(lev)]<-na
x[is.na(x)]<-na}else{
lev[is.na(lev)]<-"<NA>"
x[is.na(x)]<-"<NA>"
}

if(is.function(FUN)) FUN(length(lev))->FUN

if(all(lev%in%names(FUN))) FUN[lev]->FUN else FUN[1:length(lev)]->FUN

numeric()->out

for(i in 1:length(lev)){
out[x==lev[i]]<-FUN[i]
}
names(out)<-nam

if(!overview) return(out)
if(overview){ 
overview<-as.data.frame(cbind(lev,FUN))
return(overview)
}

}
##
