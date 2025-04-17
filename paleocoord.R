paleocoord<-function(long,lat,time,verbose=T, return_ctrl=F, data=NULL,model="GOLONKA",return_complete_cases=FALSE, online=TRUE){ #call to gplates API
out<-matrix(nrow=length(long), ncol=2)
colnames(out)<-c("paleolng","paleolat")

ctrl<-data.frame(id=NA,paleolong=NA,paleolat=NA)
if(!is.null(data)) data->ctrl

if(length(time)<length(long)) rep(time,length(long))[1:length(long)]->time
url1<-"http://gws.gplates.org/reconstruct/reconstruct_points/?points="

for(i in 1:length(long)){
ctrl0<-paste(long[i],lat[i],time[i],sep=",")#compare ID, if coordinates have been downloaded before skip API call
if((ctrl0%in%ctrl$id)){ 
as.numeric(ctrl[ctrl$id==ctrl0,c("paleolong","paleolat")])->coo

if(!is.na(coo[1])) if(coo[1]>180) coo[1]<-NA
if(!is.na(coo[2])) if(coo[2]>90) coo[2]<-NA

}else{
if(online){
if(!is.na(long[i]) & !is.na(lat[i]) & !is.na(time[i])){
url<-paste0(url1,long[i],",",lat[i],"&time=",time[i],"&model=",model)
message("calling gplates API (",url,")")
jsonlite::fromJSON(url)->jso
jso$coordinates->coo
if(!is.na(coo[1])) if(coo[1]>180) coo[1]<-NA
if(!is.na(coo[2])) if(coo[2]>90) coo[2]<-NA
ctrl0<-c(ctrl0,coo)
ctrl<-rbind(ctrl,ctrl0)
#if(verbose) print(ctrl)
}}else{
coo<-c(NA,NA)
ctrl0<-c(ctrl0,coo)
ctrl<-rbind(ctrl,ctrl0)
}}
coo->out[i,]
if(verbose) message(i," of",length(long),": ",long[i],"/",lat[i]," → ",coo[1],",",coo[2])
}

if(return_ctrl==FALSE) return(out)

if(return_complete_cases) ctrl<-ctrl[complete.cases(ctrl),]
if(return_ctrl==TRUE) return(ctrl)
}
