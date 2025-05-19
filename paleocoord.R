paleocoord<-function(long,lat,time,verbose=TRUE, return_ctrl=FALSE, data=NULL,model="GOLONKA",return_complete_cases=FALSE, online=TRUE){

#main output objects:
out<-matrix(nrow=length(long), ncol=2)
colnames(out)<-c("paleolng","paleolat")

ctrl<-data.frame(id=NA,paleolong=NA,paleolat=NA)
if(!is.null(data)) data->ctrl
#

if(length(time)<length(long)) rep(time,length(long))[1:length(long)]->time

url1<-"http://gws.gplates.org/reconstruct/reconstruct_points/?points="

#loop through entries:
for(i in 1:length(long)){

ctrl0<-paste(long[i],lat[i],time[i],sep=",")#compare ID, if coordinates have been downloaded before, skip API call

if((ctrl0%in%ctrl$id)){
#if already found in crtl, take coordinates from there:
as.numeric(ctrl[which(ctrl$id==ctrl0)[1],c("paleolong","paleolat")])->coo

}else{
if(online){#online lookup

if(!is.na(long[i]) & !is.na(lat[i]) & !is.na(time[i])){

url<-paste0(url1,long[i],",",lat[i],"&time=",time[i],"&model=",model)
message("calling gplates API (",url,")")
#download data from gplates:
jsonlite::fromJSON(url)->jso
jso$coordinates->coo

if(!is.na(coo[1])) if(coo[1]>180) coo[1]<-NA
if(!is.na(coo[2])) if(coo[2]>90) coo[2]<-NA
#if(verbose) print(ctrl)
}

}else{#no online lookup
coo<-c(NA,NA)
}


#make sure NAs also return NAs
if(is.na(long[i]) | is.na(lat[i]) | is.na(time[i])) coo<-c(NA,NA)

ctrl0<-c(ctrl0,coo)
ctrl<-rbind(ctrl,ctrl0)
}

#make absolutely sure NAs also return NAs (in case predefined ctrl for some reason contains values for NA inputs):
if(is.na(long[i]) | is.na(lat[i]) | is.na(time[i])) coo<-c(NA,NA)

coo->out[i,]
if(verbose) message(i," of",length(long),": ",long[i],"/",lat[i]," at ",time[i]," → ",coo[1],",",coo[2])
}

#output
if(return_ctrl==FALSE) return(out)
if(return_complete_cases) ctrl<-ctrl[complete.cases(ctrl),]
if(return_ctrl==TRUE) return(ctrl)
}
