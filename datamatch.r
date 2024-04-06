datamatch<-function(orig_id,add_id,add_data,case=F){

if(case==F){
stringr::str_to_lower(add_id)->add_id
stringr::str_to_lower(orig_id)->orig_id
}

l<-length(orig_id)
rep(NA,l)->out

for(i in 1:length(add_id)){
which(orig_id==add_id[i])->ind

if(length(ind)>0){
out[ind]<-add_data[i]
}
}

return(out)

}


bbmean<-function(x){mean(c(max(x,na.rm=T),min(x, na.rm=T)))}
