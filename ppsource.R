#' helper function to automatically generate a citation for phylopic silhouettes
#' @param img list() object containing rphylopic::get_phylopic() outputs for which attribution should be made
#' @param UUID logical whether uuids of silhouettes should be added
#' @param year logical whether years should be added
#' @param sep Separator between cited authors
#' @param return whether to return "raw" dataframe, "concatenated" single generated citation with all attributions or "pasted" vector with all generated citations (default)
#' @details The input to ppsource needs to be a single list() type object containing objects with the attribute named "uuid" giving a valid phylopic uuid for each of them (e.g. output of get_phylopic() from rphylopic)
#' @importFrom rphylopic get_attribution
#' @export ppsource

ppsource<-function(img,UUID=FALSE,year=TRUE, sep="; ", return="citations"){
uuid<-character()

for(i in 1:length(img)){
if(!is.null(attr(img[[i]],"uuid"))){attr(img[[i]],"uuid")->uuid[i]}else{NA->uuid[i]}
}
NNA(uuid)->uuid

rphylopic::get_attribution(uuid = uuid)->refs

out<-matrix(character(),nrow=0,ncol=5)
out<-as.data.frame(out)
colnames(out)<-c("uuid","attribution","yr","license_abbr","license")

#data.frame(attribution=character(length(uuid)),yr=NA,license_abbr=NA, license=NA,uuid=NA)

for(i in uuid){
refs$images[[i]]->refs_

out[i,"uuid"]<-refs_$image_uuid
out[i,"attribution"]<-refs_$attribution
out[i,"yr"]<-YY(as.Date(refs_$created))
out[i,"license_abbr"]<-refs_$license_abbr
out[i,"license"]<-refs_$license


}


out_<-character()
for(i in levels(factor(out$attribution))){
tmp<-out[out$attribution==i,]

yrs<-levels(factor(tmp$yr))
uuids<-levels(factor(tmp$uuid))
license_abbrs<-levels(factor(tmp$license_abbr))

author_<-paste0(i, ifelse(year,paste0(" ", listout(yrs,quotes=FALSE)),"")," – ",listout(license_abbrs,quotes=FALSE), ifelse(UUID,paste0(" (",listout(uuids,quotes=FALSE),")"),""))

out_<-c(out_,author_)

}


pout<-paste0(listout(out_,quotes=FALSE, sep=sep),". Licenses: ", listout(unique(out$license),quotes=FALSE, sep=sep))

#print(pout)

c(out_,"Licenses: ",unique(out$license))->out_
if(return=="raw"){invisible(out)
}else if(return=="concatenated"){
invisible(pout)}else{invisible(out_)}
}
