##function ages_sptab()
#' convert between an age matrix and a taxon-range table (as used by paleoDiv for diversity estimates)
#' @param x either an age matrix or a taxon-range table in the respective formats. An age matrix contains two collumns named "FAD" and "LAD" (as used by strap for tree calibration), and the taxon names as row names. A taxon-range table is a data.frame() that contains the same information in columns named "max" and "min", and another column named "tna" containing the taxon name (and might contain any number of other columns with additional information)
#' @param subset Optional subset of taxa in x to include in output
#' @param tax Optional higher level taxon names to include in the output taxon-range table. If NA (default) the collumn is included, but filled with the current time stamp as a placeholder. If NULL, the collumn is not included in the output.
#' @return converts an age matrix to a taxon-range table (data.frame) or vice versa
#' @export ages_sptab

ages_sptab<-function(x, subset=NULL, tax=NA){
colnames(x)->cn

if("FAD" %in% cn & "LAD" %in% cn){##if input is age matrix
out<-data.frame(tna=rownames(x), max=x[,"FAD"], min=x[,"LAD"])
out$ma<-(out$max+out$min)/2


if(is.null(tax)){
}else if(is.na(tax)){
tstamp<-gsub(":","-",gsub(" ","-",as.character(Sys.time())))
out$tax<-paste0("unspecified_",tstamp)
}else{
out$tax<-rep(tax,nrow(out))
}


if(!is.null(subset)){
out[which(out$tna%in%subset),]->out
}

return(out)

}else if("max" %in% cn & "min" %in% cn & "tna" %in% cn){##if input is taxon-range table
out<-data.frame(FAD=x[,"max"], LAD=x[,"min"])
rownames(out)<-x[,"tna"]

if(!is.null(subset)){
out[which(rownames(out)%in%subset),]->out
}

as.matrix(out)->out

return(out)

}else{
stop("could not find valid column names")
}

}
