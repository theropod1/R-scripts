##function datamatch()
#'
#' Assign values based on an identified variable
#' @param orig_id vector of identifiers for which to look up data
#' @param add_id vector of identifiers for data vector If NULL (default), names of add_data are used
#' @param add_data vector of data from which to look up values for each orig_id
#' @param case logical indicating whether to be case-sensitive (defaults to FALSE)
#' @return A vector of same length as orig_id containing values from add_data (of NA, where orig_id[i] is not in add_id)
#' @export datamatch
#' @importFrom stringr str_to_lower


datamatch<-function(orig_id,add_id=NULL,add_data,case=FALSE){
if(is.null(add_id)) add_id<-names(add_data)

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

##function bbmean()
#'
#' Compute mean between only the min and max values in a vector
#' @param x a vector of numbers
#' @return the mean between the min and max values of x
#' @export bbmean

bbmean<-function(x){mean(c(max(x,na.rm=T),min(x, na.rm=T)))}
