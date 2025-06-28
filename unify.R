##function unify()
#' Unify two vectors of data, filling NAs in one with values from another, if available
#'
#' @param cases0 vector of values to be unified with cases1
#' @param cases1 vector of alternative values to fill NAs in cases0 with
#' @param means logical; should mean be computed for cases where values are present in both cases0 and cases1? Only works if both are numeric
#' @return a single vector of the same length as cases0, with NAs in cases0 filled with corresponding values from cases1, if not NA themselves
#' @export unify

unify<-function(cases0,cases1,means=FALSE){
for(i in 1:length(cases0)){
if(is.na(cases0[i])) cases0[i]<-cases1[i]
if(means==TRUE) cases0[i]<-mean(c(cases0[i],cases1[i]),na.rm=TRUE)
}
return(cases0)
}
