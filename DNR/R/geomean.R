##function geomean()
#' calculate the geometric mean for a numeric() vector x.
#' @param x vector of numbers for which to calculate the geometric mean
#' @param na.rm Logical indicating whether to ignore NA values (defaults to TRUE)
#' @return A single numeric containing the geometric mean of x
#' @export geomean
#' @examples
#' geomean(c(1,2,3))

geomean<-function(x,na.rm=TRUE){
if(!is.numeric(x)){stop(paste("input must be numeric, but is of class",class(x)))}
if(na.rm==TRUE){x<-x[!is.na(x)]}
prod(x)^(1/length(x))
}
##
