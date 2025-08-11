## closest()
#' compare a range of values to a ref value and determine which is the closest in relative and absolute terms
#' @param x numeric vector of values
#' @param ref ref value to compare to (defaults to 0)
#' @param nam names for different elements of x (defaults to pulling its value from names(x)). If NULL, indices are returned instead of names
#' @param return what to return, defaults to returning full table with all values and their percent or absolute deviations from ref, if "percent" or "relative", the value with the lowest relative deviation is returned, if "absolute" or "difference", the lowest based absolute difference from x is returned.
#' @return the name or index of the value in x matching ref most closely based on the chosen metric, or the full table of results
#' @export closest
#' @examples
#' closest(c(200/100,200/94),400/205,return="%")
closest<-function(x,ref=0,nam=names(x), return="full"){
if(is.null(nam)) nam<-c(1:length(x))

x-ref->x__
if(ref!=0) x__/ref*100->x_
if(ref==0) x__->x_

message(nam[which(abs(x_)==min(abs(x_)))], " is the closest proportionally, percentage deviation ", x_[which(abs(x_)==min(abs(x_)))],"%")
message(nam[which(abs(x__)==min(abs(x__)))], " is the closest in absolute terms, absolute deviation ", x__[which(abs(x__)==min(abs(x__)))])

if(return=="full") return(data.frame(names=nam, x, ref, data.frame(percent_difference=x_, absolute_difference=x__)))
if(return%in%c("%","rel","relative","percent")) return(nam[which(abs(x_)==min(abs(x_)))])
if(return%in%c("abs","difference","absolute")) return(nam[which(abs(x__)==min(abs(x__)))])
}
