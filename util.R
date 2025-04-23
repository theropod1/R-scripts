## multigrep()
#' search for multiple values and return a vector of entries matching them
#' @param matches vector of search patterns
#' @param vector vector in which to search
#' @return indices of entried in vector that match one or more patterns in matches
#' @export multigrep
multigrep<-function(matches, vector){
out<-numeric()
for(i in matches){out<-c(out, grep(i,vector))}
return(out)
}
##

##freplace()
#' replace several values of x with replacements
#' @param x vector in which to perform substitution
#' @param values vector of values to substitute
#' @param replacements vector of substitutions to replace values with
#' @return vector of same length as x with values replaced by replacements
#' @export freplace

freplace<-function(x,values,replacements){
for(i in 1:length(x)){if(x[i]%in%values){
for(j in 1:length(values)){if(x[i]==values[j]) x[i]<-replacements[j]}
	}}
return(x)
}
##

##M()

#' Calculate a na-removing mean of any given number of values
#' @param ... numeric arguments to pass on to mean(c(...))
#' @return single numeric with mean of numbers given in ...
#' @details just a shortcut for mean(c(...),na.rm=TRUE)
#' @export M

M<-function(...) mean(c(...),na.rm=TRUE)#shortcut for mean
##

##AR()
#' find the aspect ratio of the current plot
#' @return single numeric with current plot aspect ratio (number of units of y axis per inch / number of units of x axis per inch)
#' @details similar to DescTools::Asp()
#' @export AR

AR<-function() par("pin")[1]/diff(par("usr")[1:2])/(par("pin")[2]/diff(par("usr")[3:4]))
##
