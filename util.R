## multigrep()
#' search for multiple values and return a vector of entries matching them
#' @param matches vector of search patterns
#' @param vector vector in which to search
#' @return indices of entries in vector that match one or more patterns in matches
#' @export multigrep
multigrep<-function(matches, vector){
out<-numeric()
for(i in matches){out<-c(out, grep(i,vector))}
return(out)
}
##

##cc()
#' like c(), but always converts the final result to character()
#' @param ... arguments to pass on to c
#' @return character vector of c(...)
#' @export cc

cc<-function(...){as.character(c(...))}##



##freplace()
#' replace several values of x with replacements
#' @param x vector in which to perform substitution
#' @param values vector of values to substitute
#' @param replacements vector of substitutions to replace values with (defaults to NA)
#' @return vector of same length as x with values replaced by replacements
#' @export freplace

freplace<-function(x,values,replacements=NA){

if(length(values)>length(replacements)) rep(replacements,length(values))[1:length(values)]

for(i in 1:length(x)){if(x[i]%in%values){
for(j in 1:length(values)){if(x[i]==values[j]) x[i]<-replacements[j]}
	}}
return(x)
}
##


## multilgrep()
#' search for multiple values and return logical vector for presence or absence of the different entries in pattern
#' @param matches vector of search patterns
#' @param vector vector in which to search
#' @return indices of entried in vector that match one or more patterns in matches
#' @export multilgrep
multilgrep<-function(matches, vector){
out<-logical()
for(i in matches){
out<-c(out, ifelse(any(grepl(i,vector)),TRUE,FALSE))
}
return(out)
}
##

##multigsub()
#' replace several character strings in a vector with replacements
#' @param x vector in which to perform substitution
#' @param patterns vector of patterns to substitute
#' @param replacements vector of substitutions to replace values with (defaults to an empty string, i.e. deleting characters in question)
#' @param ... additional arguments to pass on to gsub
#' @return vector of same length as x with strings in values replaced by replacements
#' @export multigsub

multigsub<-function(x,patterns,replacements="",...){

if(length(patterns)>length(replacements)) rep(replacements,length(patterns))[1:length(patterns)]->replacements

for(j in 1:length(patterns)) x<-gsub(patterns[j],replacements[j],x,...)

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

##function plotr()
#' return the width or height of the opened plotting device
#' @param axis what axis of the plot to return range or span for
#' @param range logical indicating whether to retusn
#' @return the width or height of the plot as a numeric of length 1, or the range on the specified axis as a numeric of length 2
#' @export plotr

plotr<-function(axis="x",range=FALSE){
if(range){
if(axis=="x"){
range(par("usr")[1:2])
}else{
range(par("usr")[3:4])
}

}else{
if(axis=="x"){
abs(diff(range(par("usr")[1:2])))
}else{
abs(diff(range(par("usr")[3:4])))
}

}
}

##NNA()
#' remove NA values from a vector
#' @param x vector from which NAs are to be removed
#' @param replace Either FALSE or a numeric value to substitute for NA in the output data
#' @return a vector consisting of all elements of x in the same order, with NAs removed or substituted
#' @export NNA
NNA<-function(x,replace=FALSE){ 
x[!is.na(x)]
}


##same()
#' test if any elements in a vector of two or more entries are identical
#' @param x vector on which to perform test
#' @return a logical indicating whether any elements of x are identical
#' @export same
same<-function(x){
if(length(unique(x))==1) return(TRUE)
else return(FALSE)
}

##between()
#' test if x lies between a set of values
#' @param x single numeric
#' @param between numeric vector to compare x to
#' @return a logical indicating whether x lies within the range of between
#' @export between
between<-function(x,between) x<=max(between) & x>=min(between)

##fls()
#' return a vector with file names in current working directory
#' @param pattern optional pattern to filter for in file names that are returned
#' @param ... options to pass on to grep for filtering
#' @return a character vector of file names that macth pattern
#' @export fls
fls<-function(pattern=NULL,...){
list.files()->l
if(!is.null(pattern)) return(l[grep(pattern,l,...)])
if(is.null(pattern)) return(l)

}

