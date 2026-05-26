

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

for(j in 1:length(values)){if(values[j]%in%x) x[x==values[j]]<-replacements[j]}
return(x)
}
##


## multilgrep()
#' search for multiple values and return logical vector for presence or absence of the different entries in pattern
#' @param matches vector of search patterns
#' @param vector vector in which to search
#' @return logical vector with same length as matches giving whether or not element is present in vector
#' @export multilgrep
multilgrep<-function(matches, vector){
out<-logical(length(matches))
for(i in 1:length(matches)){
out[i]<-ifelse(any(grepl(matches[i],vector)),TRUE,FALSE)
}
return(out)
}
##


## multigrepl()
#' return logical vector giving presence of matches in vector
#' @param matches vector of search patterns
#' @param vector vector in which to search
#' @return indices of entried in vector that match one or more patterns in matches
#' @export multilgrep
multigrepl<-function(matches, vector){
out<-logical(length(vector))
for(i in 1:length(out)){
out[i]<-any(multilgrep(matches,vector[i]))
}
return(out)
}
##

## multigrep()
#' search for multiple values and return a vector of entries matching them
#' @param matches vector of search patterns
#' @param vector vector in which to search
#' @return indices of entries in vector that match one or more patterns in matches
#' @export multigrep
multigrep<-function(matches, vector){
#out<-numeric()
#for(i in matches){out<-c(out, grep(i,vector))}
#return(out)
multigrepl(matches,vector)->out
which(out==TRUE)
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


##words()
#' subdivide string into words and return a vector
#' @param x character string
#' @param sep word separator character string
#' @return character vector of words in x
#' @export words
words<-function(x, sep=" "){#converts words in a string to a vector
strsplit(x, sep)[[1]]
}##


##word()
#' return a specific word from each in a vector of character strings
#' @param x character vector
#' @param sep word separator in the strings in character vector
#' @param n which word to return from each string, can be a numeric or "last" (default)
#' @return character vector of nth word in each string in x
#' @export word
word<-function(x, sep=" ", n="last",...){#extracts word n (or "last" for last word of each) from a string.

if(length(sep)==1 && length(n)==1){

s<-function(x, separator, wn,...){
spl<-strsplit(x, sep, ...)[[1]]
if(n=="last") n_<-length(spl) else n_<-n
return(spl[as.numeric(n_)])
}

return(sapply(X=x, FUN=s, separator=sep, wn=n,...))

}else{
if(length(n)<length(x)) rep(n,length(x))->n
if(length(sep)<length(x)) rep(sep,length(x))->sep
out<-numeric()
for(i in 1:length(x)){
strsplit(x[i], sep,...)[[1]]->spl
if(length(spl)==1){out[i]<-x[i]}else{
n[i]->n_
if(n[i]=="last") n_<-length(spl)
strsplit(x[i], sep[i])[[1]]->t
t[as.numeric(n_)]->out[i]
}
}
return(out)

}


}##

##italicize()
#' italicize each string in a character vector
#' @param text character vector
#' @param fun name of formatting function to apply, default to "italic"
#' @return character vector with each string italicize
#' @export italicize
italicize<-function(text,fun="italic"){
parse(text = paste0(fun,"('", text, "')"))
}

##capitalize()
#' capitalize each string in a character vector
#' @param x character vector
#' @param fun function to apply to first letter, defaults to toupper (could also be tolower, to un-capitalize words)
#' @return character vector with each string capitalized
#' @export capitalize

capitalize <- function(x,fun=toupper) {
  paste0(fun(substring(x, 1, 1)), substring(x, 2))
}

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


##overlaps()
#' test if x overlaps an interval
#' @param x numeric vector of length 2 (longer vectors are coerced using range())
#' @param between numeric vector giving interval to compare x to
#' @return a logical indicating whether x overlaps the range of between
#' @export overlaps
overlaps<-function(x,between){
range(between)->between
out<-FALSE
if(any(between(x,between))) out<-TRUE
if(max(x)>=max(between) & min(x)<=min(between))  out<-TRUE
return(out)
}


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

##exists_()
#' implementation of exists() that works on vectors
#' @param x character vector of object names whose existence to test
#' @param ... other arguments to pass to exists()
#' @return logical vector containing TRUE/FALSE statements whether objects named in x exist in current workspace
#' @export exists_
exists_<-function(x,...){
logical()->out
for(i in 1:length(x)){
exists(x[i],...)->out[i]
}
return(out)
}

##function rd()
#' convenience function based on readChar() for conveniently reading a text string from a file
#' @param filename file name
#' @param ... additional arguments to be passed on to readChar()
#' @return the content of the text file as a single character string
#' @export rd
rd<-function(filename,...){
if( is.character(filename) && file.exists(filename) ){
return(readChar(filename,nchars=file.info(filename)$size,...))}else{stop("not a valid filename")}
}


##function pr()
#' convenience function to return plot range
#' @parameter axis single character giving axis; if x, use x axis, otherwise use y axis
#' @return the width or height of the current plot (based on par("usr")
#' @export pr

pr <- function(axis = "x") {
            if(axis == "x"){
                abs(diff(range(par("usr")[1:2])))}else{
                abs(diff(range(par("usr")[3:4])))}}



##function lebbel()
#' convenience function for adding lettered subplot labels to a plot
#' @param A1 default NULL index number of plot (e.g 1–>A, 2–>B etc.). This number is stored in a global object named A1 and is iteratively increased by 1 each time lebbel is called, unless reset by specifying an explicit value
#' @param v verbosity setting (logical)
#' @param lwr lowercase? (logical)
#' @param o Number of sublabels (e.g. A1, A2…) to be counted towards between increases of A1. default 0 (no sublabels)
#' @param ... additional arguments to be passed on to mtext()
#' @return Nothing (adds alphabetic label to current plotting device)
#' @export lebbel
#' @examples
#' lebbel(adj=c(0,0),A1<-1) #for first label in plot
#' lebbel(adj=c(0,0)) #for every subsequent label

lebbel<-function(A1=NULL,v=FALSE,lwr=FALSE,o=0,adj=c(0,0),alt=NULL,...){
	ABC<-"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	if(lwr) tolower(ABC)->ABC
    if(!exists("A1", envir = .GlobalEnv)) A1<<-1
    if(!is.null(A1) && is.numeric(A1)) A1<<-A1
    if(is.null(A1)) get("A1", envir = .GlobalEnv)->A1

    A1->A1_
    if(!is.null(alt)) A1_<-alt[A1]
    
    if(o==0){
    
mtext(substr(ABC,A1_,A1_),adj=adj,...)
if(v) print(substr(ABC,A1_,A1_))
if(v) print(A1_)
A1<-A1+1
A1<<-A1
}else{
    if(!exists("o_temp", envir = .GlobalEnv)) o_temp<<-1
    if(exists("o_temp", envir = .GlobalEnv)) get("o_temp", envir = .GlobalEnv)->o_temp
    
AB0 <- substr(ABC,A1_,A1_)
mtext(bquote(.(AB0)[.(o_temp)]),adj=adj,...)
if(v) print(substr(ABC,A1_,A1_))
if(v) print(A1_)

o_temp<-o_temp+1
o_temp<<-o_temp
    
    if(o_temp>o){o_temp<<-1
    A1<-A1+1
	A1<<-A1}
}
#usage equivalent to mtext(side=3, line=0.5, substr(ABC,A1,A1),adj=c(0,0))

}##





##function whichorderin()
#' convenience function for determining order of elements of one vector in another vector
#' @param ordered vector for whose elements to determine order
#' @param unordered vector in which to look for elements of ordered
#' @param multiple logical, allow multiple occurrences of each element of ordered (slower)
#' @param na.rm logical, remove nas for elements that were not found from output
#' @return indices at which each element in ordered is found in unordered (can be used to bring unordered into identical order)
#' @export whichorderin


whichorderin<-function(ordered,unordered,multiple=FALSE,na.rm=TRUE){#function to copy the order of an identifier
if(multiple){#multiple occurrences in unordered possible for each elemen of ordered (slower)
out<-numeric()
for(i in 1:length(ordered)){
which(unordered==ordered[i])->i_
out<-c(out,i_)
}

}else{#if only the first of each element should be returned
out<-numeric(length(ordered))
for(i in 1:length(ordered)){
which(unordered==ordered[i])->i_
if(length(i_)>0) i_[1]->out[i]
}
}
if(na.rm) out<-out[!is.na(out)] #remove NAs
return(out)#indices of each element of ordered in unordered
}##




##function bracket()
#' plot a bracket at a given x or y position
#' @param x x value or values at which to plot bracket
#' @param y y value or values at which to plot bracket
#' @param adjust factor to multiply plot width or height width for length of bracket ends
#' @param labels text label to place on top of bracket
#' @param txt list() type object of additional settings to pass on to txt
#' @param sep should brackets be plotted as one single line or separate lines?
#' @param ... Other arguments to pass on to lines()
#' @return nothing (plots bracket on current plotting device)
#' @export bracket

bracket<-function(x,y,adjust=-1/50,labels="*",txt=list(),sep=FALSE,...){

if(length(x)==1){
horiz<-FALSE
pr("x")->plex
if(length(y)<2) stop("vertical plotting but less than one value specified for y")
range(y)->y
}else{
horiz<-TRUE
pr("y")->plex
if(length(x)<2) stop("horizontal plotting but less than one value specified for x")
range(x)->x
}

line_args<-list(...)
#prep text settings
if(!("labels"%in%names(txt))) labels->txt$labels
if(!("x"%in%names(txt))) mean(x)->txt$x
if(!("y"%in%names(txt))) mean(y)->txt$y
if(!("adj"%in%names(txt)) & horiz) c(0.5,0)->txt$adj
if(!("adj"%in%names(txt)) & !horiz) c(-0.5,0.5)->txt$adj
if(!("cex"%in%names(txt))) 2->txt$cex
if(!("col"%in%names(txt)) & "col"%in%names(line_args)) line_args$col->txt$col

##plot lines
if(horiz){
if(sep){
lines(x=x,y=c(y,y),...)
lines(x=x[c(1,1)],y=c(y,y+adjust*plex),...)
lines(x=x[c(2,2)],y=c(y,y+adjust*plex),...)}else{
lines(x=x[c(1,1,2,2)],y=c(y+adjust*plex,y,y,y+adjust*plex),...)
}
}else{
if(sep){lines(x=c(x,x),y=y,...)
lines(y=y[c(1,1)],x=c(x,x+adjust*plex),...)
lines(y=y[c(2,2)],x=c(x,x+adjust*plex),...)}else{
lines(y=y[c(1,1,2,2)],x=c(x+adjust*plex,x,x,x+adjust*plex),...)
}
}

if(!is.null(labels)) do.call("text",txt)

}



