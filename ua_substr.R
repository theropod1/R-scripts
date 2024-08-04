##function ua_substr_()
#' evaluate whether a given string represents an unambiguous substring among a number of options
#' @param x string to test (or character vector with several strings to test)
#' @param options character()-vector with options among which to test for unambiguity of x
#' @param match (optional) character string (single or vector of length equal to length(x)) to match (if x is not a substring of match, FALSE is returned)
#' @param ignore.case logical indicating whether to ignore case while matching substrings (default to TRUE)
#' @param substitute logical indicating whether to return logical TRUE/FALSE or to substitute and return the full character string corresponding to the unambiguous substring, if available
#' @details 
#' @return Returns either a logical() identifying if x is an unambiguous substring within options (and corresponding to match, if specified), or a character() containing the corresponding full character string

ua_substr<-function(x, options, match=NULL, ignore.case=TRUE, substitute=FALSE,...){

##define base function
ua_substr_<-function(x, options, match=NULL, ignore.case=TRUE, substitute=FALSE,...){
TRUE->res2 #matches match (assumed true if match==NULL)

if(!is.null(match)){
grepl(x, match,ignore.case=ignore.case,...)->res2
if(res2==FALSE){return(FALSE)#if match is given and x is not in match, return FALSE
}else{#otherwise see if x is also in any other elements
    if(match%in%options){
    grep(x,options,ignore.case=ignore.case,...)->res1
    }else{
    grep(x,c(match,options),ignore.case=ignore.case,...)->res1
    }
}}else{#if no match is specified, test for occurrences of x in options
grep(x,options,ignore.case=ignore.case,...)->res1
}

if(substitute==TRUE){
if(length(res1)==1){#if x is unambiguous, return corresponding character string
return(options[res1])
}else{return(FALSE)}

}else{if(length(res1)==1 & res2==TRUE){
return(TRUE)}else{return(FALSE)}}

}
#

##generalized to vectors:
if(length(x)>1){#if a vector is supplied for x
    if(length(match)<length(x)){
    match<-rep(match,length(x))[1:length(x)]#make sure match is same length
    }
    
logical()->result
for(i in 1:length(x)){
result[i]<-ua_substr_(x[i], options, match[i], ignore.case=ignore.case, substitute=substitute,...)}
return(result)
}else{
return(ua_substr_(x, options, match, ignore.case=ignore.case, substitute=substitute,...))
}

}
##
