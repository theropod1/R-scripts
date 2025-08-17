##%>%
#' A simple pipe operator that takes the left hand side argument and passes it to the function given as a string on the right hand side. indicate with a "." as an argument in the right hand side function code which argument should be replaced with the piped call
#' @param lhs object or function call to be evaluated and passed on
#' @paran rhs either a function name, or a character string containing a valid function call (i.e. able to be coerced to a valid call by parse()), with the argument(s) to be replaced by lhs indicated by a .
#' @return The parsed and evaluated rhs with . replaced by lhs
#' @export %>%

`%>%` <- function(lhs,rhs){
if(!is.character(rhs)) rhs<-deparse(substitute(rhs)) #if rhs is not a character string, coerce it to one

if(any(c(grepl(",.",rhs,fixed=TRUE),grepl("=.",rhs,fixed=TRUE),grepl("(.",rhs,fixed=TRUE)))){ #look for argument(s) containing the
gsub(",.",paste0(",",deparse(substitute(lhs))),rhs,fixed=TRUE)->rhs_
gsub("=.",paste0("=",deparse(substitute(lhs))),rhs_,fixed=TRUE)->rhs_
gsub("(.",paste0("(",deparse(substitute(lhs))),rhs_,fixed=TRUE)->rhs_

}else{#if no arguments are supplied between parentheses
gsub("()",paste0("(",deparse(substitute(lhs)),")"),rhs,fixed=TRUE)->rhs_
if(!grepl("(",rhs,fixed=TRUE)) rhs_<-paste0(rhs,"(",deparse(substitute(lhs)),")") #if only a function name was supplied
}

parse(text=rhs_)->rhs_#convert string back to call
message("evaluating call: ", rhs_)

eval(rhs_) #final result
}
##


##%+-%
#' A simple plusminus operator that takes the left hand side argument and returns a vector with the right hand side value subtracted from and added to each value of lhs
#' @param lhs object or function call to be evaluated and passed on
#' @paran rhs a single numeric
#' @return lhs+-rhs
#' @export %+-%
`%+-%` <- function(lhs,rhs){
out<-matrix(NA,nrow=length(lhs),ncol=2)
for(i in 1:length(lhs)) lhs[i]+c(-1,1)*rhs->out[i,]
if(length(out)==2) as.numeric(out)->out
return(out)
}

