##function ifs()
#' return 
#' @param conditions A vector of logical expressions to evaluate
#' @param returns A vector of values to return, depending on which expression returns TRUE
#' @param none what to return of none of the conditions are met
#' @param mode Setting whether to evaluate for expression returning TRUE (default) or FALSE
#' @return a vector of whatever returns are specified
#' @export ifs
#' @examples
#' a<-c(1:10)
#' conditions<-c(expression(a>=6 & a<=8),expression(a==3 | a==2))
#' ifs(conditions, returns=c("sixtoeight","twoorthree"), none="other")

ifs<-function(conditions, returns, none=NA, mode=TRUE){
length(conditions)->nc
length(eval(conditions[1]))->nv

matrix(nrow=nv, ncol=nc)->res

for(i in 1:length(conditions)){#evaluate expressions
eval(conditions[i])->res[,i]
}

numeric()->output

for(i in 1:nv){##loop through matrix and ascertain which condition is met
which(res[i,]==mode)->j
if(length(j)==0){
none->output[i]
}else if(length(j)>1){
returns[min(j)]->output[i]
}else{
returns[j]->output[i]
}
}#end looping through matrix

return(output)
}
##
