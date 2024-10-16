##function install.if.missing()
#' Install a package or entries in a list of packages only if package is not already installed.
#' @param packages packages to install as character vector
#' @param verbose whether to be verbose about status of package installation
#' @return Nothing (installs packages not already installed)
#' @export install.if.missing()

install.if.missing<-function(packages, verbose=TRUE){
installed.packages()->p
p[,"Package"]->p

n<-0

for(i in 1:length(packages)){#loop through packages
as.character(packages[i])->p1

if(!is.na(p1) &!is.null(p1)){

if(!(p1%in%p)){

	if(verbose){message("installing ",p1)}

tryCatch({install.packages(p1)}, error=function(e){return(paste("--ERROR--",e$message))},warning=function(w){return(w$message)})->o

if(exists("o")){
if(length(o)>0){
if(!grepl("not available",o) | !grepl("--ERROR--",o)){#conditions for when to increment installed package count
n<-n+1

}else{
	if(verbose){message(o)}
if(exists("os")){os<-c(os,o)}else{os<-o}

}}else{
n<-n+1 #increment package counter if there is no error or warning
}}else{
n<-n+1 #increment package counter if there is no error or warning
}

}}}#end loop

message(n, " packages installed, ",length(packages)-n, " packages not installed!")
if(exists("os")){message(os,"errors or warnings without installation")}
}
