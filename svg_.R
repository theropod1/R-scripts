svg_<-function(script=NULL, width=12, height=9, ptsize=18, fname=NULL, bg = "transparent",...){

c(width,height,ptsize)->dimres

if(is.null(fname)){fname<-"svgexport"
nf<-1
fname1<-paste0(fname,"_",leading0(nf,mx=4),".svg")

while(file.exists(fname1)){
fname1<-paste0(fname,"_",leading0(nf,mx=4),".svg")
nf<-nf+1
}
}else{fname->fname1}
message("exporting to ", fname1, " with ", paste0(dimres,collapse=", "))

svg(file=fname1, width=dimres[1], height=dimres[2], pointsize=dimres[3], bg=bg,...)

if(!is.null(script)){source(script) 
dev.off()

}else{ 
script<-select.list(list.files(full.names=F),multiple = FALSE, title = "Select script to execute", graphics = FALSE)

if(!is.null(script) && length(script)>0){
source(script)
dev.off()

}
}

}


pdf_<-function(script=NULL, width=12, height=9, ptsize=18, fname=NULL, bg = "transparent",...){

c(width,height,ptsize)->dimres

if(is.null(fname)){fname<-"pdfexport"
nf<-1
fname1<-paste0(fname,"_",leading0(nf,mx=4),".pdf")

while(file.exists(fname1)){
fname1<-paste0(fname,"_",leading0(nf,mx=4),".pdf")
nf<-nf+1
}}else{fname->fname1}
message("exporting to ", fname1, " with ", paste0(dimres,collapse=", "))

pdf(file=fname1, width=dimres[1], height=dimres[2], pointsize=dimres[3], bg=bg,...)

if(!is.null(script)){source(script) 
dev.off()

}else{ 
script<-select.list(list.files(full.names=F),multiple = FALSE, title = "Select script to execute", graphics = FALSE)

if(!is.null(script) && length(script)>0){
source(script)
dev.off()

}
}

}



##
leading0<-function(x,prefix="",suffix="", mx=NULL){

if(is.null(mx)) max(nchar(x),na.rm=TRUE)->mx

x_<-x
for(i in 1:length(x)){
if(!is.na(x[i])){
len<-mx-nchar(x[i])
lead<-paste0(rep(0,len),collapse="")

if(is.na(lead)) lead<-""

x_[i]<-paste0(lead,x[i])
}
}
x_<-paste0(prefix,x_,suffix)
return(x_)
}
