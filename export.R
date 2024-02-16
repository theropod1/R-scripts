##function: 
#simple function to render the current plot window "as is". Scale can be used to specify a scale factor. Note that if a height and/or width are specified, this is multiplied by the setting for scale. Currently functional output formats are jpeg and png

export<-function(format=jpeg,filename=paste0("Rdeviceexport.jpeg"), scale=1,res=100*scale,width=NULL,height=NULL,...){

dev.size("px")->s
s[2]/s[1]->asp

if(!is.null(height)){
height/s[2]->s2
s[2]<-height
s[1]<-height/asp
res<-res*s2
}
if(!is.null(width)){
width/s[1]->s2
s[1]<-width
s[2]<-width*asp
res<-res*s2
}

if(!is.null(height) & !is.null(width)){
warning("both height and width provided, scaling to width")
}

dev.copy(format, filename, width=s[1]*scale, height=s[2]*scale,res=res,...)
dev.off()
}
##
