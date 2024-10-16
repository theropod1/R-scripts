##function: export()
#' simple function to render the current plot window to a bitmap image. 
#' @param format output device to use (tested with jpeg and png, default is jpeg)
#' @param filename name of the output file. If NULL (default), file is named "Rdeviceexport_" followed by the current Sys.time() and the name of the function given to parameter format as an extension.
#' @param scale scale factor (defaults to 1)
#' @param res resolution of export image (defaults to 100*scale)
#' @param width (optional) width of output
#' @param height (optional) height of output (overridden by width if specified to maintain aspect ratio)
#' @param ... additional arguments to pass to dev.copy
#' @return saves the current plotting window to file
#' @importFrom grDevices dev.copy
#' @importFrom grDevices dev.size
#' @importFrom grDevices dev.off
#' @importFrom grDevices jpeg
#' @importFrom grDevices png
#' @export export
#' @examples
#' plot(c(1,2,3),c(1,2,3))
#' export(png) # exports the current plot window to a png file

export<-function(format=jpeg,filename=NULL, scale=1,res=100*scale,width=NULL,height=NULL,...){

extension <- as.character(substitute(format))

if(is.null(filename)){
filename<-paste0("Rdeviceexport_",gsub(":","-",gsub(" ","-",as.character(Sys.time()))),".",extension)
}


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
message("both height and width provided, scaling to width")
}

dev.copy(format, filename, width=s[1]*scale, height=s[2]*scale,res=res,...)
dev.off()
}
##
