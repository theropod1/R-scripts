##addimg()
#' convenience function for adding images to plots
#' @param image Array containing image data, e.g. output of png::readPNG() or jpeg::readJPG()
#' @param x x coordinate to plot at
#' @param y y coordinate to plot at
#' @param adj numeric vector of length two for alignment of the image in relation to x and y (defaults to c(0.5,0.5), for an image bounding box centered on x and y)
#' @param width width of the plotted image (if NULL, width is kept in proportion to height)
#' @param height height of the plotted image (if NULL, height is kept in proportion to width)
#' @return Plots the image to the current plotting device in the specified location and scale, and returns an invisible vector containing the left, right, bottom and top coordinates of the plotted image
#' @export addimg

addimg<-function(image,x,y,adj=c(0.5,0.5),width=NULL,height=NULL,...){
asp_ratio<-AR() #get aspect ratio of current plotting device

if(is.null(width) & is.null(height)) stop("Both width and height are null, at least one needs to be specified!")

#determine width and height
imagewidth<-ifelse(!is.null(width),width,height*dim(image)[2]/dim(image)[1]*asp_ratio)
imageheight<-ifelse(!is.null(height),height,width*dim(image)[1]/dim(image)[2]*asp_ratio)

#determine plotting location
xleft<-x-imagewidth*(adj[1])
ybottom<-y-imageheight*(adj[2])
xright<-x+imagewidth*(1-adj[1])
ytop<-y+imageheight*(1-adj[2])

#plot image
rasterImage(image, xleft=xleft, ybottom=ybottom,xright=xright, ytop=ytop,xpd=TRUE,asp=1,...)
#return plotting coordinates invisibly
invisible(c(xleft,xright,ybottom,ytop))
}
##
