##function matrixplot()
#' Visualize a matrix as a color-coded plot
#'
#' @param matrix The matrix to be plotted
#' @param colscale.x vector of two values corresponding to the colors to interpolate between for cell backgrounds
#' @param colscale vector of two colors to interpolate between for cell backgrounds
#' @param na.col color to use for NA values in matrix
#' @param alpha opacity to use for cell backgrounds
#' @param mar Settings for par("mar")
#' @param adj.y Adjustment for y axis/row name labels
#' @param adj.x Adjustment for x axis/column name labels
#' @param srt.x Rotation for x axis/column name labels
#' @param srt.y Rotation for y axis/column name labels
#' @param digits number of significant digits to visualize
#' @param bold_threshold threshold for which numbers to bold (numbers greater than threshold are bolded)
#' @param txt.col text color for matrix content
#' @param txt.col.lab text color for row/column names
#' @param txt.cex text size
#' @param lab.cex row/column name size
#' @param lines whether to borders of matrix
#' @return 
#' @importFrom graphics polygon
#' @export matrixplot
#' @examples

matrixplot<-function(matrix, colscale.x=range(matrix,na.rm=T), colscale=c("white","black"), na.col="white", alpha=0.5, mar=c(1.1,6.1,5.1,1.1), adj.y=1, adj.x=c(0.5,0), srt.x=45, srt.y=0, digits=3, bold_threshold=NA, txt.col="black", txt.col.lab=txt.col, txt.cex=1, lab.cex=1,lines=T, spaces=c("_")){

par("mar")->omar
par(xpd=NA, mar=mar)

plot(NULL,type="n", xlim=c(0,1), ylim=c(0,1), axes=F, xlab="", ylab="")
#find y and x coordinates
rev(1/nrow(matrix)*0.5*c(0:(nrow(matrix)*2)))->ys_
ys_[even(nrow(matrix))]->ys

1/ncol(matrix)*0.5*c(0:(ncol(matrix)*2))->xs_
xs_[even(ncol(matrix))]->xs


rownames(matrix)->rows
colnames(matrix)->cols

if(!is.null(spaces)){#replace select characters by spaces
if(is.character(spaces)){
for(i in 1:length(spaces)){
gsub(spaces[i]," ", rows)->rows
gsub(spaces[i]," ", cols)->cols
}}}

text(adj=adj.y, x=-0.04, y=ys, rows,srt=srt.y,cex=lab.cex, col=txt.col.lab)#y axis labels
text(adj=adj.x, y=1.04, x=xs, cols,srt=srt.x,cex=lab.cex, col=txt.col.lab)#x axis labels

invisible(data.frame(xs_,ys_))

if(lines==TRUE){
par(xpd=FALSE)
abline(v=xs_[odd(ncol(matrix)+1)])
abline(h=ys_[odd(nrow(matrix)+1)])
par(xpd=NA)}

##add data

#set cell colors
matrix(add.alpha(intercol(matrix, colx=colscale.x, col=colscale, na.col=na.col),alpha),nrow=nrow(matrix))->cellcol
#print(cellcol)


for(i in 1:nrow(matrix)){
for(j in 1:ncol(matrix)){

#cellcol<-"blue"#intercol(matrix[i,j],x1=min, x2=max)

xpoly<-xs_[odd(ncol(matrix)+1)][c(j,j+1,j+1,j)]
ypoly<-ys_[odd(nrow(matrix)+1)][c(i+1,i+1,i,i)]
polygon(x=xpoly, y=ypoly,border=NA, col=cellcol[i,j])

if(!is.na(bold_threshold) & !is.na(matrix[i,j])){

if(matrix[i,j]>bold_threshold){
val<-as.character(signif(matrix[i,j],digits))

text(adj=c(0.5,0.5),y=ys[i], x=xs[j], bquote(bold(.(get("val")))), col=txt.col,cex=txt.cex)

}else{
text(adj=c(0.5,0.5),y=ys[i], x=xs[j], signif(matrix[i,j],digits), col=txt.col,cex=txt.cex)
}}else{
text(adj=c(0.5,0.5),y=ys[i], x=xs[j], signif(matrix[i,j],digits), col=txt.col,cex=txt.cex)
}

}
}

par(xpd=NA, mar=omar)#restore par settings
}



##function: intercol()
#' interpolate between two color values (col) based on another variable (x)
#'
#' @param x values for which to interpolate color
#' @param colx values corresponding to ends of color scale
#' @param col colors corresponding to ends of color scale
#' @param na.col color to use for NA values
#' @return 
#' @export intercol
#' @examples


intercol<-function(x, colx=c(0,1), col=c("white","black"), na.col="white"){

tmp<-function(x,colx=c(0,1), col=c("white","black"), na.col="white"){
if(is.na(x)){return(na.col)}else{
if(x>max(colx,na.rm=T)){
return(col[2])
}else if(x<min(colx,na.rm=T)){
return(col[1])}else{

col2rgb(col[1])->col1
col2rgb(col[2])->col2

rgb_interp<-col1+(col2-col1)*((x-colx[1])/(colx[2]-colx[1]))
return(rgb(t(rgb_interp),maxColorValue=255))

}}}##close internal function

if(length(x)>1){

	numeric()->out
	for(i in 1:length(x)){
tmp(x=x[i],colx=colx,col=col,na.col=na.col)->out[i]}
}else{
tmp(x,,colx=colx,col=col, na.col)->out}

return(out)

}


##find even and odd numbers

even<-function(length=NULL,min=1, max=min+length*2,c=NULL){
if(!is.null(c)){
c[c%%2==0]
}else{
if(min%%2==0){
seq(min, max, 2)
}else{
seq(min+1, max, 2)
}}
}



odd<-function(length=NULL,min=0, max=min+length*2, c=NULL){

if(!is.null(c)){
c[c%%2!=0]
}else{
if(min%%2==1){
seq(min, max, 2)
}else{
seq(min+1, max, 2)
}}
}
