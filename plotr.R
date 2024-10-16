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
