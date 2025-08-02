##scaletobounds()
#' Linearly scale a vector to a set of bounds
#' @param x numeric or numeric-alike vector to be scaled
#' @param bounds numeric vector of length 2 giving what to scale the minimum and maximum values in x to. Defaults to \code{c(0,1)}
#' @return the rescaled vector x
#' @export scaletobounds

scaletobounds<-function(x,bounds=c(0,1)){
min(x)->lwr
max(x)->upr
upr-lwr->scale
bounds[2]-bounds[1]->newscale

abs(x-lwr)/scale*newscale+bounds[1]
}
