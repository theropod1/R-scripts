#install.if.missing("geojsonio")
#needs apt install protobuf-compiler libprotobuf-dev libjq-dev
#install.if.missing(c("jsonlite", "sp"))

##replacement for rgdal::readOGR
readOGR_gplates <- function(url) {
  if (!requireNamespace("geojsonio", quietly = TRUE))
    stop("Package 'geojsonio' is required")
  if (!requireNamespace("sp", quietly = TRUE))
    stop("Package 'sp' is required")

  tf <- tempfile(fileext = ".json")

  on.exit(unlink(tf), add = TRUE)

  utils::download.file(
    url      = url,
    destfile = tf,
    quiet    = TRUE
  )

  geojsonio::geojson_read(
    x    = tf,
    what = "sp"
  )
}

##replacement for broom::tidy
sp_polygons_to_df <- function(sp_obj) {
  stopifnot(inherits(sp_obj, "SpatialPolygonsDataFrame"))

  res <- vector("list", length(sp_obj@polygons))
  k <- 1

  for (i in seq_along(sp_obj@polygons)) {
    poly <- sp_obj@polygons[[i]]

    for (j in seq_along(poly@Polygons)) {
      coords <- poly@Polygons[[j]]@coords

      res[[k]] <- data.frame(
        long  = coords[, 1],
        lat   = coords[, 2],
        group = paste0(i, "_", j)
      )

      k <- k + 1
    }
  }

  do.call(rbind, res)
}



###helper functions for plotting
makemap<-function(x,y=NULL,names=NULL){
if(is.data.frame(x) && is.null(y) && is.null(names)){
df_map <- list(
  x = x$long,
  y = x$lat,
  names = x$group
)
}else{
df_map <- list(
  x = x,
  y = x,
  names = names
)
}
class(df_map) <- "map"

return(df_map)
}##

raw_multipoly<-function(x,y=NULL,names=NULL,...){

if(is.null(y)) y<-x$lat
if(is.null(names)) names<-x$group
if(is.data.frame(x)) x<-x$long

for(i in 1:length(names)){
polygon(x=x[names==names[i]],y=y[names==names[i]])
}

}

fast_multipoly <- function(df=NULL, x = "long",y = "lat", group = "group", thinning=FALSE, FUN=polygon,...) {
if(is.numeric(x) && is.numeric(y) && length(group)==length(x)) df<-data.frame(x=x,y=y,group=group)
if(!(is.data.frame(df) | is.list(df))) stop("no valid data.frame or x-y-group data found!")

  ord <- order(df[[group]]) #order
  df <- df[ord, ]
  
  if(thinning>0) df <- df[ seq(1:nrow(df)) %% thinning == 1, ]

  split_idx <- cumsum(!duplicated(df[[group]])) #determine which elements are not duplicates, i.e. new polygon starts

  xx <- unlist(tapply(df[[x]], split_idx, function(v) c(v, NA)))
  yy <- unlist(tapply(df[[y]], split_idx, function(v) c(v, NA)))

  FUN(xx, yy, ...)
}



if(1==2){##run example:
library(ggplot2)

ma<-125 #age to visualize
url <- paste0("http://gws.gplates.org/reconstruct/coastlines/?time=",ma,"&model=GOLONKA")
url2<-paste0("http://gws.gplates.org/reconstruct/static_polygons/?time=",ma,"&model=GOLONKA")

# read GeoJSON from a URL and return an sp object
coast_sp <- readOGR_gplates(url)
poly_sp <- readOGR_gplates(url2)

df <- sp_polygons_to_df(coast_sp)
df2 <- sp_polygons_to_df(poly_sp)

ggplot() + geom_polygon(data=df, aes(long, lat, group = group), fill = "lightgray", color = "darkgrey")  + geom_polygon(data=df2, aes(long, lat, group = group), fill = NA, color = "black", size=1)
}



