gplates_pbgeo<-function(ma=66, CL=NULL, PL=NULL, occ=NULL, ocean="lightblue", land="white", col="darkgrey", polycol="darkgrey", legend=FALSE, pointcol="hltax", alpha=1, size=1){
##option plotting sf directly without dataframe conversion
if(is.null(CL)) gplatesr::gplates_reconstruct_coastlines(ma)->CL

sf::st_transform(sf::st_as_sf(CL), crs = "+proj=moll")->m__

if(is.null(PL)) gplatesr::gplates_reconstruct_static_polygons(ma)->PL

sf::st_transform(sf::st_as_sf(PL), crs = "+proj=moll")->mp__

##prep background box
bbox <- data.frame(lon = c(rep(-180, 180), seq(-180, 180, length.out = 100),
                           rep(180, 180), seq(180, -180, length.out = 100)),
                   lat = c(seq(-90, 90, 1), rep(90, 99), 
                           seq(90, -90, -1), rep(-90, 99)))
                           
  sf::st_as_sf(bbox,coords = c("lon", "lat"), crs = 4326)->bbox
  dplyr::summarise(bbox,geometry = sf::st_combine(geometry))->bbox
  sf::st_cast(bbox,"POLYGON")->bbox
  sf::st_transform(bbox,"ESRI:54009")->bbox

require(ggplot2)

ggplot()+geom_sf(data = bbox, fill = ocean, col = NA, linewidth = 0) + geom_sf(data=m__, fill=land, col="darkgrey",linewidth=.25)+geom_sf(data=mp__, fill=NA, col=polycol,linewidth=.25)+coord_sf(default_crs = "+proj=moll", clip="on")->ggmap

if(!is.null(occ)){
occ<-occ[!is.na(occ$paleolng),]#make sure incomplete cases are ignored
occ<-occ[!is.na(occ$paleolat),]

ggmap<-ggmap+geom_sf(data=sf::st_transform(sf::st_as_sf(occ, coords = c("paleolng", "paleolat"), crs = 4326)), aes(col=hltax, pch=hltax), alpha=alpha, size=size)

}

if(!legend) ggmap<-ggmap+theme(panel.background = element_rect(fill = NA), plot.background = element_rect(fill = NA),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid.major = element_line(color = NA, linewidth = 0),panel.grid.minor = element_line(color = NA, linewidth = 0), legend.position="none")
if(legend) ggmap<-ggmap+theme(panel.background = element_rect(fill = NA), plot.background = element_rect(fill = NA),axis.text = element_blank(),axis.ticks = element_blank(),panel.grid.major = element_line(color = NA, linewidth = 0),panel.grid.minor = element_line(color = NA, linewidth = 0)) + labs(col=' Taxon ',pch=' Taxon ')

print(ggmap)

invisible(list(ma=ma, CL=m__, PL=mp__, ggplot_map=ggmap))

}
