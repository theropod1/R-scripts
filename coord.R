##function coord
#' convert a character string with geographical coordinates into decimal coordinates
#' @param x character string containing coordinates in dd mm ss N/S/E/W format, longitude and latitude separated by a space
#' @return a data.frame() containing two columns, $lng for the decimal longitude, $lat for the decimal latitude
#' @export coord

coord<-function(x){

coord_<-function(x){
strsplit(x, split=c(" "))[[1]]->indiv
length(indiv)->l
#print(l)

indiv[grep("N",indiv)]->North
indiv[grep("S",indiv)]->South

indiv[grep("W",indiv)]->West
indiv[grep("E",indiv)]->East

lngneg<-FALSE
if(!(length(East)==1 | length(West)==1)){
warning("No longitude found, proceeding to use last value as longitude")
lng<-ifelse(l>1, indiv[l], indiv)
}else{
if(length(East)==1){
East->lng
}else{
West->lng
lngneg<-TRUE
}
}
#print(lng)

latneg<-FALSE
if(!(length(North)==1 | length(South)==1)){
warning("No latitude found, proceeding to use first value as latitude")
lat<-ifelse(l>1, indiv[1], indiv)
}else{
if(length(North)==1){
lat<-North
}else{
lat<-South
latneg<-TRUE
}
}#

##numeric and decimal conversion for longitude
deg<-numeric()
a<-1
while(substr(lng,a,a)!="\u00B0"){deg<-paste0(deg,substr(lng,a,a))
a<-a+1}
lng<-substr(lng,a+1,nchar(lng))
a<-1
mi<-numeric()
while(substr(lng,a,a)!="\u2032" & substr(lng,a,a)!="'"){mi<-paste0(mi,substr(lng,a,a))
a<-a+1}
lng<-substr(lng,a+1,nchar(lng))
a<-1
sec<-numeric()
if(grepl("\u2033",lng)==FALSE & grepl("\"",lng)==FALSE){sec<-0}else{
while(substr(lng,a,a)!="\u2033" & substr(lng,a,a)!="\""){sec<-paste0(sec,substr(lng,a,a))
a<-a+1}}
#message("longitude: ",deg,"\u00B0",mi,"\'",sec,"\"")

lng<-as.numeric(deg)+as.numeric(mi)/60+as.numeric(sec)/(60^2)


#print(lat)
##numeric and decimal conversion for latitude
deg<-numeric()
a<-1
while(substr(lat,a,a)!="\u00B0"){deg<-paste0(deg,substr(lat,a,a))
a<-a+1}
lat<-substr(lat,a+1,nchar(lat))
a<-1
mi<-numeric()
while(substr(lat,a,a)!="\u2032" & substr(lat,a,a)!="\'"){mi<-paste0(mi,substr(lat,a,a))
a<-a+1}
lat<-substr(lat,a+1,nchar(lat))
a<-1
sec<-numeric()
if(grepl("\u2033",lat)==FALSE & grepl("\"",lat)==FALSE){sec<-0}else{
while(substr(lat,a,a)!="\u2033" & substr(lat,a,a)!="\""){sec<-paste0(sec,substr(lat,a,a))
a<-a+1}}
#message("latitude: ",deg,"\u00B0",mi,"\'",sec,"\"")

lat<-as.numeric(deg)+as.numeric(mi)/60+as.numeric(sec)/(60^2)

if(lngneg){
lng<--1*lng}
if(latneg){
lat<--1*lat}
rm(deg)
rm(mi)
rm(sec)
rm(a)

return(data.frame(lng=lng,lat=lat))

}#end internal function

if(length(x)>1){
longitudes<-numeric()
latitudes<-numeric()

for(i in 1:length(x)){
longitudes[i]<-coord_(x[i])$lng
latitudes[i]<-coord_(x[i])$lat
}
return(data.frame(lng=longitudes, lat=latitudes))
}else{coord_(x)}
}
##
