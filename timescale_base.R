## class constructor for geotimescale
#' Create a custom geotimescale object
#'
#' @param data a Matrix or data.frame, or a filename to read using read.csv(), containing a hierarchical stratigraphic table
#' @param bottom_name character giving name of the column containing the bottom age for each interval
#' @param top0 what value to use as top value for all intervals (defaults to 10E-8 for compatibility with log-transformed plotting)
#' @return A 'geotimescale' object.
#' @export new_geotimescale

new_geotimescale <- function( data, bottom_name="bottom", top0=10E-8) {

 if(is.character(data) && length(data)==1 && file.exists(data)) read.csv(data) -> data
 
 data[order(data[,bottom_name]),]->data
 
  structure( data.frame( data[,which(colnames(data)!=bottom_name)], bottom=data[,bottom_name] ), class = c("geotimescale","data.frame") ) -> out
  out$top<-c(top0, out$bottom)[1:length(out$bottom)]
  stratifill<-c('#FDEDEC','#FDEDEC','#FDECE4','#FDECE4','#FEECDB','#FEECDB','#FEEBD2','#FFF2D3','#FFF2C7','#FFF2C7','#FFF2BA','#FFEDB3','#FFF0B9','#FFEFAF','#F9F97F','#FFFFBF','#FFF2CD','#FFFFB3','#FFF2C3','#FFFF99','#FFFF73','#FFFF66','#FFEC8C','#FFFF59','#FFFF4D','#FFEC73','#FFFF41','#FFFF33','#FFEC55','#FFFF00','#FFE619','#FEE6AA','#FED99A','#FEC07A','#FDCDA1','#FDC091','#FDB482','#FCA773','#FDB46C','#FDBF6F','#FEBF65','#FDB462','#FDA75F','#FD9A52','#F2F91D','#F2FA8C','#E6F47F','#D9EF74','#CCE968','#BFE35D','#B3DE53','#A6D84A','#CCEA97','#BFE48A','#B3DF7F','#A6D975','#99D36A','#8CCD60','#8CCD57','#7FC64E','#D9F1F7','#CCECF4','#BFE7F1','#B3E3EE','#BFE7E5','#B3E2E3','#A6DDE0','#9AD9DD','#80CFD8','#99CEE3','#80C5DD','#67BCD8','#4EB3D3','#42AED0','#34B2C9','#E3B9DB','#D6AAD3','#C99BCB','#BD8CC3','#C983BF','#BC75B7','#B168B1','#B051A5','#A4469F','#983999','#812B92','#67C5CA','#FCC0B2','#FCB4A2','#FBA794','#FB9A85','#FB8D76','#FB8069','#FB745C','#E38776','#E37B68','#E36F5C','#E36350','#EF5845','#F04028','#CCD4C7','#BFD0C5','#BFD0BA','#B3CBB9','#A6C7B7','#99C2B5','#8CBEB4','#7EBCC6','#BFC26B','#B3BE6C','#A6B96C','#99B46C','#8CB06C','#80AB6C','#678F66','#67A599','#F2EDB3','#F2EDAD','#F1E19D','#F1E185','#F1D576','#F1C868','#E5D075','#E5C468','#E5B75A','#E5AC4D','#CB8C37','#E6F5E1','#D9F0DF','#CCECDD','#BFE6CF','#CCEBD1','#BFE6C3','#B3E1C2','#BFE6CF','#B3E1C2','#A6DCB5','#99D7B3','#B3E1B6','#A6DBAB','#99D69F','#8CD094','#7FCA93','#74C69C','#66C092','#4DB47E','#41B087','#33A97E','#1A9D6F','#009270','#E6F5C9','#D9F0BB','#CCEBAE','#B3E095','#CCDFAA','#BFD99D','#B3D492','#A6CF86','#B3CA8E','#A6C583','#99C078','#A6BA80','#99B575','#8CB06C','#7FA056','#99C08D','#9AD9DD','#FED96A','#FECC5C','#FEBF4E','#FEB342','#FED99A','#FDCC8A','#FDC07A','#FDB462','#F875A7','#F76898','#F75B89','#F74F7C','#F74370','#F73563','#FAA7C8','#F99BC1','#F881B5','#F768A9','#F668B2','#F4449F','#E61D8C','#DA037F','#F0047F','#AE027E','#F74370','#FDEDEC','#FEECDB','#FFF2D3','#FFF0B9','#FFF2CD','#FFF2C3','#FFEC8C','#FFEC55','#A6D84A','#8CCD57','#B3E3EE','#42AED0','#BD8CC3','#983999','#BFD0BA','#8CBEB4','#B3BE6C','#80AB6C','#F1E19D','#E5AC4D','#7FCA93','#1A9D6F')
  names(stratifill)<-c('Meghalayan','Upper Holocene','Northgrippian','Middle Holocene','Greenlandian','Lower Holocene','Holocene','Upper Pleistocene','Chibanian','Middle Pleistocene','Calabrian','Gelasian','Lower Pleistocene','Pleistocene','Quaternary','Piacenzian','Upper Pliocene','Zanclean','Lower Pliocene','Pliocene','Messinian','Tortonian','Upper Miocene','Serravallian','Langhian','Middle Miocene','Burdigalian','Aquitanian','Lower Miocene','Miocene','Neogene','Chattian','Rupelian','Oligocene','Priabonian','Bartonian','Lutetian','Ypresian','Eocene','Thanetian','Selandian','Danian','Paleocene','Paleogene','Cenozoic','Maastrichtian','Campanian','Santonian','Coniacian','Turonian','Cenomanian','Upper Cretaceous','Albian','Aptian','Barremian','Hauterivian','Valanginian','Berriasian','Lower Cretaceous','Cretaceous','Tithonian','Kimmeridgian','Oxfordian','Upper Jurassic','Callovian','Bathonian','Bajocian','Aalenian','Middle Jurassic','Toarcian','Pliensbachian','Sinemurian','Hettangian','Lower Jurassic','Jurassic','Rhaetian','Norian','Carnian','Upper Triassic','Ladinian','Anisian','Middle Triassic','Olenekian','Induan','Lower Triassic','Triassic','Mesozoic','Changhsingian','Wuchiapingian','Lopingian','Capitanian','Wordian','Roadian','Guadalupian','Kungurian','Artinskian','Sakmarian','Asselian','Cisuralian','Permian','Gzhelian','Kasimovian','Upper Pennsylvanian','Moscovian','Middle Pennsylvanian','Bashkirian','Lower Pennsylvanian','Pennsylvanian','Serpukhovian','Upper Mississippian','Visean','Middle Mississippian','Tournaisian','Lower Mississippian','Mississippian','Carboniferous','Famennian','Frasnian','Upper Devonian','Givetian','Eifelian','Middle Devonian','Emsian','Pragian','Lochkovian','Lower Devonian','Devonian','Pridoli','Ludfordian','Gorstian','Ludlow','Homerian','Sheinwoodian','Wenlock','Telychian','Aeronian','Rhuddanian','Llandovery','Silurian','Hirnantian','Katian','Sandbian','Upper Ordovician','Darriwilian','Dapingian','Middle Ordovician','Floian','Tremadocian','Lower Ordovician','Ordovician','Cambrian 10','Jiangshanian','Paibian','Furongian','Guzhangian','Drumian','Wuliuan','Miaolingian','Cambrian 4','Cambrian 3','Cambrian Series 2','Cambrian 2','Fortunian','Terreneuvian','Cambrian','Paleozoic','Phanerozoic','Ediacaran','Cryogenian','Tonian','Neoproterozoic','Stenian','Ectasian','Calymmian','Mesoproterozoic','Statherian','Orosirian','Rhyacian','Siderian','Paleoproterozoic','Proterozoic','unnamed1','Neoarchean','unnamed2','Mesoarchean','unnamed3','Paleoarchean','unnamed4','Eoarchean','Archean','Hadean','Precambrian','Late Holocene','Early Holocene','Late Pleistocene','Early Pleistocene','Late Pliocene','Early Pliocene','Late Miocene','Early Miocene','Late Cretaceous','Early Cretaceous','Late Jurassic','Early Jurassic','Late Triassic','Early Triassic','Late Pennsylvanian','Early Pennsylvanian','Late Mississippian','Early Mississippian','Late Devonian','Early Devonian','Late Ordovician','Early Ordovician')
  attr(out,"stratifill")<-stratifill
  return(out)
}##



##timebin() function
#' timebin data based on numeric age data
#' @param x a vector of numeric age data
#' @param strat stratigraphic table
#' @param lower lower border of intervals
#' @param upper upper border of intervals
#' @param def_level default hierarchy level in strat to take interval names from (default 4, for stages in the phanerozoic dataframe)
timebin<-function(x,strat,lower=NULL,upper=NULL,def_level=4){

if(inherits(strat,"geotimescale") | ("bottom"%in%colnames(strat))  & ("top"%in%colnames(strat))){
if(is.null(lower)) lower<-strat$bottom
if(is.null(upper)) upper<-strat$top
strat<-as.character(strat[,def_level])
}

if(is.numeric(x)){

y<-rep(NA,length(x))
for(i in 1:length(x)){
y_<-strat[lower>=x[i] & upper<=x[i]]

if(length(y_)==1) y_->y[i]
if(length(y_)>1) y_[1]->y[i]
if(length(y_)==0) NA->y[i]
}

}else{
#XXX future logic to convert named intervals to numeric time
}

return(y)
}


##plot.geotimescale()
#' plot method for a geological timescale
#' @param x a stratigraphic able, either an object of class 'geotimescale', or
#' @param time.convert optional; function to apply to the ages in x to convert them to plotting space (e.g. in the case of plotting alongside a phylogenetic tree.
#' @param fromto vector of two numbers giving the left and right (or bottom and top, if horiz==TRUE) margins to plot the timescale to
#' @param levels levels of hierarchy to plot as column indices in x
#' @param horiz logical whether to plot horizontally (default FALSE)
#' @param v verbosity setting (default FALSE)
#' @param rotate logical or numeric vector indicating rotaton the labels of time intervals for each level of hierarchy. Default (NULL) causes the all but the last level (horiz==FALSE) or only the last level (for horiz==TRUE) to be rotated 90°.
#' @param widths manual settings for column or row (for horiz==TRUE) widths (in relative numbers), overrides default settings
#' @param width_adjust adjustment of default width difference between rotated and non-rotated columns/rows
#' @param text_range range within which to plot text; text that falls outside this range is omitted (useful for excluding Quarternary stages)
#' @param add logical, add to existing plot? (default NULL will add to open plotting device if available, specify TRUE/FALSE to override)
#' @param stratifill logical; use stratifill attribute to automatically generate fill colors (default=TRUE)
#' @param label_top logical; plot labels after all polygons are plotted to keep them on top at all times? (default=FALSE)
#' @param alpha_fill alpha channel value to be applied to fill colors
#' @param ... additional parameters to pass on to plot and polygon
#' @return nothing, but adds the plotted time scale to the current plotting device
#' @export plot.geotimescale
#' @method plot.geotimescale
#' @importFrom paleoDiv add.alpha
#' @examples
plot.geotimescale<-function(x, time.convert=NULL, fromto=NULL, levels=c(1:4), horiz=FALSE, v=FALSE, rotate=NULL, widths=NULL, width_adjust=3, text_range=range(x$bottom), add=NULL, stratifill=TRUE, label_top=FALSE,alpha_fill=1,...){
list(...)->plot_args

between<-function(x,between) x<=max(between) & x>=min(between)

if(!inherits(x,"geotimescale")) new_geotimescale(x)->x
attr(x,"stratifill")->straticolors
if(!is.null(time.convert)){
time.convert(x$bottom)->x$bottom
time.convert(x$top)->x$top
}

#rotate text logic
if(is.null(rotate)){ rotate<-rep(TRUE,length(levels)) 
rotate[length(levels)]<-FALSE
if(horiz) !rotate->rotate
}
if(is.logical(rotate)){
rotate->rotatel
ifelse(rotate,90,0)->rotate
}else{
ifelse(rotate==0,FALSE,TRUE)->rotatel
}

if(is.null(add) & dev.cur() !=1 ) TRUE -> add else if(is.null(add) & dev.cur() == 1 ) FALSE -> add #check if plotting device already open

if(v) print(add)
#column or row widths
if(is.null(fromto)){ 
if(!add){c(0,1)->fromto

}else if(!horiz){
par("usr")->plotlim
abs(diff(range(plotlim[1:2])))->plotwidth
abs(diff(range(plotlim[3:4])))->plotheight
c(plotlim[1],plotlim[1]+plotwidth/8)->fromto

}else{
par("usr")->plotlim
abs(diff(range(plotlim[1:2])))->plotwidth
abs(diff(range(plotlim[3:4])))->plotheight

c(plotlim[3],plotlim[3]+plotheight/8)->fromto 


}
}

length(levels)->nlev
abs(diff(range(fromto)))->width
if(v) print(fromto)
if(v) print(width)

if(is.null(widths)){ 
ifelse(rotatel, 1/nlev/width_adjust,1/nlev*width_adjust)->widths

if(horiz) ifelse(!rotatel, 1/nlev/width_adjust,1/nlev*width_adjust)->widths
if(v) print(widths)
}
widths/sum(widths)*width->widths
if(fromto[2]<fromto[1]) widths<-widths*-1
if(v) print(widths)

#plotting
if(!add){
plot_args->plot_args1

if(!("xlim"%in%names(plot_args))) plot_args1$xlim<-fromto
if(!("xlim"%in%names(plot_args)) && horiz==TRUE) plot_args1$xlim<-rev(range(c(x$bottom,x$top)))

if(!("ylim"%in%names(plot_args))) plot_args1$ylim<-rev(range(c(x$bottom,x$top)))
if(!("ylim"%in%names(plot_args)) && horiz==TRUE) plot_args1$ylim<-fromto

if(!("xlab"%in%names(plot_args))) plot_args1$xlab<-""
if(!("ylab"%in%names(plot_args))) plot_args1$ylab<-""

if(!("xaxt"%in%names(plot_args)) && horiz==FALSE) plot_args1$xaxt<-"n"
if(!("yaxt"%in%names(plot_args)) && horiz==TRUE) plot_args1$yaxt<-"n"
if(!("bty"%in%names(plot_args)) && horiz==TRUE) plot_args1$bty<-"n"

plot_args1$type<-"n"
plot_args1$y<-NA
plot_args1$x<-NA

do.call(plot, plot_args1)
par("usr")->plotlim
abs(diff(range(plotlim[1:2])))->plotwidth
abs(diff(range(plotlim[3:4])))->plotheight
}

if(add & v) message("adding to existing plot") else if(v) message("creating standalone plot with par(\"usr\") = ",paste(plotlim,collapse=" "))

for(i in levels){ #loop through strat. hierarchy
which(levels==i)->i_

sum(widths[1:i_])+fromto[1]->lmar
if(i_>1) sum(widths[1:(i_-1)])+fromto[1]->umar else fromto[1]->umar

tapply(x$bottom,factor(x[,i]),FUN=max)->maxes
tapply(x$top,factor(x[,i]),FUN=min)->mins
mins[order(maxes)]->mins
maxes[order(maxes)]->maxes

names(maxes)->nam

for(j in 1:length(nam)){ #loop through each stratigraphic unit in given level of hierarchy

plot_args2<-plot_args

if(horiz){#horizontal plot
plot_args2$y<-c(umar, umar, lmar, lmar)
plot_args2$x<-c(maxes[j],mins[j],mins[j],maxes[j])
texthor<-mean(c(maxes[j], mins[j], mins[j], maxes[j]))
textvert<-mean(c(umar, umar, lmar, lmar))
}else{#vertical plot
plot_args2$x<-c(umar, umar, lmar, lmar)
plot_args2$y<-c(maxes[j],mins[j],mins[j],maxes[j])
texthor<-mean(c(umar, umar, lmar, lmar))
textvert<-mean(c(maxes[j], mins[j], mins[j], maxes[j]))
}

if(stratifill && "stratifill"%in%names(attributes(x))){
if(nam[j] %in% names(straticolors)) plot_args2$col<-paleoDiv::add.alpha(straticolors[nam[j]],alpha_fill) else plot_args2$col<-paleoDiv::add.alpha("lightgrey",alpha_fill)

do.call(polygon, plot_args2)

}else{
if("fill"%in%names(plot_args2) & !("col"%in%names(plot_args2))) plot_args2$col<-plot_args2$fill

if("col"%in%names(plot_args2)){
if(length(plot_args2$col)<length(nam)) rep(plot_args2$col,length(nam))[j]->plot_args2$col

paleoDiv::add.alpha(plot_args2$col,alpha_fill)->plot_args2$col

if(v) message(nam[j])
#alternate colors if vector 
}

do.call(polygon, plot_args2)
}

if(!is.null(text_range) && length(text_range>1)){
if(!label_top && between(mean(c(maxes[j], mins[j], mins[j], maxes[j])),text_range)) text(x=texthor,y=textvert, nam[j], srt=rotate[i_], ... )
}
}#end loop through intervals 1 (polygons)

if(!is.null(text_range) && length(text_range>1)){
if(label_top) for(j in 1:length(nam)){ #loop through each stratigraphic unit in given level of hierarchy for labelling

plot_args2<-plot_args

if(horiz){#horizontal plot
plot_args2$y<-c(umar, umar, lmar, lmar)
plot_args2$x<-c(maxes[j],mins[j],mins[j],maxes[j])
texthor<-mean(c(maxes[j], mins[j], mins[j], maxes[j]))
textvert<-mean(c(umar, umar, lmar, lmar))
}else{#vertical plot
plot_args2$x<-c(umar, umar, lmar, lmar)
plot_args2$y<-c(maxes[j],mins[j],mins[j],maxes[j])
texthor<-mean(c(umar, umar, lmar, lmar))
textvert<-mean(c(maxes[j], mins[j], mins[j], maxes[j]))
}


if(between(mean(c(maxes[j], mins[j], mins[j], maxes[j])),text_range)) text(x=texthor,y=textvert, nam[j], srt=rotate[i_], ... )
}#end loop through intervals 2 (labels)
}

}#end loop through hierarchy

}##


##data:
#based on Int’l Chronostratigraphic Chart v2024/10, see https://stratigraphy.org/
phanerozoic<-read.csv(text="era,period,epoch,stage,bottom
Cenozoic,Quaternary,Holocene,Meghalayan,0.0042
Cenozoic,Quaternary,Holocene,Northgrippian,0.0082
Cenozoic,Quaternary,Holocene,Greenlandian,0.0117
Cenozoic,Quaternary,Pleistocene,Late Pleistocene,0.129
Cenozoic,Quaternary,Pleistocene,Chibanian,0.774
Cenozoic,Quaternary,Pleistocene,Calabrian,1.8
Cenozoic,Quaternary,Pleistocene,Gelasian,2.58
Cenozoic,Neogene,Pliocene,Piacenzian,3.6
Cenozoic,Neogene,Pliocene,Zanclean,5.333
Cenozoic,Neogene,Miocene,Messinian,7.246
Cenozoic,Neogene,Miocene,Tortonian,11.63
Cenozoic,Neogene,Miocene,Serravallian,13.82
Cenozoic,Neogene,Miocene,Langhian,15.98
Cenozoic,Neogene,Miocene,Burdigalian,20.45
Cenozoic,Neogene,Miocene,Aquitanian,23.04
Cenozoic,Paleogene,Oligocene,Chattian,27.3
Cenozoic,Paleogene,Oligocene,Rupelian,33.9
Cenozoic,Paleogene,Eocene,Priabonian,37.71
Cenozoic,Paleogene,Eocene,Bartonian,41.03
Cenozoic,Paleogene,Eocene,Lutetian,48.07
Cenozoic,Paleogene,Eocene,Ypresian,56
Cenozoic,Paleogene,Paleocene,Thanetian,59.24
Cenozoic,Paleogene,Paleocene,Selandian,61.66
Cenozoic,Paleogene,Paleocene,Danian,66
Mesozoic,Cretaceous,Late Cretaceous,Maastrichtian,72.2
Mesozoic,Cretaceous,Late Cretaceous,Campanian,83.6
Mesozoic,Cretaceous,Late Cretaceous,Santonian,85.7
Mesozoic,Cretaceous,Late Cretaceous,Coniacian,89.8
Mesozoic,Cretaceous,Late Cretaceous,Turonian,93.9
Mesozoic,Cretaceous,Late Cretaceous,Cenomanian,100.5
Mesozoic,Cretaceous,Early Cretaceous,Albian,113.2
Mesozoic,Cretaceous,Early Cretaceous,Aptian,121.4
Mesozoic,Cretaceous,Early Cretaceous,Barremian,125.77
Mesozoic,Cretaceous,Early Cretaceous,Hauterivian,132.6
Mesozoic,Cretaceous,Early Cretaceous,Valanginian,137.05
Mesozoic,Cretaceous,Early Cretaceous,Berriasian,143.1
Mesozoic,Jurassic,Late Jurassic,Tithonian,149.2
Mesozoic,Jurassic,Late Jurassic,Kimmeridgian,154.8
Mesozoic,Jurassic,Late Jurassic,Oxfordian,161.5
Mesozoic,Jurassic,Middle Jurassic,Callovian,165.3
Mesozoic,Jurassic,Middle Jurassic,Bathonian,168.3
Mesozoic,Jurassic,Middle Jurassic,Bajocian,170.3
Mesozoic,Jurassic,Middle Jurassic,Aalenian,174.7
Mesozoic,Jurassic,Early Jurassic,Toarcian,182.7
Mesozoic,Jurassic,Early Jurassic,Pliensbachian,190.8
Mesozoic,Jurassic,Early Jurassic,Sinemurian,199.3
Mesozoic,Jurassic,Early Jurassic,Hettangian,201.4
Mesozoic,Triassic,Late Triassic,Rhaetian,205.7
Mesozoic,Triassic,Late Triassic,Norian,227.3
Mesozoic,Triassic,Late Triassic,Carnian,237
Mesozoic,Triassic,Middle Triassic,Ladinian,241.464
Mesozoic,Triassic,Middle Triassic,Anisian,247.2
Mesozoic,Triassic,Early Triassic,Olenekian,249.9
Mesozoic,Triassic,Early Triassic,Induan,251.9
Paleozoic,Permian,Lopingian,Changhsingian,254.14
Paleozoic,Permian,Lopingian,Wuchiapingian,259.8
Paleozoic,Permian,Guadalupian,Capitanian,265.1
Paleozoic,Permian,Guadalupian,Wordian,268.8
Paleozoic,Permian,Guadalupian,Roadian,274.4
Paleozoic,Permian,Cisuralian,Kungurian,272.3
Paleozoic,Permian,Cisuralian,Artinskian,283.5
Paleozoic,Permian,Cisuralian,Sakmarian,295
Paleozoic,Permian,Cisuralian,Asselian,298.9
Paleozoic,Carboniferous,Pennsylvanian,Gzhelian,303.7
Paleozoic,Carboniferous,Pennsylvanian,Kasimovian,307
Paleozoic,Carboniferous,Pennsylvanian,Moscovian,315.2
Paleozoic,Carboniferous,Pennsylvanian,Bashkirian,323.2
Paleozoic,Carboniferous,Mississippian,Serpukhovian,330.9
Paleozoic,Carboniferous,Mississippian,Visean,346.7
Paleozoic,Carboniferous,Mississippian,Tournaisian,358.9
Paleozoic,Devonian,Late Devonian,Famennian,372.2
Paleozoic,Devonian,Late Devonian,Frasnian,382.7
Paleozoic,Devonian,Middle Devonian,Givetian,387.7
Paleozoic,Devonian,Middle Devonian,Eifelian,393.3
Paleozoic,Devonian,Early Devonian,Emsian,407.6
Paleozoic,Devonian,Early Devonian,Pragian,410.8
Paleozoic,Devonian,Early Devonian,Lochkovian,419.62
Paleozoic,Silurian,Pridoli,Pridoli,422.7
Paleozoic,Silurian,Ludlow,Ludfordian,425
Paleozoic,Silurian,Ludlow,Gorstian,426.7
Paleozoic,Silurian,Wenlock,Homerian,430.6
Paleozoic,Silurian,Wenlock,Sheinwoodian,432.9
Paleozoic,Silurian,Llandovery,Telychian,438.6
Paleozoic,Silurian,Llandovery,Aeronian,440.5
Paleozoic,Silurian,Llandovery,Rhuddanian,443.1
Paleozoic,Ordovician,Late Ordovician,Hirnantian,445.2
Paleozoic,Ordovician,Late Ordovician,Katian,452.8
Paleozoic,Ordovician,Late Ordovician,Sandbian,458.2
Paleozoic,Ordovician,Middle Ordovician,Darriwilian,469.4
Paleozoic,Ordovician,Middle Ordovician,Dapingian,471.3
Paleozoic,Ordovician,Early Ordovician,Floian,477.1
Paleozoic,Ordovician,Early Ordovician,Tremadocian,486.85
Paleozoic,Cambrian,Furongian,Cambrian 10,491
Paleozoic,Cambrian,Furongian,Jiangshanian,494.2
Paleozoic,Cambrian,Furongian,Paibian,497
Paleozoic,Cambrian,Miaolingian,Guzhangian,500.5
Paleozoic,Cambrian,Miaolingian,Drumian,504.5
Paleozoic,Cambrian,Miaolingian,Wuliuan,506.5
Paleozoic,Cambrian,Cambrian Series 2,Cambrian 4,514.5
Paleozoic,Cambrian,Cambrian Series 2,Cambrian 3,521
Paleozoic,Cambrian,Terreneuvian,Cambrian 2,529
Paleozoic,Cambrian,Terreneuvian,Fortunian,538.8
Neoproterozoic,Ediacaran,Ediacaran,Ediacaran,635
Neoproterozoic,Cryogenian,Cryogenian,Cryogenian,720")

phanerozoic<-new_geotimescale(phanerozoic)

#example usage:
#plot(phanerozoic,horiz=T,add=F,border="white",lwd=0.5,col=add.alpha("black"), text_range=c(600,15))
#paste(sort(tapply(phanerozoic$bottom,phanerozoic$period,FUN=max)),collapse=",")

#paste(phanerozoic$stage,collapse="','")
#paste(phanerozoic$bottom,collapse=",")
#paste(attr(phanerozoic,"stratifill")[phanerozoic$stage],collapse="','")
