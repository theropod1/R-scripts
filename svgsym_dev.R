##
svgsym<-function(path){
if(!"tempsym"%in%list.files()) dir.create("tempsym")
rsvg::rsvg_svg(path,"tempsym/tmp12345.svg")
return(grImport2::readPicture("tempsym/tmp12345.svg"))
}##

##
spoints<-function(x,y=NULL, data=NULL, pch=NULL, cex=1, col=par("fg"), alpha=0.5, v=FALSE,...){
if(is.null(data) || nrow(data)>0){
if(is.null(y)){#extract variables
if(is.data.frame(x) | is.matrix(x)){
y<-x[,2]
x<-x[,1]

}else if(inherits(x,"formula")){

if(is.numeric(x[[2]]) && is.numeric(x[[3]])){
as.numeric(x[[2]])->y
as.numeric(x[[3]])->x
}else{
if(!is.null(data) && is.data.frame(data)) model.frame(x,data=data)->mf else model.frame(x)->mf
mf[,1]->y
mf[,2]->x
}
}
}

if(v) message(paste(y,x,sep="~",collapse=","))

if(is.null(pch) || is.numeric(pch) || is.character(pch)){
if(v) message("no symbol of class Picture supplied, switching to ordinary points() function")

if(is.null(pch)) pch<-1
points(x=x,y=y,data=data,pch=pch,cex=cex,type="p",col=add.alpha(col,alpha),...) #plot regular points

}else if(inherits(pch,"Picture")){
u   <- par("usr")
plt <- par("plt")

# Make a grid viewport corresponding exactly to the base plot
grid::pushViewport(grid::viewport(
        x = grid::unit(mean(plt[1:2]), "npc"),
        y = grid::unit(mean(plt[3:4]), "npc"),
        width  = grid::unit(diff(plt[1:2]), "npc"),
        height = grid::unit(diff(plt[3:4]), "npc"),
        xscale = u[1:2],
        yscale = u[3:4]))

pch@content[[1]]@content[[1]]@gp$fill<-pch@content[[1]]@content[[1]]@gp$col<-paleoDiv::add.alpha(col,alpha) # change symbol color as needed

grImport2::grid.symbols(pch, x = x, y = y, size = grid::unit(0.05*cex, "npc"),just=c(0.5,0.5),...)#plot symbols
grid::popViewport() # remove viewport to allow repeat plotting if needed
}
}}##

#fosssym<-list()
#svgsym("pch/ichno.svg")->fosssym$ichno
#svgsym("pch/bone.svg")->fosssym$bone
#svgsym("pch/copro.svg")->fosssym$copro
#svgsym("egg.svg")->fosssym$egg

#save(fosssym,file="fossym.RData")
#load("fossym.RData")


#grid::viewport(xscale=u[1:2],yscale=u[3:4])
#grid.symbols(psym,x = gg$lng, y = gg$lat,size = grid::unit(0.01, "npc"),just=c(0.5,1))

