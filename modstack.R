###function modstack()
modstack<-function(coo, indices=c(1:length(coo)), col=NULL, lwd=1, lty=1, xlim=NULL, ylim=NULL, alpha=1, xlab="", ylab="", xpd=FALSE, add=FALSE, asp=1,...){
par("xpd")->oxpd
par(xpd=xpd)

if(inherits(what="Coo",outl)){
dplyr::pull(unclass(coo)$fac[,1])[indices]->fac
coo$coo[indices]->coo
}

if(is.null(col)){
col<-col_asign(fac,viridis::viridis)
}


if(add==FALSE){
##find plot limits
as.data.frame(coo[1])->c

prange<-c(min(c[,1],na.rm=T),max(c[,1],na.rm=T),min(c[,2],na.rm=T),max(c[,2],na.rm=T))

for(i in 2:length(coo)){
as.data.frame(c)->c

min(c[,1],na.rm=T)->minx
max(c[,1],na.rm=T)->maxx
min(c[,2],na.rm=T)->miny
max(c[,2],na.rm=T)->maxy

min(c(prange[1], minx))->prange[1]
max(c(prange[2], maxx))->prange[2]
min(c(prange[3], miny))->prange[3]
max(c(prange[4], maxy))->prange[4]

}
##actual plotting

if(is.null(xlim)){
prange[1:2]->xlim
}
if(is.null(ylim)){
prange[3:4]->ylim
}

plot(x=as.data.frame(coo[1])[,1],y=as.data.frame(coo[1])[,2], type="n", xlim=prange[1:2], ylim=prange[3:4],asp=asp, xlab=xlab,ylab=ylab,...)#empty plot
}

#plotting vector prep
if(length(col)<length(coo)){
rep(col, length(coo))->col
}

if(length(alpha)<length(coo)){
rep(alpha, length(coo))->alpha
}

if(length(lwd)<length(coo)){
rep(lwd, length(coo))->lwd
}

if(length(lty)<length(coo)){
rep(lty, length(coo))->lty
}


##plot polygons
for(i in 1:length(coo)){
as.data.frame(coo[i])->c
polygon(x=c[,1], y=c[,2], border=paleoDiv::add.alpha(col[i],alpha[i]), lwd=lwd[i], lty=lty[i])

}

par(xpd=oxpd)
}


##################


###function proc.viz()
proc.viz<-function(gpagen,links=NULL, mean=TRUE, meancol="black", meancex=1.5, meanpch=16, meanlwd=2, col="grey", col.line=col,lty=NA,lwd=1, pch=16,xlab="x",ylab="y",xlim=NULL,ylim=NULL,...){

if(inherits(gpagen,"gpagen")) gpagen$coords->gpagen

if(!inherits(gpagen,"array")) stop("gpagen must be an array or gpagen-type object!")

##vectorize specimen settings
if(length(col)<dim(gpagen)[3]) rep(col,dim(gpagen)[3])[1:dim(gpagen)[3]]->col
if(length(pch)<dim(gpagen)[3]) rep(pch,dim(gpagen)[3])[1:dim(gpagen)[3]]->pch
if(length(col.line)<dim(gpagen)[3]) rep(col.line,dim(gpagen)[3])[1:dim(gpagen)[3]]->col.line
if(length(lty)<dim(gpagen)[3]) rep(lty,dim(gpagen)[3])[1:dim(gpagen)[3]]->lty
if(length(lwd)<dim(gpagen)[3]) rep(lwd,dim(gpagen)[3])[1:dim(gpagen)[3]]->lwd

##fix matrix, if present
if(!is.null(links)){
if(is.null(dim(links)[2])) matrix(as.numeric(links), ncol=2, byrow=T)->links
}

##set limits, if unset
if(is.null(xlim)) range(gpagen[,1,])->xlim
if(is.null(ylim)) range(gpagen[,2,])->ylim

##start plotting
plot(plot(proc.comb$coords[,,1]), type="n", ylab=ylab, xlab=xlab, xlim=xlim, ylim=ylim, asp=1,...)

for(i in 1:dim(gpagen)[3]){##loop through specimens
gpagen[,,i]->specimen
points(specimen[,1], specimen[,2], col=col[i], pch=pch[i],...)

if(!is.null(links)){##loop through links
for(j in 1:nrow(links)){
lines(x=specimen[links[j,],1],y=specimen[links[j,],2], col=col.line[i], lwd=lwd[i], lty=lty[i],...)
}}##end linkages

}

if(mean){##add mean shape
geomorph::mshape(gpagen)->msh
points(msh[,1], msh[,2], col=meancol, cex=meancex, pch=meanpch)

if(!is.null(links)){##loop through links
for(j in 1:nrow(links)){
lines(x=msh[links[j,],1],y=msh[links[j,],2], lwd=meanlwd, col=meancol)
}}##end linkages

}


}
