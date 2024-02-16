##function: 
#manually plot a violin plot for data x at position pos. ... are options to be passed on to density(). Cutoff gives values at which to cut off the tails of the distribution, either as a range (if length(cutoff)>1) or as a z score. Dscale needs to be adjusted manually to fit the desired plot.

viol<-function(x, pos, dscale=1, cutoff=range(x), horiz=T, add=T, fill="grey", col="black", lwd=1,...){

density(x,...)->d

if(length(cutoff)==1){#if sd given for cutoff
as.numeric(d$y[which(abs(d$x-mean(x,na.rm=T))/sd(x,na.rm=T)<cutoff)])/2*dscale->dstat #scaled density statistic to contruct violin
as.numeric(d$x[which(abs(d$x-mean(x,na.rm=T))/sd(x,na.rm=T)<cutoff)])->xstat #independent variable
}

else if(length(cutoff>1)){#if range is given for cutoff
as.numeric(d$y[which(d$x<=max(cutoff) & d$x>=min(cutoff))])/2*dscale->dstat#scaled density statistic to contruct violin
as.numeric(d$x[which(d$x<=max(cutoff) & d$x>=min(cutoff))])->xstat#independent variable
}

#plotting
if(add==T){
if(horiz==T){#plot horizontally
polygon(y=c(pos+dstat,rev(pos-dstat)),x=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd)
}else(#plot vertically
polygon(x=c(pos+dstat,rev(pos-dstat)),y=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd)
)

}else{
if(horiz==T){

plot(density(x), xlim=range(xstat), ylim=c(pos+max(dstat), pos-max(dstat)), type="n")

polygon(y=c(pos+dstat,rev(pos-dstat)),x=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd, ylab="Density", xlab="Value")

}else{
plot(density(x), ylim=range(xstat), xlim=c(pos+max(dstat), pos-max(dstat)), type="n", xlab="Density", ylab="Value")

polygon(x=c(pos+dstat,rev(pos-dstat)),y=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd)
}
}


}
##

