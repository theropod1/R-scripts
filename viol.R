##function: 
#manually plot a violin plot for data x at position pos. ... are options to be passed on to density(). Cutoff gives values at which to cut off the tails of the distribution, either as a range (if length(cutoff)>1) or as a z score. Dscale needs to be adjusted manually to fit the desired plot. ... gives additional options to be passed on to density, or (if overridden) any other plotting function, after its first option. x is the x values at which plotting is to take place, unless the function supplied for stat (e.g. density) already outputs a data.frame with its own x values alongside the d values

viol<-function(x, pos, x2=NULL, stat=density, dscale=1, cutoff=range(x), horiz=T, add=T, fill="grey", col="black", lwd=1,...){
#sort ascending
if(!is.null(x2) & is.numeric(x2)){
x2[order(x)]->x2
}
x[order(x)]->x

#calculate plotting statistic. defaults to density, but other functions can be used by altering the stat parameter
if(is.numeric(stat)){#if stat is a vector 
if(length(stat)==length(x)){
stat->d}else{stop("If stat is numeric(), it has to be the same length as x")}
}else{#if stat is a function
if(!is.null(x2) & length(x2)==length(x)){
stat(x2,...)->d
}else{
stat(x,...)->d
}
}

if(is.numeric(d)){#if d is a numeric vector, i.e. not a data.frame() as is output by density()
d<-data.frame(y=d, x=x)#make data.frame if not already existing. Otherwise it is assumed that there are d$y (giving the plotting statistic) and d$x (giving the independent variable) in the object
}

if(length(cutoff)==1){#if z score is given for cutoff, convert it into range around mean
cutoff<-c(mean(x,na.rm=T)-cutoff*sd(x,na.rm=T),mean(x,na.rm=T)+cutoff*sd(x,na.rm=T))
}

#now crop data range to match cutoff range
as.numeric(d$y[which(d$x<=max(cutoff) & d$x>=min(cutoff))])->dstat0
dstat0[!is.na(dstat0)]->dstat0_#remove NAs
dstat0_/2*dscale->dstat#scaled statistic to contruct violin

as.numeric(d$x[which(d$x<=max(cutoff) & d$x>=min(cutoff))])->xstat
xstat[!is.na(dstat0)]->xstat
#independent variable

##plotting
if(add==T){#add to an existing plot
if(horiz==T){#plot horizontally
polygon(y=c(pos+dstat,rev(pos-dstat)),x=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd)
}else(#plot vertically
polygon(x=c(pos+dstat,rev(pos-dstat)),y=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd)
)

}else{#create a new plot
if(horiz==T){
plot(density(x), xlim=range(xstat), ylim=c(pos+max(dstat), pos-max(dstat)), type="n")

polygon(y=c(pos+dstat,rev(pos-dstat)),x=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd, ylab="Density", xlab="Value")

}else{
plot(density(x), ylim=range(xstat), xlim=c(pos+max(dstat), pos-max(dstat)), type="n", xlab="Density", ylab="Value")

polygon(x=c(pos+dstat,rev(pos-dstat)),y=c(xstat,rev(xstat)), border=col, col=fill, lwd=lwd)
}
}

#return invisible plot object
invisible(data.frame(o_statistic=c(dstat0_,rev(dstat0_)),plot_statistic=c(pos+dstat,rev(pos-dstat)),independent=c(xstat,rev(xstat))))


}
##

