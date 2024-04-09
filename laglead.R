##lag analysis
laglead<-function(x,y=NULL,ordinate=NULL, shifts=c(-20:20),plot=TRUE,polycol=add.alpha("black",0.1),polycol2=add.alpha("white",0.1),linecol="black",plotmains=c("A","B","C"), markers=NULL, markercols=c("green","blue"), markerlty=c(1,1),...){

length(shifts)->len
is<-numeric()
spearmans<-numeric()
pearsons<-matrix(ncol=3,nrow=len)#numeric()
kendalls<-numeric()

if(is.data.frame(x) & "y" %in% colnames(x) & "x" %in% colnames(x) & is.null(y)){
dat<-data.frame(x=x$x, y=x$y)
}else if(length(y)!=length(x)){
errmsg<-paste("Found for length(y) =",length(y),",length(x) =",length(x))
stop(paste("aborting, x and y arent’t same length.",errmsg))
}else{
dat<-data.frame(x=x, y=y)
}


for(r in c(1:len)){
shifts[r]->i

x<-rep(NA,length(dat$x))
y<-rep(NA,length(dat$x))

##STABLE only works for evenly spaced AND ordered time series
if(is.null(ordinate)){if(i>0){

message("shift=",i,">0, appending NAs to front of temperature, i.e. CO2 lags temperature")

for(j in 1:length(dat$x)){
x[j]<-dat$x[j]
y[j]<-c(rep(NA,abs(i)),dat$y)[j]
}
}else if(i<0){
message("shift=",i,"<0, appending NAs to front of CO2, i.e. CO2 leads temperature")

for(j in 1:length(dat$x)){
x[j]<-c(rep(NA,abs(i)),dat$x)[j]
y[j]<-dat$y[j]
}
}else{
message("shift=",i,", i.e. no lag, using data as is")
x<-dat$x
y<-dat$y
}
}##STABLE

##DEV
if(!is.null(ordinate)){
for(j in 1:nrow(dat)){

x[j]<-dat$x[j]
y[which(ordinate==ordinate[j]+i)]<-dat$y[j]

}
}##end DEV

x_<-x
y_<-y

x<-x[!is.na(y)]
y<-y[!is.na(y)]

y<-y[!is.na(x)]
x<-x[!is.na(x)]

kendalls[r] <- as.numeric(cor.test(x, y, method = 'kendall')$estimate)

spearmans[r] <- as.numeric(cor.test(x, y, method = 'spearman')$estimate)

pearsons[r,] <- c(as.numeric(cor.test(x, y, method = 'pearson')$estimate),as.numeric(cor.test(x, y, method = 'pearson')$conf.int))

is[r]<-i

}#end loop through i and r

data.frame(shift=is, pearsons_point=pearsons[,1], pearsons_lwr=pearsons[,2],pearsons_upr=pearsons[,3], spearmans, kendalls)->outdf

if(plot==TRUE){

if(par("mfrow")[2]<3){
par(mfrow=c(1,3))}

plot(pearsons[,1]~is,type="l", main=plotmains[1], ylab="Correlation: Pearson’s r",xlab="Shift [x]",lwd=2,col=linecol,...)
polygon(x=c(par("usr")[2],0,0,par("usr")[2]),y=c(0,0,1,1),col=polycol2,border=NA,lty=2,...)
polygon(x=c(par("usr")[1],0,0,par("usr")[1]),y=c(0,0,1,1),col=polycol,lty=2,...)
text(mean(c(par("usr")[1],0)),par("usr")[3]+diff(range(par("usr")[3:4]))/40,adj=c(0.5,0), "x leads y",col=add.alpha(polycol,1),...)
text(mean(c(par("usr")[2],0)),par("usr")[3]+diff(range(par("usr")[3:4]))/40,adj=c(0.5,0), "x lags y",col=add.alpha(polycol2,1),...)

#confidence band
lines(pearsons[,2]~is,type="l", lty=2,col=linecol,...)
lines(pearsons[,3]~is,type="l", lty=2,col=linecol,...)

if(!is.null(markers)){#add marker lines
abline(v=markers, col=markercols, lty=markerlty)
}

plot(spearmans~is,type="l", main=plotmains[2], ylab="Correlation: Spearman’s rho",xlab="Shift [x]",lwd=2,col=linecol,...)
polygon(x=c(par("usr")[2],0,0,par("usr")[2]),y=c(0,0,1,1),col=polycol2,border=NA,lty=2,...)
polygon(x=c(par("usr")[1],0,0,par("usr")[1]),y=c(0,0,1,1),col=polycol,lty=2,...)

text(mean(c(par("usr")[1],0)),par("usr")[3]+diff(range(par("usr")[3:4]))/40,adj=c(0.5,0), "x leads y",col=add.alpha(polycol,1),...)
text(mean(c(par("usr")[2],0)),par("usr")[3]+diff(range(par("usr")[3:4]))/40,adj=c(0.5,0), "x lags y",col=add.alpha(polycol2,1),...)

if(!is.null(markers)){#add marker lines
abline(v=markers, col=markercols, lty=markerlty)
}

plot(kendalls~is,type="l", main=plotmains[3], ylab="Correlation: Kendall’s tau",xlab="Shift [x]",lwd=2,col=linecol,...)

polygon(x=c(par("usr")[2],0,0,par("usr")[2]),y=c(0,0,1,1),col=polycol2,border=NA,lty=2,...)
polygon(x=c(par("usr")[1],0,0,par("usr")[1]),y=c(0,0,1,1),col=polycol,lty=2,...)

if(!is.null(markers)){#add marker lines
abline(v=markers, col=markercols, lty=markerlty)
}

text(mean(c(par("usr")[1],0)),par("usr")[3]+diff(range(par("usr")[3:4]))/40,adj=c(0.5,0), "x leads y",col=add.alpha(polycol,1),...)
text(mean(c(par("usr")[2],0)),par("usr")[3]+diff(range(par("usr")[3:4]))/40,adj=c(0.5,0), "x lags y",col=add.alpha(polycol2,1),...)

}

return(outdf)

}
#to do: expand to be able to take into account irregularly spaced sequences of x values
#e.g. x[dat$x==x+i]<-dat$y or similar

##graphics functions
cptd<-function(times=1){par("usr")[3]+times*diff(par("usr")[3:4])}

cplr<-function(times=1){par("usr")[2]+times*diff(par("usr")[1:2])}


