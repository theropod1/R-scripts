dext<-function(x, year=c(1,4), month=c(5,6), day=1){
numeric()->dates

for(i in 1:length(x)){

substr(x[i],year[1],year[2])->yr
substr(x[i],month[1],month[2])->mo

if(length(day)==2){
substr(x[i],day[1],day[2])->d
}else{day->d}

if(is.numeric(d)){

if(as.numeric(yr) %% 4 ==0){
maxd<-c(31,29,31,30,31,30,31,31,30,31,30,31)
}else{
maxd<-c(31,28,31,30,31,30,31,31,30,31,30,31)
}

if(d>maxd[as.numeric(mo)]){d<-maxd[as.numeric(mo)]}

if(d<10){
d<-as.character(paste0("0",d))
}else{d<-as.character(d)
}

}

dates[i]<-paste(yr,mo,d, sep="-")

}

return(as.Date(dates))

}

#extract month
MM<-function(x){as.numeric(substr(as.character(x),6,7))}
YY<-function(x){as.numeric(substr(as.character(x),1,4))}
