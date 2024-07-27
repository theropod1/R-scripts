jitterp<-function(x,y,width,col="black",alpha=0.5,...){

if(length(y)==1){
y<-rep(y,length(x))+rnorm(n=length(x), mean=0, sd=width)
}else if(length(x)==1){
x<-rep(x,length(y))+rnorm(n=length(y), mean=0, sd=width)
}else{
stop("Please make sure either x or y is length()==1")
}

points(x=x, y=y,col=paleoDiv::add.alpha(col,alpha),...)

invisible(data.frame(x,y))

}
