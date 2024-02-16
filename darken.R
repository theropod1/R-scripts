##function
#darken or lighten any colour given as col by either adding to, subtracting from or replacing the value in the value-channel.
darken<-function(col, add=0, abs=NULL){
  if(missing(col))
    stop("Please provide a vector of colours.")
    
    character(length(col))->out
    
    for(i in 1:length(col)){
    rgb2hsv(col2rgb(add.alpha(col[i], 1)))->tmpcol
    
    if(!is.null(abs)){
    tmpcol[3]<-abs
    }else{
    tmpcol[3]<-tmpcol[3]+add
    }
    hsv(tmpcol[1], tmpcol[2], tmpcol[3])->out[i]
    
    }
    
    return(out)
 
}
##

##function:
#wrapper around darken, applicable to vectors of abs or add
darken_<-function(x, add=0, abs=NULL){

if(length(add)==1 & is.null(abs)){#if add is given as a single value
darken(x,add)
}else if(!is.null(abs) & length(abs)==1){#if abs is given as a single value
darken(x,abs=abs)
}else if(!is.null(abs) & length(abs)>1 & length(x)==1){#if abs is given and a vector
length(abs)->n
character(n)->out
for(i in 1:n){
out[i]<-darken(x, abs=abs[i])}
return(out)
}else if(length(add)>1 & is.null(abs) & length(x)==1){#if add is given as a vector
length(add)->n
character(n)->out
for(i in 1:n){
out[i]<-darken(x, add=add[i])}
return(out)
}else if(length(add)>1 & is.null(abs) & length(x)>1){#if add is given as a vector and x is a vector too
length(x)->n
character(n)->out
if(length(add)!=n){error("colour and change vectors need to be the same length")}
for(i in 1:n){
out[i]<-darken(x[i], add=add[i])}
return(out)
}else if(!is.null(abs) & length(abs)>1 & length(x)==1){#if abs is given and a vector, and x is a vector too
length(x)->n
character(n)->out
if(length(abs)!=n){error("colour and change vectors need to be the same length")}
for(i in 1:n){
out[i]<-darken(x[i], abs=abs[i])}
return(out)
}

}
##
