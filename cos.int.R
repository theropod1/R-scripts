##function: 
#cosine interpolation for a vector of input values x. lower.int and upper.int give the values to which to interpolate (maxima of cosine function), lower_orig and upper_orig the corresponding original values. If single_transition==FALSE, the rest of the cosine wave outside of the interval between lower and upper_orig is kept, otherwise all values above upper_orig are replaced by upper.int, and all values below lower_orig by lower.int.


cos.int<-function(x,lower_orig=min(x),lower.int=0, upper_orig=max(x), upper.int=1, single_transition=TRUE){
#cos(x)#0 at pi, 1 at 2pi
abs(upper_orig-lower_orig)->r
abs(upper.int-lower.int)->r_

pi/r->cf

if(lower.int>upper.int){
x_<-(x-lower_orig)*cf+2*pi

cos(x_)/2*r_+0.5+min(c(lower.int,upper.int))->interpolation
}else{
x_<-(x-lower_orig)*cf+pi

cos(x_)/2*r_+0.5+min(c(lower.int,upper.int))->interpolation}

if(single_transition==TRUE){
for(i in 1:length(interpolation)){
if(x[i]<lower_orig){
interpolation[i]<-lower.int
}else if(x[i]>upper_orig){
interpolation[i]<-upper.int
}

}
}

return(interpolation)
}
##
