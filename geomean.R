##function: 
#calculate the geometric mean for a numeric() vector x.

geomean<-function(x){
if(is.numeric(x)==T){#tests that input is numeric
    if(length(x)>=2){#tests that input contains more than one value
        prod(x)^(1/length(x))->out#multiplies all values, then takes the nth root
        return(out)#returns result
        }else{print("Input length < 2!")}#returns this if input is too short
        }else{print("Input not numeric!")}#returns this if input is not numeric
}

##

##function:
#calculate standard error
se<-function(x){sd(x)/sqrt(length(x))}
##
