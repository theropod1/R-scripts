##function:
#A simple function for calculating rowmeans or other parameters. Input is a data.frame, matrix or any number of vectors given under ..., fun (defaults to mean, but other functions, such as sd, are also possible) is then applied to each row in the resulting dataframe. If values for collumns are required, simply use cbind(row1, row2,etc.) or t(data.frame(col1, col2, etc) instead

rowmeans<-function(..., fun=mean){
vectors<-cbind(...)

out<-numeric()
for(i in 1:nrow(vectors)){

as.numeric(vectors[i,])->inp

out<-c(out, fun(inp, na.rm=T))

}
return(out)
}
##
