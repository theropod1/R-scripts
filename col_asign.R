##function:
#assign colours using a desired function (e.g. viridis(), ggcol()) based on factor levels in a variable x
col_asign<-function(x,FUN=ggcol){

levels(factor(x))->lev

character()->out

for(i in 1:length(x)){
out<-c(out,FUN(length(lev))[which(lev==x[i])])}

return(out)

}
##
