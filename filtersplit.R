#function that splits data frame named x into list containing data frames for entries matching each unique factor level of varname

filtersplit<-function(x,varname){
list()->t

for(i in levels(factor(varname))){
x[varname==i,]->tmp
t[[i]]<-tmp
}
names(t)<-levels(factor(varname))

return(t)
}
