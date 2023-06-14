
##download data from paleobiodb including full record
pdb<-function(taxon="", interval="all", full=FALSE){

if(full==TRUE){

    if(interval=="all"){
    pbdb_url <-paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",taxon,"&show=full")
    }else{
    pbdb_url <-paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",taxon,"&interval=",interval,"&show=full")}
}else{
    if(interval=="all"){
    pbdb_url <-paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",taxon)
    }else{
    pbdb_url <-paste0("https://paleobiodb.org/data1.2/occs/list.csv?base_name=",taxon,"&interval=",interval)}
    }

occ<-read.csv(pbdb_url)
occ$hltax<-taxon
occ$identified_name->occ$tna#for colname compatibility with paleobiodb package
occ$min_ma->occ$lag#for colname compatibility with paleobiodb package
occ$max_ma->occ$eag#for colname compatibility with paleobiodb package
return(occ)
}
#
#
#


##select subtaxa of rank rank from a csv table downloaded via pdb with option full=T
stax.sel<-function(data,taxa, rank="class"){
tmp<-numeric()

for(i in 1:length(taxa)){
which(eval(parse(text=paste0("data$",rank)))==taxa[i])->ids
c(tmp, ids)->tmp
}
return(tmp)
}
#
#
#



##
##
##function: 
#make species table with earliest (eag) and latest (lag) occurrence dates for each unique factor value of tna
mk.sptab<-function(xx,tax="taxon_name"){
sptab<-levels(factor(xx$tna))
n<-length(sptab)

xx$lag<-as.numeric(xx$lag)
xx$eag<-as.numeric(xx$eag)

sptab<-data.frame(tna=sptab, max=rep(NA,n), min=rep(NA,n))#make table with age ranges for all species

for(i in 1:n){
sptab$max[i]<-max(xx$eag[xx$tna==sptab$tna[i]])
sptab$min[i]<-min(xx$lag[xx$tna==sptab$tna[i]])
}
(sptab$min+sptab$max)/2->sptab$ma
sptab$tax<-rep(tax,n)
return(sptab)
}
####
#### 


##
##
##function:
#for counting number of species at a given numerical age
divdistr<-function(x,table=sptab){
which(table$min<=x)->a
which(table$max>=x)->b
intersect(a,b)->id
length(id)->length
return(length)
}
####
####


##
##
##function
#wrapper around divdistr function for applicability to vectors
divdistr_<-function(x, table=sptab){
length(x)->n
rep(NA,n)->tmp
for(i in 1:n){
divdistr(x[i], table=table)->tmp[i]
}
return(tmp)
}
####
####

##
##
##function:
#count number of species in particular interval
divdistr_int<-function(x,table=sptab, ids=F){#here x needs to be a vector of length 2 containing the minimum and maximum ages defining the interval
which(table$min<=max(x))->a
which(table$min>=min(x))->b

which(table$max<=max(x))->c
which(table$max>=min(x))->d

intersect(a,b)->id1#this intersection contains those entries that have a minimum smaller than the maximum but larger than the minimum
intersect(c,d)->id2#this intersection contains those entries that have a maximum smaller than the maximum but larger than the minimum

union(id1,id2)->id#this is the union of both intersections, giving all taxa that have any temporal overlap with the selected interval
length(id)->length
if(ids==T){return(id)} else{
return(length)}#standard setting ids=F makes it return the number of records
}
####
####



##
##
##function:
#for counting number of occurrences at a given numerical age. if ab.val==F, counts number of occurrences, otherwise counts number of specimens/individuals via the "abundance_value" collumn
abdistr<-function(x,table=xx,ab.val=T){
which(as.numeric(table$lag)<=x)->a
which(as.numeric(table$eag)>=x)->b
intersect(a,b)->id
length(id)->n
sum(table$abund_value[id], na.rm=T)->abundance
if(ab.val==F){
return(n)} else{
return(abundance)}
}
####
####


##
##
##function
#wrapper around abdistr function for applicability to vectors
abdistr_<-function(x, table=xx, ab.val=T){
length(x)->n
rep(NA,n)->tmp
for(i in 1:n){
abdistr(x[i], table=table, ab.val=ab.val)->tmp[i]
}
return(tmp)
}
####
####


##
##
##function: 
#Function that will produce a data-frame to use with ggplot for co-opting the geom_violin() function to produce a spindle-diagram of occurrences. Contains a number of repetitions of the taxon name for each age proportional to the number of occurrences or individuals (i.e. abundance proxy) at any given time
ab.gg<-function(data=occ, taxon="taxon_A", agerange=c(252,66), precision_ma=1, ab.val=T){
ma<-numeric()
tax<-character()#just empty vectors to append our values to
agerange<-seq(min(agerange),max(agerange),-precision_ma)#this just makes a sequence from the given age interval

abdistr_(agerange, table=data, ab.val=ab.val)->abundance#this computes the abundance for the agerange

ma<-c(ma,c(rep(agerange, abundance)))#this repeats each value in the age range as often as there are occurrences or specimens from that time

tax<-c(tax,rep(taxon, sum(abundance)))#this simply repeats the name of the taxon as often as there are specimens
data.frame(ma=ma, tax=tax)->dd#this puts both collumns we just made together into one data.frame
return(dd)#this returns the data.frame
}
####
####


##
##
##function: 
#Function that will produce a data-frame to use with ggplot for co-opting the geom_violin() function to produce a spindle-diagram of species diversity. Contains a number of repetitions of the taxon name for each age proportional to the number of occurrences or individuals (i.e. abundance proxy) at any given time
div.gg<-function(data=occ, taxa="", agerange=c(252,66), precision_ma=1){#occ needs to be a list()-object with mk.sptab-output for relevant subtaxa saved as occ$taxonname_sptab)
ma<-numeric()
tax<-character()#empty vectors to append to
agerange<-seq(max(agerange),min(agerange),-precision_ma)#make a sequence out of agerange based on precision

for(i in 1:length(taxa)){
st<-eval(parse(text=paste0("occ$sptab_",taxa[i])))#get the value of occ$sptab_taxon and save it as st.
taxonfun<-divdistr_(agerange, table=st)#make diversity distribution for taxon

ma<-c(ma,c(rep(agerange, taxonfun)))#repeat each value for agerange by the estimated diversity at that time

tax<-c(tax,rep(taxa[i], sum(taxonfun)))#repeat taxon names according to the sum of their diversity values
}#repeat for each taxon in taxa/sptab_taxon in occ
data.frame(ma=ma, tax=tax)->dd
return(dd)
}
####
####
