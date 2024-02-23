##
##
##function: 
#download data from paleobiodb (might take a long time for very large taxa, esp. if full=T)
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
###
###


###
###
##function
#subtract one occurrence dataset from another by excluding any occurrence_no found in both. Useful for analyzing stem-lineage diversity, e.g. pdb.diff(mammaliaformes,subtract=mammalia)->stem_mammaliaforms
pdb.diff<-function(x,subtract){

if(is.data.frame(subtract)){
drop<-which(x$occurrence_no %in% subtract$occurrence_no)
}else{
drop<-which(x$occurrence_no %in% subtract)
}
if(length(drop>0)){
x[-drop,]}else{x}
}
###
###


###
###
##function
#generate a union of two occurrence dataset, making sure that each occurrence_no is only represented once. Useful to complete a dataset if a pdb search term fails to capture all the desired taxa due to categorization or phylogeny, e.g. pdb.union(rbind(pdb("Sauropodomorpha"),pdb("Sauropoda")))->Sauropodomorpha, because currently the pdb seems to only return non-sauropod sauropodomorphs if "Sauropodomorpha" alone is used
pdb.union<-function(x){

for(i in 1:nrow(x)){

which(x$occurrence_no == x$occurrence_no[i])->tmp

if(length(tmp)>1){
x[-tmp[2:length(tmp)],]->x
}

}
return(x)
}
###
###


##
##
##function: 
#select subtaxa of rank="rank" from a csv table downloaded via pdb() with option full=T, where rank parameter must be the valid name of a collumn in the occurrence dataset (e.g. "class"). Returns the indices of the rows containing the data in question, for easy filterig of the dataframes
stax.sel<-function(data,taxa, rank="class"){
tmp<-numeric()

for(i in 1:length(taxa)){
which(eval(parse(text=paste0("data$",rank)))==taxa[i])->ids
c(tmp, ids)->tmp
}
return(tmp)
}
###
###



##
##
##function: 
#make a "species table" from occurrence records (output of pdb), with earliest (eag) and latest (lag) occurrence dates for each unique factor value of tna (taxon name). Output is a data.frame containing the species and their respective stratigraphic range (based on their occurrence records)
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
#generalization of the "species table" from occurrence records for any taxonomic level, provided as an input character vector under parameter "taxa" (e.g. use the genus collumn from the pdb()-output), with the respective earliest and latest time interval for each occurrence given via the parameters "earliest" and "latest".. Output is a data.frame containing the taxa and their respective stratigraphic ranges (based on their occurrence records)
mk.taxtab<-function(taxa, earliest, latest, tax="taxon_name"){
sptab<-levels(factor(taxa))
n<-length(sptab)

xx<-data.frame(tna=taxa, lag=as.numeric(latest), eag=as.numeric(earliest))

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
#This function counts the number of species in a "species table" (output of mk.sptab) at a given point in time x
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
#wrapper around divdistr function; generalized function for applicability to vectors (i.e. several values of x, can be used to graph species diversity through time, e.g. with curve(divdistr_(x,sptab) or with ggplot2)
divdistr_<-function(x, table=sptab,w=rep(1,length(x)),smooth=0){
length(x)->n
rep(NA,n)->tmp
for(i in 1:n){
divdistr(x[i], table=table)*w[i]->tmp[i]
}

if(smooth>0){tmp<-rmeana(y0=tmp,x0=x, plusminus=smooth)}
return(tmp)
}
####
####

##
##
##function:
#counts number of species ocurring in a particular time interval
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
#cleans up the tna$-collumn of ocurrence data and removes common character combinations leading to duplicates
occ.cleanup<-function(x){
if(is.data.frame(x)){
length(levels(factor(x$tna)))->lev
stringr::str_replace_all(x$tna, c("n. gen. "="","cf. "=""," $"="", "^ "="", "n. sp. "="","[[:punct:]]"="", "  "=" "))->out
}else{
length(levels(factor(x)))->lev
stringr::str_replace_all(x, c("n. gen. "="","cf. "=""," $"="", "^ "="", "n. sp. "="","[[:punct:]]"="", "  "=" "))->out
}

print(paste(lev, "factor levels reduced down to", length(levels(factor(out)))))
return(out)

}


##
##
##function:
#Counts number of occurrences overlapping a given numerical age. If ab.val==F, counts number of occurrences, otherwise counts number of specimens/individuals via the "abundance_value" collumn given in pdb data
abdistr<-function(x,table=xx,ab.val=T){

if(length(is.na(table$abund_value))==0){
table$abund_value<-1
}else{
table$abund_value[which(is.na(table$abund_value))]<-1}

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
#wrapper around abdistr function for applicability to vectors (analogue of divdistr_() )
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
#Produce a data.frame to use with ggplot by co-opting the geom_violin() function to plot spindle-diagrams of occurrences. Contains a number of repetitions of the taxon name for each age proportional to the number of occurrences or individuals (i.e. abundance proxy) at any given time
ab.gg<-function(data=occ, taxa="taxon_A", agerange=c(252,66), precision_ma=1, ab.val=T){
ma<-numeric()
tax<-character()#just empty vectors to append our values to
agerange<-seq(min(agerange),max(agerange),precision_ma)#this just makes a sequence from the given age interval

###for individual species
if(is.data.frame(data) & taxa =="SPP"){
levels(factor(data$tna))->lev
for(i in 1:length(lev)){

st<-data[data$tna==lev[i],]#get the table for each species and save it as st.


###
abdistr_(agerange, table=st, ab.val=ab.val)->abundance#this computes the abundance for the agerange

ma<-c(ma,c(rep(agerange, abundance)))#this repeats each value in the age range as often as there are occurrences or specimens from that time

tax<-c(tax,rep(lev[i], sum(abundance)))#this simply repeats the name of the species as often as there are specimens
}
}else{
###
for(i in 1:length(taxa)){
if(is.data.frame(data)){st<-data}else if(is.list(data) & taxa !="SPP"){
st<-eval(parse(text=paste0("occ$",taxa[i])))#get the value of each taxon and save it as st.
}else{stop("data must be a data.frame() or a list()-object containing data.frames")}

###


abdistr_(agerange, table=st, ab.val=ab.val)->abundance#this computes the abundance for the agerange

ma<-c(ma,c(rep(agerange, abundance)))#this repeats each value in the age range as often as there are occurrences or specimens from that time

tax<-c(tax,rep(taxa[i], sum(abundance)))#this simply repeats the name of the taxon as often as there are specimens
}}



data.frame(ma=ma, tax=tax)->dd#this puts both collumns we just made together into one data.frame
return(dd)#this returns the data.frame
}
####
####


##
##
##function: 
#Produce a data-frame to use with ggplot by co-opting the geom_violin() function to plot spindle-diagrams of species diversity. Contains a number of repetitions of the taxon name for each age proportional to the number of species (i.e. diversity proxy) at any given time.
div.gg<-function(data=occ, taxa="", agerange=c(252,66), precision_ma=1){#occ needs to be a list()-object with mk.sptab-output for relevant subtaxa saved as occ$sptab_taxonname)
occ<-data
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

#example workflow on how to plot simple paleobiodiversity graphs:
if(1==2){#just to prevent this being automatically executed ;)
occ<-list()
pdb("Eurypterida")->occ$Eurypterida
mk.sptab(occ$Eurypterida)->occ$sptab_Eurypterida
curve(divdistr_(x,occ$sptab_Eurypterida), xlim=c(500,250)) #produces a simple graph of raw species diversity – do note that there tend to be many repeat entries due to "alternate" spellings in pdb data, so you might need to manually clean up your sptab if you want more reliable absolute figures (relative figures and trends should be less affected)
palaeoverse::axis_geo()
div.gg(data=occ, taxa=c("Eurypterida"), agerange=c(500,250))->occ$divgg
ggplot2::ggplot(data=occ$divgg)+ggplot2::xlim(500,250)+ggplot2::geom_violin(ggplot2::aes(x=ma, y=tax, fill=tax), col="white", scale="count", adjust=0.5)+deeptime::coord_geo()#produces a spindle diagram of diversity
}


##function
#Automatically download pdb occurrence data and return a list object including species range tables, for use with divdistr_() and functions based on it for graphing diversity.
pdb.autodiv<-function(taxa,cleanup=TRUE,interval=NULL){
occ<-list()
if(class(taxa)=="phylo"){
tree0$tip.label->treetips}else{
taxa->treetips}

#download and cleanup
for(i in 1:length(treetips)){

if(is.null(interval)){
pdb(treetips[i])->occ[[i]]
}else{
pdb(treetips[i],interval=interval)->occ[[i]]
}


if(cleanup==TRUE){
occ.cleanup(occ[[i]])->occ[[i]]$tna}
}
names(occ)<-treetips

#build species tables
for(i in 1:length(treetips)){
mk.sptab(eval(parse(text=paste0("occ$",treetips[i]))))->occ[[length(treetips)+i]]
}
names(occ)<-c(treetips,paste0("sptab_", treetips))

return(occ)
}
##



##function:
#converts a species table into a form suitable for plotting on a calibrated phylogeny (e.g. in ape) using the viol() function. Requires either a calibrated tree (will be needed for plotting anyway) or its root.time.
convert.sptab<-function(sptab,tree=NULL,root.time=NULL){
if(is.null(root.time)){
tree$root.time->root.time
}

which(sptab$max>root.time)->drop
if(length(drop)>0){
sptab[-drop]->sptab}
sptab->sptab_

sptab_$max<-abs(sptab$min-root.time)
sptab_$min<-abs(sptab$max-root.time)


return(sptab_)
}
##

##also related
#source("viol.R") #allows plotting of customizeable violin plots in base graphics
#source("phylospindles.R") #function for plotting phylogenies with added diversity spindles
