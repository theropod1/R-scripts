##function
#automatically plot a calibrated phylogeny with "diversity spindles" based on several sptab_<Taxon_name>, contained in a list()-object (parameter occ). Scale needs to be adjusted manually using dscale parameter.

phylo.spindles<-function(phylo0, occ, ages=NULL, xlimits=c(300,0), col=add.alpha("black"), fill=col,lwd=1,dscale=0.002,res=1, cex.txt=1,col.txt=add.alpha(col,1), axis=T, labels=T, txt.y=-0.4,txt.x=150, add=FALSE,tbmar=0.2,weights=1){

if(add==FALSE){
ape::plot.phylo(phylo0,x.lim=-1*(xlimits-phylo0$root.time),align.tip.label=2, label.offset=50,show.tip.label=F, y.lim=c(1-tbmar,length(phylo0$tip.label)+tbmar))->tmp1
}else(
tmp1<-get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
)

#colors
col->col_
fill->fill_
col.txt->col.txt_

#weights
if(length(weights)==1){
weights<-rep(weights,length(seq(tmp1$x.lim[1],tmp1$x.lim[2],res)))
}
#print(weights) #for troubleshooting

##limits for spindles
for(i in 1:length(phylo0$tip.label)){

if(!is.null(ages)){#if age data is provided

    if(phylo0$tip.label[i] %in% rownames(ages)){ #check if tiplabel can be found in rownames
    cutoff<-abs(as.numeric(ages[rownames(ages)==phylo0$tip.label[i],])-phylo0$root.time)
    }else{
    warning(paste0(phylo0$tip.label[i], " not found in rownames(ages). Please make sure that rownames in age data match taxa in phylogeny!"))
        if(nrow(ages)==length(phylo0$tip.label)){# … contingency if rownames cannot be matched, but row numbers can:
        cutoff<-abs(as.numeric(ages[i,])-phylo0$root.time)
        }else{
        cutoff<-range(convert.sptab(eval(parse(text=paste0("occ$sptab_",phylo0$tip.label[i]))), phylo0)[,2:3])}
        }
}else{#use range of data if no validly formatted ages are provided
cutoff<-range(convert.sptab(eval(parse(text=paste0("occ$sptab_",phylo0$tip.label[i]))), phylo0)[,2:3])
}


##vary colours, if desired
if(length(col_)==length(phylo0$tip.label)){
col<-col_[i]
}
if(length(fill_)==length(phylo0$tip.label)){
fill<-fill_[i]
}
if(length(col.txt_)==length(phylo0$tip.label)){
col.txt<-col.txt_[i]
}

#apply weights
if(is.matrix(weights)){
w<-weights[,i]
}else if(length(weights)==length(seq(tmp1$x.lim[1],tmp1$x.lim[2],res))){
w<-weights
}

if(length(w)!=length(seq(tmp1$x.lim[1],tmp1$x.lim[2],res))){stop("weights vector must have the same length as time interval/resolution")}

#plot spindles
viol(seq(tmp1$x.lim[1],tmp1$x.lim[2],res),pos=i, stat=divdistr_, table=convert.sptab(eval(parse(text=paste0("occ$sptab_",phylo0$tip.label[i]))), phylo0), dscale=dscale, col=col, fill=fill, lwd=1,cutoff=cutoff,w=w)

if(labels==T){#add labels

if(length(txt.x)==length(phylo0$tip.label)){#if a vector of x values for labels is provided
    if(length(which(names(txt.x)==phylo0$tip.label[i]))==1){
    which(names(txt.x)==phylo0$tip.label[i])->j
    text(x=1-(txt.x[j]-phylo0$root.time),y=i,adj=c(0,txt.y), phylo0$tip.label[i], cex=cex.txt,col=col.txt)
    }else{
    text(x=1-(txt.x[i]-phylo0$root.time),y=i,adj=c(0,txt.y), phylo0$tip.label[i], cex=cex.txt,col=col.txt)}
}else{
text(x=1-(txt.x-phylo0$root.time),y=i,adj=c(0,txt.y), phylo0$tip.label[i], cex=cex.txt,col=col.txt)}
}

}#end loop


if(axis==T){#add time axis
ticks<-seq(round(min(c(max(xlimits),phylo0$root.time))/10)*10,round(min(xlimits)/10)*10,-25)

axis(1,at=1-(ticks-phylo0$root.time), lab=ticks)}

}
##

##example workflow for phylo.spindles()
if(1==2){#just to prevent execution
#load all needed functions
source("div_functions.R")
source("phylospindles.R")
source("addalpha.R")
source("viol.R")
source("tsperiods.R")

library(ape)
library(strap)

ape::read.tree(text="((Stegosauria,Ankylosauria),(Ornithopoda,(Ceratopsia,Pachycephalosauria)));")->phylo0#load a phylogeny of ornithischian dinosaurs using ape’s read.tree function
#manually create a matrix that will be used for time-calibrating the tree:
ages<-cbind(FAD=c(170,170,165,160,130),LAD=c(120,66,66,66,66))
rownames(ages)<-phylo0$tip.label
#not time-calibrate using strap
strap::DatePhylo(phylo0, ages, rlen = 2, method = "equal", add.terminal = FALSE)->phylo0

#download pdb occurrence records and automatically convert to species range tables:
occ<-pdb.autodiv(phylo0) #this takes either a character vector or an object of class "phylo" as input. Additional taxa can be manually downloaded and appended to this list(), or a preexisting object used as long as it has names matching the naming scheme needed for the phylo.spindles() function (that is, species tables named "sptab_taxonname1" etc.)

##now plot the tree, including diversity spindles
phylo.spindles(phylo0,occ=occ,col=add.alpha("black"),ages=ages,txt.y=.5, dscale=0.005,xlim=c(260,0))
ts.periods(phylo0, names=F, ylim=c(0,.7),alpha=0.6)#optionally add a colour-coded period-level timescale using the function from tscol.R

#Spindles can also be added manually,e.g. overlying other spindles, using the viol()-function, if desired:
viol(x=c(0:260),stat=divdistr_, pos=3, table=convert.sptab(occ$sptab_Ornithopoda,phylo0),dscale=0.005, cutoff=tsconv(c(165,66),phylo0),fill=add.alpha("red"),col=add.alpha("red"))
#note the use of tsconv() here to convert between geological ages and the x axis scale to which the calibrated phylogeny is plotted by ape, which starts at 0 at the base of the phylogeny
}


##function
#Convert between geological timescales and the scale to which a tree is plotted by ape::plot.phylo() (the latter is inverted and starts at 0 at the base of the tree.
tsconv<-function(x,phylo0){
-1*(x-phylo0$root.time)
}
