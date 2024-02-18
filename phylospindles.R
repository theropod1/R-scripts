##function
#automatically plot a calibrated phylogeny with "diversity spindles" based on several sptab_<Taxon_name>, contained in a list()-object (parameter occ). Scale needs to be adjusted manually using dscale parameter.

phylo.spindles<-function(tree0, occ, ages=NULL, xlimits=c(300,0), col=add.alpha("black"), fill=col,lwd=1,dscale=0.002,res=1, cex.txt=1,col.txt=add.alpha(col,1), axis=T, labels=T, txt.y=-0.4,txt.x=150){

ape::plot.phylo(tree0,x.lim=-1*(xlimits-tree0$root.time),align.tip.label=2, label.offset=50,show.tip.label=F, y.lim=c(0.8,length(tree0$tip.label)+0.2))->tmp1

col->col_
fill->fill_
col.txt->col.txt_

for(i in 1:length(tree0$tip.label)){

#crop spindles to age data, if given
if(!is.null(ages)){
cutoff<-abs(as.numeric(ages[rownames(ages)==tree0$tip.label[i],])-tree0$root.time)
}else{
cutoff<-range(convert.sptab(eval(parse(text=paste0("occ$sptab_",tree0$tip.label[i]))), tree0)[,2:3])
}

#vary colours, if desired
if(length(col_)==length(tree0$tip.label)){
col<-col_[i]
}
if(length(fill_)==length(tree0$tip.label)){
fill<-fill_[i]
}
if(length(col.txt_)==length(tree0$tip.label)){
col.txt<-col.txt_[i]
}

#plot spindles
viol(seq(tmp1$x.lim[1],tmp1$x.lim[2],res),pos=i, stat=divdistr_, table=convert.sptab(eval(parse(text=paste0("occ$sptab_",tree0$tip.label[i]))), tree0), dscale=dscale, col=col, fill=fill, lwd=1,cutoff=cutoff)

if(labels==T){#add labels
text(x=1-(txt.x-tree0$root.time),y=i,adj=c(0,txt.y), tree0$tip.label[i], cex=cex.txt,col=col.txt)}

}#end loop


if(axis==T){#add axis
ticks<-seq(round(min(c(max(xlimits),tree0$root.time))/10)*10,round(min(xlimits)/10)*10,-25)

axis(1,at=1-(ticks-tree0$root.time), lab=ticks)}

}
##


##function
#Convert between geological timescales and the scale to which a tree is plotted by ape::plot.phylo() (the latter is inverted and starts at 0 at the base of the tree.
tsconv<-function(x,tree0){
-1*(x-tree0$root.time)
}
