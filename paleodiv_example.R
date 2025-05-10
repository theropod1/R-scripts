library(paleoDiv)
library(strap)
library(paleotree)
library(rphylopic)
source("https://raw.githubusercontent.com/theropod1/R-scripts/refs/heads/main/ppsource.R") #for ppsource function that automatically summarizes the sources for phylopic silhouettes
#####

#download
read.tree(text="(Enantiornithes,(Hesperornithiformes,(Palaeognathae,(Galloanserae,Odontopterygiformes,Neoaves(Columbiformes,Passerea((Cuculiformes,Strisores),((Gruiformes,Charadriiformes),(Aequornithes,Telluraves(((Accipitriformes,Strigiformes),(Coraciiformes,Piciformes)),(Cariamiformes,(Falconiformes,(Psittaciformes,Passeriformes))))))))))));")->tree0
if(!exists("treedata0")) pdb.autodiv(tree0)->treedata0 #only start completely new download if dataset is not already downloaded; this saves time if script is executed repeatedly

if(exists("treedata0")){ 
treedata1<-list()

tree0$tip.label[!(tree0$tip.label%in%names(treedata0))]->addtax
if(length(addtax)>0) treedata1<-pdb.autodiv(addtax)
}
if(!all(names(treedata1)%in%names(treedata0))) c(treedata0,treedata1)->treedata0

#age corrections
ages<-tree.ages(tree0,treedata0)
ages["Aequornithes",]<-c(66,0)
ages["Gruiformes",]<-c(66,0)
ages["Cariamiformes",]<-c(56,0)
ages["Strisores",]<-c(48,0)
ages["Enantiornithes",2]<-c(66)

#time calibration
strap::DatePhylo(tree0, ages, rlen = 4, method = "equal", add.terminal = FALSE)->tree0 #time calibration
paleotree::minBranchLength(tree0,mbl=2)->tree0 # making short branches longer for visibility (optional)

##plot headers
#pdf("phylo_a4.pdf",width=210/25.4, height=297/25.4) #for a4 page size

##plotting
phylo.spindles(tree0,occ=treedata0,fill=add.alpha(rainbow(length(tree0$tip.label)),0.5),col=darken(rainbow(length(tree0$tip.label)),-0.2),ages=ages,txt.y=0.5, dscale=0.010,xlim=c(200,-50),axis=F,txt.x=ages[,2]-1,tbmar=c(0.6,0.2),labels=T)

#add timescale
ts.stages(tree0, names=F, ylim=c(-.3,.5),alpha=0.6)
ts.periods(tree0, names=T,alpha=0)

axis(1,at=tsconv(seq(400,-0,-50),tree0), lab=seq(400,-0,-50), cex=0.75,col="grey30",col.lab="grey30")

#add vertical lines for mass extinctions
abline(v=tsconv(c(315,260,252,201.3,143,89,66,23),tree0),lwd=c(1,2,2,2,1,1,2,1), col=add.alpha("grey"))
#label clades
nodelabels(c(1:7),c(34,31,30,25,24,23,21),col="black", bg="white",frame="circle",cex=0.8)
legend("topleft",pch=cc(1:7), legend=c("Australaves","Afroaves","Telluraves","Passerea","Neoaves","Neognathae","Neornithes"), bg=add.alpha("white",0.8), box.col=NA)


##add images from phylopic:
if(!exists("img")) list()->img

for(i in 1:length(tree0$tip.label)){

if(!exists("img")){img[[tree0$tip.label[i]]] <- get_phylopic(uuid = get_uuid(name = tree0$tip.label[i],n=1)[1])
}else if(!(tree0$tip.label[i]%in%names(img))){
img[[tree0$tip.label[i]]] <- get_phylopic(uuid = get_uuid(name = tree0$tip.label[i],n=1)[1])
}

add_phylopic_base(img = img[[tree0$tip.label[i]]], x = tsconv(60,tree0), y = i, ysize = 1, fill=add.alpha(ggcol(length(tree0$tip.label))[i],00.50))
}

