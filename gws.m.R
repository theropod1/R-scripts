##allometric equations for lamnid sharks

gws.m<-function(tl_m=14,method="mean"){
a<-tl_m
data.frame(numeric(length(a)),
3.29*10^-6*(100*a)^3.174,
7.5763*10^-6*(0.9442*(100*a)-5.7441)^3.0848,
4.80376*10^-6*(100*a)^3.09497,
7.914*a^3.096,
3.8*10^-6*(100*a)^3.15,
10^0.99*a^3.00)->masses
colnames(masses)<-c("TL", "Gottfried_et_al_1996","Kohler_et_al_1995", "Casey_Pratt_1985","Mollet_Cailliet_1996", "Tricas_McCosker_1985", "McClain_et_al_2015")
masses$TL<-tl_m

if(method=="mean"){out<-apply(masses[,2:7],1,mean)}
if(method=="max"){out<-apply(masses[,2:7],1,max)}
if(method=="min"){out<-apply(masses[,2:7],1,min)}
if(method=="sd"){out<-apply(masses[,2:7],1,sd)}

if(method=="all"){masses$mean<-apply(masses[,2:7],1,mean)
masses$SD<-apply(masses[,2:7],1,sd)
out<-masses}

if(method=="g1996"){out<-masses[,2]}#for gottfried et al. 1996 n=174
if(method=="k1995"){out<-masses[,3]}#for kohler et al. 1995 nforkl= 112, nmass=125
if(method=="c1985"){out<-masses[,4]}#for casey and pratt 1985 n=200
if(method=="m1996"){out<-masses[,5]}#for mollet and cailliet 1996 n=327
if(method=="t1984"){out<-masses[,6]}#for Tricas and McCosker 1984 n=127
if(method=="m2015"){out<-masses[,7]}#for Mcclain et al. 2015 n=90

return(out)
}
##
#example usage:
gws.m(5)#gives the mean mass estimate for a TL of 5 m
gws.m(20, "c1985")#gives the mass estimate for a TL of 20 m based on only the regression of Casey and Pratt 1985
gws.m(20, "all")#gives the mass estimate for a TL of 20 m based on each regression equation



gws.pcl<-function(tl){0.7845*tl^1.0382}#precaudal length function (power curve from Mollet & Cailliet 1996, both measurements in metres)


gws.fl<-function(tl){0.9442*(100*tl)-5.74411}#fork length function (power curve from Mollet & Cailliet 1996, both measurements in metres)


mako.m<-function(tl_m=14){
fl<- 0.9286*tl_m*100-1.7101#I. oxyrinchus, Kohler et al. 1995, n forklength= 199
out<- 5.2432e-6*fl^3.1407#nmass=2081

return(out)
}


lamna.m<-function(tl_m=14){
fl<-0.8971*tl_m*100+1.7939#Lamna nasus, Kohler et al. 1995, nforkl=13

out<- 1.4823e-5*fl^2.9641#nmass=15

return(out)
}


##references:
#Casey, J.G. and Pratt, H.L. 1985. Distribution of the White Shark,  Carcharodon carcharias, in the Western North Atlantic. Memoirs of the Southern California Academy of Sciences 9: 2–14.
#Gottfried, M.D., Compagno, L.J. and Bowman. 1996. Size and skeletal anatomy of the giant" megatooth" shark  Carcharodon megalodon. In: Klimley, A.P. and Ainley, D.G. (eds.), Great White Sharks: The Biology of Carcharodon Carcharias, 55–66. Academic Press, San Diego.
#Kohler, N.E., Casey, J.G. and Turner, P.A. 1996. Length-length and length-weight relationships for 13 shark species from the western North Atlantic. Fishery Bulletin 93: 412–418.
#McClain, C.R., Balk, M.A., Benfield, M.C., Branch, T.A., Chen, C., Cosgrove, J., Dove, A.D., Gaskins, L.C., Helm, R.R. and Hochberg, F.G. 2015. Sizing ocean giants: patterns of intraspecific size variation in marine megafauna. PeerJ 3: e715.
#Mollet, H.F. and Cailliet, G.M. 1996. Using Allometry to Predict Body Mass from Linear Measurements of the White Shark. Great White Sharks: the biology of Carcharodon carcharias: 81–89.
#Tricas, T.C. and McCosker, J.E. 1984. Predatory Behaviour of the White Shark ( Carcharodon carcharias) with notes on its biology. Proceedings of the California Academy of Sciences 43 (14): 221–234.



##NOTES
if(1==2){
#approximate mean formula by solving equation system:
log(gws.m(5)/gws.m(20)) / log(5/20)->b
gws.m(5)/5^b->a
#test
curve(gws.m(x), xlim=c(0,20))
curve(a*x^b, col="red", add=T)
#mean formula is 8.069889*x^3.105513

#estimate mean formula by linear regression across 1 m intervals in range 1-25 m
gws.m(seq(1,25,1))->mm
lm(log(mm)~log(as.numeric(names(mm))))->llmm
names(llmm$coefficients)<-c("(Intercept)","TL")

#mean formula is 8.020427*x^3.107862
}
