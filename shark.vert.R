#vertebra scaling following Gottfried et al. 1996, wintner and cliff 1999. Estimates TL (m) from max. vertebral width (mm), or the reverse
shark.vert<-function(x,reverse=FALSE, method=ifelse(length(x)>1,"carcharodon1","all")){
ms<-c("carcharodon1","carcharodon2","carcharodon3","isurus1","lamna1")
if(is.numeric(method)) method<-ms[method]
else if(method=="all") method<-ms

if(reverse=="r") TRUE->reverse

out<-numeric() #empty object to append to

if("wintnercliff" %in% method | "carcharodon1" %in% method){ ##use wintner and cliff regression
if(reverse) c(out,wintner_cliff_1999 = (0.0188*(0.8550*(x*100)-0.0955)-0.284)*10)->out
else c(out,wintner_cliff_1999 = ((x/10+0.284)/0.0188+0.0955)/0.8550/100)->out
#reference: Fig. 3 in Wintner, S.P. and Cliff, G. 1999. Age and growth determination of the white shark, Carcharodon carcharias, from the east coast of South Africa. Fish. Bull., 97:153---169.
#equation for vertebral diameter~precaudal lenght
#with pcl~TL regression from: Mollet, H.F. and Cailliet, G.M. 1996. Using Allometry to Predict Body Mass from Linear Measurements of the White Shark, p. 81---89. In Klimley, P.A. and Ainley, D.G. (eds.), Great White Sharks: the biology of Carcharodon carcharias.
}

if("gottfriedetal" %in% method | "carcharodon2" %in% method){ ##use gottfried et al. regression for carcharodon
if(reverse) c(out,Gottfried_et_al_1996 = (x-0.22)/0.058)->out
else c(out,Gottfried_et_al_1996 = 0.22+0.058*x)->out
#reference: p. 60 in Gottfried, M.D., Compagno, L.J., and Bowman. 1996. Size and skeletal anatomy of the giant" megatooth" shark  Carcharodon megalodon, p. 55---66. In Klimley, A.P. and Ainley, D.G. (eds.), Great White Sharks: the biology of Carcharodon carcharias. Academic Press, San Diego. 
}

if("natansonskomal2015" %in% method | "natansonetal2015" %in% method | "carcharodon3" %in% method){ ##use natanson et al. regression for Carcharodon
if(reverse)  c(out,Natanson_Skomal_2015 = ((0.9442*x*100-5.7441)-35.6)/10.8*2)->out
else c(out,Natanson_Skomal_2015 = ((10.8*x/2+35.6)+5.7441)/0.9442/100)->out
#reference: Figure 1 in Natanson, L.J. and Skomal, G.B. 2015. Age and growth of the white shark, Carcharodon carcharias, in the western North Atlantic Ocean. Marine and Freshwater Research: 66:387. https://doi.org/10.1071/MF14127.
#TL from Fork length:  FL=0.9442*TL-5.7441 -> (FL+5.7441)/0.9442=TL
#Kohler, N.E., Casey, J.G., and Turner, P.A. 1996. Length-length and length-weight relationships for 13 shark species from the western North Atlantic. Fishery Bulletin: 93:412---418.
}

if("cernalicandeo" %in% method | "isurus1" %in% method){ ##use natanson et al. regression for Isurus
if(reverse) c(out,Cerna_Licandeo_2009 = (x*100/25.192)^(1/0.823)*2)->out
else c(out,Cerna_Licandeo_2009 = 25.192*(x/2)^0.823/100)->out
#reference: Figure 2 in Cerna, F. and Licandeo, R. 2009. Age and growth of the shortfin mako (Isurus oxyrinchus) in the south-eastern Pacific off Chile. Marine and Freshwater Research: 60:394. https://doi.org/10.1071/MF08125.
}

if("natansonetal2002" %in% method | "lamna1" %in% method){ ##use natanson et al. regression for Lamna
if(reverse) c(out,Natanson_etal_2002 = ((0.885*x*100 + 0.99)/exp(2.96))^(1/0.88)*2)->out
else c(out,Natanson_etal_2002 = ((x/2)^0.88*exp(2.96)-0.99)/0.885/100)->out
#reference: Natanson, L.J., Mello, J.J., and Campana, S.E. 2002. Validated age and growth of the porbeagle shark (Lamna nasus) in the western North Atlantic Ocean. FISHERY BULLETIN-NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION: 100:266---278.
}

return(out)
}
