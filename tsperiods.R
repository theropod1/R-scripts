##function
#draw a horizontal geological timescale with periods/systems
ts.periods <- function(phylo=NULL,alpha=1,names=T,exclude=c("Quarternary"),col.txt=NULL,border=NA,ylim=0.5,adj.txt=c(0.5,0.5),txt.y=mean,bw=F,update=NULL){
  ## Data for geological periods
  
  if(!is.null(update)){
  read.csv(update)->ts #use this to manually update time scale
  periods<-data.frame(interval=ts$periods,start=ts$bottom, end=ts$top,col=ts$col)
  
  }else{
  periods <- data.frame(
    period = c("Quarternary", "Neogene", "Paleogene", "Cretaceous", "Jurassic", "Triassic", "Permian", "Carboniferous", "Devonian", "Silurian", "Ordovician", "Cambrian","Ediacaran"),
    start = c(0, 2.58, 23.03, 66, 145, 201.3, 252.17, 298.9, 358.9, 419.2, 443.8, 485.4,541),
    end = c(2.58, 23.03, 66, 145, 201.3, 252.17, 298.9, 358.9, 419.2, 443.8, 485.4, 541,635), col = paste0("#",c("F9F97F","FFE619","FD9A52","7FC64E","34B2C9","812B92","F04028","67A599","CB8C37","B3E1B6","009270","7FA056","FED96A")))
  }

  if(!is.null(phylo)){
periods$start<-tsconv(periods$start,phylo)
periods$end<-tsconv(periods$end,phylo)
  }

  
  if(is.null(col.txt)){col.txt<-periods$col}#set text colour, is unset

if(length(ylim)==1 & !is.null(phylo)){
lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
c(min(lastPP$y.lim),ylim)->ylim
}else if(length(ylim)==1){ylim<-c(0,ylim)}
  
  for (i in 1:nrow(periods)) {
    if(bw!=T){polygon(
      x=c(periods$start[i], periods$end[i], periods$end[i], periods$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = add.alpha(periods$col[i],alpha=alpha),
      border = border
    )}else if(bw==T){
    polygon(
      x=c(periods$start[i], periods$end[i], periods$end[i], periods$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = "white",
      border = border
    )
    }
    
    if(names==TRUE){
    if(periods$period[i] %in% exclude == FALSE){
    
    if(length(col.txt)>1){#if text colours are given as vector
    text(x=mean(c(periods$start[i],periods$end[i])),y=txt.y(ylim), adj=adj.txt,periods$period[i],col=col.txt[i])}else{#if single text colour is given
    text(x=mean(c(periods$start[i],periods$end[i])),y=txt.y(ylim), adj=adj.txt,periods$period[i],col=col.txt)
    }
    }
    
    }
  }

}

##


##function
#function for drawing stages
ts.stages <- function(phylo=NULL,alpha=1,names=F,col.txt=NULL,border=NA,ylim=0.5,adj.txt=c(0.5,0.5),txt.y=mean,bw=F,update=NULL){
  ##Data for geological periods
  if(!is.null(update)){
  read.csv(update)->ts #use this to manually update time scale
  intervals<-data.frame(interval=ts$stage,start=ts$bottom, end=ts$top,col=ts$col)
  }else{
    intervals<-data.frame(interval=c('Avalon Assemblage', 'White Sea Assemblage', 'Nama Assemblage', 'Fortunian', 'Stage 2', 'Stage 3', 'Stage 4', 'Wulian', 'Drumian', 'Guzhangian', 'Paibian', 'Jiangshanian', 'Stages 10', 'Tremadocian', 'Floian', 'Dapingian', 'Darriwilian', 'Sandbian', 'Katian', 'Hirnantian', 'Rhuddanian', 'Aeronian', 'Telychian', 'Sheinwoodian', 'Homerian', 'Gorstian', 'Ludfordian', 'Pridoli', 'Lochkovian', 'Pragian', 'Emsian', 'Eifelian', 'Givetian', 'Frasnian', 'Famennian', 'Tournaisian', 'Visean', 'Serpukhovian', 'Bashkirian', 'Moscovian', 'Kasimovian', 'Gzhelian', 'Asselian', 'Sakmarian', 'Artinskian', 'Kungurian', 'Roadian', 'Wordian', 'Capitanian', 'Wuchiapingian', 'Changhsingian', 'Induan', 'Olenekian', 'Anisian', 'Ladinian', 'Carnian', 'Norian', 'Rhaetian', 'Hettangian', 'Sinemurian', 'Pliensbachian', 'Toarcian', 'Aalenian', 'Bajocian', 'Bathonian', 'Callovian', 'Oxfordian', 'Kimmeridgian', 'Tithonian', 'Berriasian', 'Valanginian', 'Hauterivian', 'Barremian', 'Aptian', 'Albian', 'Cenomanian', 'Turonian', 'Coniacian', 'Santonian', 'Campanian', 'Maastrichtian', 'Danian', 'Selandian-Thanetian', 'Ypresian', 'Lutetian', 'Bartonian', 'Priabonian', 'Rupelian', 'Chattian', 'Lower Miocene', 'Middle Miocene', 'Upper Miocene', 'Pliocene', 'Pleistocene', 'Holocene'),start=c(580, 560, 550, 538.8, 529, 521, 514.5, 509, 504.5, 500.5, 497, 494.2, 491, 486.85, 477.08, 471.26, 469.42, 458.18, 452.75, 445.21, 443.07, 440.49, 438.59, 432.93, 430.62, 426.74, 425.01, 422.73, 419, 412.4, 410.51, 394.3, 385.3, 378.9, 371.1, 359.3, 346.73, 330.34, 323.4, 315.15, 307.02, 303.68, 298.89, 293.52, 290.51, 283.3, 274.37, 269.21, 264.34, 259.55, 254.24, 251.9, 249.88, 246.7, 241.46, 237, 227.3, 209.51, 201.36, 199.46, 192.9, 184.2, 174.7, 170.9, 168.17, 165.29, 161.53, 154.78, 149.24, 143.1, 137.7, 132.6, 126.5, 121.4, 113.2, 100.5, 93.9, 89.39, 85.7, 83.65, 72.17, 66.04, 61.66, 56, 48.07, 41.03, 37.71, 33.9, 27.29, 23.04, 15.99, 11.63, 5.33, 2.59, 0.0117), end=c(560, 550, 538.8, 529, 521, 514.5, 509, 504.5, 500.5, 497, 494.2, 491, 486.85, 477.08, 471.26, 469.42, 458.18, 452.75, 445.21, 443.07, 440.49, 438.59, 432.93, 430.62, 426.74, 425.01, 422.73, 419, 412.4, 410.51, 394.3, 385.3, 378.9, 371.1, 359.3, 346.73, 330.34, 323.4, 315.15, 307.02, 303.68, 298.89, 293.52, 290.51, 283.3, 274.37, 269.21, 264.34, 259.55, 254.24, 251.9, 249.88, 246.7, 241.46, 237, 227.3, 209.51, 201.36, 199.46, 192.9, 184.2, 174.7, 170.9, 168.17, 165.29, 161.53, 154.78, 149.24, 143.1, 137.7, 132.6, 126.5, 121.4, 113.2, 100.5, 93.9, 89.39, 85.7, 83.65, 72.17, 66.04, 61.66, 56, 48.07, 41.03, 37.71, 33.9, 27.29, 23.04, 15.99, 11.63, 5.33, 2.59, 0.0117, 0),col=c('#fcd589', '#fdd587', '#fed583', '#a9be93', '#b6c29e', '#b5ca9f', '#c0ceaa', '#c1d6af', '#cddbb8', '#d6e1c1', '#d8e8c4', '#e4efcf', '#edf2db', '#0dac98', '#0eb1a0', '#67c0ae', '#79c5b8', '#9bceaf', '#a6d5c3', '#b6d9c3', '#b3dccc', '#c1e1d6', '#cae8e0', '#cce7d8', '#d6ebe2', '#d6ecea', '#e2f1ec', '#ebf5ec', '#eac378', '#ebcd87', '#edd595', '#f5da93', '#f5e2a0', '#f4edc3', '#f4f0d5', '#9db989', '#b7c089', '#cdc888', '#a6c9cd', '#bed3ce', '#cad8d9', '#d6dcda', '#e07f6c', '#e18a76', '#e39684', '#e49f90', '#f49984', '#f4a692', '#f6af9b', '#fac4b8', '#facfc6', '#b266a6', '#bb71ac', '#c793c3', '#d0a0c8', '#d1b3d5', '#dbc1de', '#e5cbe4', '#22b5e9', '#5ebeee', '#86c9f3', '#a3d1f3', '#a2d8f0', '#b0dff1', '#bce3f2', '#cae6f2', '#cce8fd', '#d4eefd', '#e0f2fc', '#9ec979', '#a8d182', '#b7d690', '#c2da9c', '#cfe1a7', '#d9e8b1', '#c6d86c', '#d2dd77', '#dce383', '#e5e88f', '#efec9b', '#f5f1a7', '#fbc27f', '#fccb87', '#f7ba8e', '#f9c39f', '#fcceac', '#fcd7ba', '#fedfb3', '#ffeac3', '#fff14a', '#fef26b', '#fef488', '#fff7b2', '#fff2c5', '#fff5eb'))
}
  
  if(!is.null(phylo)){
intervals$start<-tsconv(intervals$start,phylo)
intervals$end<-tsconv(intervals$end,phylo)
  }
  
  
  if(is.null(col.txt)){
    col.txt<-intervals$col}
    
if(length(ylim)==1 & !is.null(phylo)){
lastPP <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
c(min(lastPP$y.lim),ylim)->ylim
}else if(length(ylim)==1){ylim<-c(0,ylim)}

  for (i in 1:nrow(intervals)) {
    ###
    if(bw!=T){polygon(
      x=c(intervals$start[i], intervals$end[i], intervals$end[i], intervals$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = add.alpha(intervals$col[i],alpha=alpha),
      border = border
    )}else if(bw==T){
    polygon(
      x=c(intervals$start[i], intervals$end[i], intervals$end[i], intervals$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = "white",
      border = border
    )
    ###
    }
    
    if(names==TRUE){
    if(length(col.txt)>1){
    text(x=mean(c(intervals$start[i],intervals$end[i])),y=txt.y(ylim), adj=adj.txt,intervals$interval[i],col=col.txt[i])}else{
    text(x=mean(c(intervals$start[i],intervals$end[i])),y=txt.y(ylim), adj=adj.txt,intervals$interval[i],col=col.txt)
    }
    }
  }

}
##
