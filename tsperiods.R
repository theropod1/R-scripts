ts.periods <- function(phylo=NULL,alpha=1,names=T,col.txt=NULL,border=NA,ylim=c(0,0.5),adj.txt=c(0.5,0.5),txt.y=mean){
  # Data for geological periods
  periods <- data.frame(
    period = c("Quaternary", "Neogene", "Paleogene", "Cretaceous", "Jurassic", "Triassic", "Permian", "Carboniferous", "Devonian", "Silurian", "Ordovician", "Cambrian"),
    start = c(0, 2.58, 23.03, 66, 145, 201.3, 252.17, 298.9, 358.9, 419.2, 443.8, 485.4),
    end = c(2.58, 23.03, 66, 145, 201.3, 252.17, 298.9, 358.9, 419.2, 443.8, 485.4, 541)
  )

  if(!is.null(phylo)){
periods$start<-tsconv(periods$start,phylo)
periods$end<-tsconv(periods$end,phylo)
  }
  
  # Color palette based on ICS color scheme
  colors <-paste0("#",c("F9F97F","FFE619","FD9A52","7FC64E","34B2C9","812B92","F04028","67A599","CB8C37","B3E1B6","009270","7FA056"))
  
  if(is.null(col.txt)){
    col.txt<-colors}


  # Plotting
  #plot(1, type = "n", xlab = "", ylab = "", axes = FALSE, xlim = c(0, 550), ylim = c(0, 1))

  for (i in 1:nrow(periods)) {
    polygon(
      x=c(periods$start[i], periods$end[i], periods$end[i], periods$start[i]),
      c(ylim[1], ylim[1], ylim[2], ylim[2]),
      col = add.alpha(colors[i],alpha=alpha),
      border = border
    )
    
    if(names==TRUE){
    if(length(col.txt)>1){
    text(x=mean(c(periods$start[i],periods$end[i])),y=txt.y(ylim), adj=adj.txt,periods$period[i],col=col.txt[i])}else{
    text(x=mean(c(periods$start[i],periods$end[i])),y=txt.y(ylim), adj=adj.txt,periods$period[i],col=col.txt)
    }
    }
  }

}
