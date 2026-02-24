##function
#Compute the procrustes distance for an array of aligned procrustes coordinates of specimens (a) to any given specimen in the array (b), or the mean shape (if mean==TRUE). Can be used to manually calculate procrustes distances, for example if geomorph did not output a distance matrix
p.dist <- function (a,b=NULL, mean=FALSE) {
                p <- dim(a)[3]
                if(mean==TRUE){shap <- apply (a, c(1, 2), mean)}else{ #if I wanted mean shape instead
                shap<-a[,,b]}
                pdist <- vector ("numeric", p)#make empty vector to fill with procrustes distances
                for(i in 1:p) {
                        pdist[i] <- sqrt (sum ((a[,,i] - shap)^2)) # calculate procrustes distances
                        }
                return(pdist)
}
##

##function
#Wrapper around p.dist to allow calculation of a procrustes distance matrix, comparing every specimen to every specimen
p.dist_<-function(arr,names=""){
dim(arr)[3]->l
matrix(numeric(l*l),nrow=l,ncol=l)->pdm
    if(length(names)==l){
rownames(pdm)<-names
colnames(pdm)<-names
    }

    for(i in 1:l){
p.dist(a=arr,b=i)->pdm[i,]
    }
return(pdm)
}
#usage example:
#assume you have used geomorph::gpagen to do a procrustes alignment of coordinate sets, saved in variable procustes
#read.table("specimen_names.txt", header=T)->n
#p.dist_(procrustes$coords, n)->pdists#calculate distance matrix using specimen ids as row and col names

#ape::plot.phylo(phangorn::upgma(pdists))#calculate and plot a upgma tree based on the distance matrix
##

##function Csize()
#' compute the centroid size of specimens in a morphometric array
#' @param tps an array of landmark coordinates such as is returned by geomorph::readland.tps()
#' @param scale default 1, data will be assumed to be scaled; otherwise, scale (px per unit) for each specimen
#' @return a centroid size or vector of centroid sizes
#' @export Csize

Csize<-function(tps,scale=1){

dim(tps)[3]->n
out<-numeric(n)


if(is.null(scale)) scale<-1
if(length(scale)<n) rep(scale,n)[1:n]->scale

for(i in 1:n){
tps[,,i]->tps_i
tps_i/scale[i]->tps_i_scaled
colMeans(tps_i_scaled)->centroidXY
#square root of the summed squared distances of each landmark to the centroid.
sqrt(sum(( tps_i_scaled[,1]-centroidXY[1])^2 + (tps_i_scaled[,2]-centroidXY[2])^2 ))->out[i]
}

names(out)<-dimnames(tps)[[3]]

return(out)
}
