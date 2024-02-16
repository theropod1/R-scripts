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
