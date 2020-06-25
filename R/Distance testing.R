#libraries
library(tidyverse)
library(rgl)
library(Rvcg) # package for working with 3d meshes in R
library(Morpho)
library(Arothron)
#library(MBA) # Spline regression
knitr::opts_chunk$set(cache = T)
#grabbing the shoes and changing them into point clouds
stl_new <- list.files("./data-raw/My_Scans", pattern = ".stl", full.names = T)
stl_file_path1 <- stl_new[str_detect(stl_new, "SonyL")][1]
sony1 <- Rvcg::vcgImport(stl_file_path1, clean = T)
stl_file_path2 <- stl_new[str_detect(stl_new, "SonyL")][3]
sony2 <- Rvcg::vcgImport(stl_file_path2, clean = T)
sony1c <- center3d(sony1)
sony2c <- center3d(sony2)
sony1shoe<-shoe_coord(sony1c)
sony2shoe<-shoe_coord(sony2c)


#testing distance to start with:

KDtree <- vcgCreateKDtreeFromBarycenters(sony2c)
proMesh <- vcgClostOnKDtreeFromBarycenters(KDtree,sony1c,sign=F,k=50,threads=0)
x1 <- vert2points(sony1c)
x2 <- vert2points(proMesh)
dists <- abs(proMesh$quality)
qud <- quantile(dists,probs=1)#max distance


normchk <- normcheck(sony1c,proMesh,0)
max(dists)

#now seeing if alligning is working correctly or that the distances are different
sony1pca<-prcomp(sony1shoe, center = TRUE,scale. = FALSE)
sony2pca<-prcomp(sony2shoe, center = TRUE,scale. = FALSE)
sony1tran<-transform3d(sony1c, matrix = rotationMatrix(matrix = sony1pca$rotation))
sony2tran<-transform3d(sony2c, matrix =rotationMatrix(matrix = sony2pca$rotation))
scan1<-transform3d(sony1tran, matrix = rotations[[8]])
scan2<-transform3d(sony2tran, matrix = rotations[[7]])
icp12<-icp(scan1, scan2, iterations = 100)


KDtree1 <- vcgCreateKDtreeFromBarycenters(sony2tran)
proMesh1 <- vcgClostOnKDtreeFromBarycenters(KDtree1,sony1tran,sign=F,k=50,threads=0)
x1 <- vert2points(sony1tran)
x2 <- vert2points(proMesh1)
dists <- abs(proMesh1$quality) #idk what units this would be on
max(dists) #is 31, so something changed


#now making it into a function
distance<-function(mesh1, mesh2){
  KDtree<- Rvcg::vcgCreateKDtreeFromBarycenters(mesh2)
  proMesh <- Rvcg::vcgClostOnKDtreeFromBarycenters(KDtree,mesh1,sign=F,k=50,threads=0)
  dists<- abs(proMesh1$quality)
  return(dists)
}

test<-distance(sony1, sony2)
testing<-distance(sony1c, sony2c)
test2<-distance(scan1, scan2)
test3<-distance(scan2, icp12)

#these are all different scans, but the same distance
qud <- quantile(test,probs=1)
quad2<-quantile(testing, probs = 1)
quad4<-quantile(test3, probs = 1)
