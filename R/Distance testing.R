#libraries
library(tidyverse)
library(rgl)
library(Rvcg) # package for working with 3d meshes in R
library(Morpho)
library(Arothron)
#library(MBA) # Spline regression
knitr::opts_chunk$set(cache = T)
#grabbing the shoes and changing them into point clouds and centering them in order to test distances
stl_new <- list.files("./data-raw/My_Scans", pattern = ".stl", full.names = T)
stl_file_path1 <- stl_new[str_detect(stl_new, "SonyL")][1]
sony1 <- Rvcg::vcgImport(stl_file_path1, clean = T)
stl_file_path2 <- stl_new[str_detect(stl_new, "SonyL")][3]
sony2 <- Rvcg::vcgImport(stl_file_path2, clean = T)
sony1c <- center3d(sony1)
sony2c <- center3d(sony2)
sony1shoe<-shoe_coord(sony1c)
sony2shoe<-shoe_coord(sony2c)

sony1pca<-prcomp(sony1shoe, center = TRUE,scale. = FALSE)
sony2pca<-prcomp(sony2shoe, center = TRUE,scale. = FALSE)

#rotation
rotations <- list(
  rotationMatrix(matrix=diag(c(1,1,1))),
  rotationMatrix(matrix=diag(c(-1,1,1))),
  rotationMatrix(matrix=diag(c(1,-1,1))),
  rotationMatrix(matrix=diag(c(1,1,-1))),
  rotationMatrix(matrix=diag(c(1,-1,-1))),
  rotationMatrix(matrix=diag(c(-1,1,-1))),
  rotationMatrix(matrix=diag(c(-1,-1,1))),
  rotationMatrix(matrix=diag(c(-1,-1,-1)))
)

#after alignment
sony1tran<-transform3d(sony1c, matrix = rotationMatrix(matrix = sony1pca$rotation))
sony2tran<-transform3d(sony2c, matrix =rotationMatrix(matrix = sony2pca$rotation))
scan1<-transform3d(sony1tran, matrix = rotations[[8]])
scan2<-transform3d(sony2tran, matrix = rotations[[7]])
icp12<-icp(scan1, scan2, iterations = 100)


#testing distance to start with from the original icp function:

KDtree <- vcgCreateKDtreeFromBarycenters(sony2c)
proMesh <- vcgClostOnKDtreeFromBarycenters(KDtree,sony1c,sign=F,k=50,threads=0)
x1 <- vert2points(sony1c)
x2 <- vert2points(proMesh)
dists <- abs(proMesh$quality)
qud <- quantile(dists,probs=1)#max distance


normchk <- normcheck(sony1c,proMesh,0)
max(dists)

#now seeing if alligning is working correctly or that the distances are different

KDtree1 <- vcgCreateKDtreeFromBarycenters(sony2tran)
proMesh1 <- vcgClostOnKDtreeFromBarycenters(KDtree1,sony1tran,sign=F,k=50,threads=0)
x1 <- vert2points(sony1tran)
x2 <- vert2points(proMesh1)
dists <- abs(proMesh1$quality) #idk what units this would be on
max(dists)

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

#############################################################################################

testing<-Morpho::meshDist(scan1, scan2)

originaldistance<-Morpho::meshDist(sony1c, sony2c)
max(icpdistance$dists)

rgl::plot3d(originaldistance, aspect="iso", add=TRUE)
axes3d()

pcaligneddistance<-Morpho::meshDist(scan1, scan2)

icpdistance6<-Morpho::meshDist(icp16, scan6)


testing<-Morpho::meshDist(scan1, scan6)


stl_file_path6 <- stl_new[str_detect(stl_new, "SonyL")][6]
sony6 <- Rvcg::vcgImport(stl_file_path6, clean = T)
sony6c <- center3d(sony6)


originaldistance6<-Morpho::meshDist(sony1c, sony6c)#this shows it visually
sony6shoe<-shoe_coord(sony6c)
sony6pca<-prcomp(sony6shoe, center = TRUE,scale. = FALSE)
sony6tran<-transform3d(sony6c, matrix =rotationMatrix(matrix = sony6pca$rotation))
scan6<-transform3d(sony6tran, matrix = rotations[[7]])
icp16<-icp(scan1, scan6, iterations = 100)


###################This works the best and most consistantly as a metric of distance#########################################################################################

promesh <- Rvcg::vcgClostKD(sony1,sony2,sign=T,threads = 1)
closest <- promesh$vb[1:3,]
distances<- promesh$quality

View(head(distances))
low <- quantile(distances,probs=0.05,na.rm = TRUE)
upper<-quantile(distances, probs=.95, na.rm = TRUE)

closestsequence <- seq(from=low,to=upper,length.out=25)

closestsequence




promeshac <- Rvcg::vcgClostKD(scan1,scan2,sign=T,threads = 1)
closestac <- promeshac$vb[1:3,]
distancesac<- promeshac$quality

View(head(distancesac))
lowac <- quantile(distancesac,probs=0.05,na.rm = TRUE)
upperac<-quantile(distancesac, probs=.95, na.rm = TRUE)

closestsequenceac <- seq(from=lowac,to=upperac,length.out=25)

closestsequenceac
#############################################################################################################

#these are all getting different response so





