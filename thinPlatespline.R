

#\/\//\/\/\/\/\/\/\/\/\/\/\/\

# Thin plate Spine Interpolation for MatLab calculated
# delta-changes between historical rcp IPCC data

#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\

library(fields)
library(R.matlab)

rm(list = ls())
setwd('A:/PhD/chapter1/PIBCM/ipcc/fundulus/rcp/2100')

ipcc<-readMat('dcMPI-r85-2100.mat')
clim<-readMat('copernicus-climDelta.mat')

delta<-ipcc$delta
dlat<-ipcc$latitude
dlon<-ipcc$longitude
clat<-clim$climDelta[,1]
clon<-clim$climDelta[,2]


lat1<-seq(20.25,60,0.5)
lon1<-seq(-96.75,-54,0.5)
grid.list1<-list(lon1,lat1)
xy1<-make.surface.grid(grid.list1)

dxy<-length(clat)
dcmat<-matrix(NaN,nrow = dxy,ncol = 1)
fcmat<-matrix(NaN,nrow = dxy,ncol = 12)




#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

# Matching IPCC grid centers to Copernicus grid centers for all 12 months

for (mlp in 1:12) {
  
mon<-delta[,,mlp]
tmon<-as.vector(mon)
grid.list<-list(dlon,dlat)
xy<-make.surface.grid(grid.list)
res1<-Tps(xy,tmon,lon.lat = TRUE)
res2<-predict(res1,xy1)



   for (ij in 1:dxy) {
  
  fl2<-which(as.matrix(xy1[,1])==clon[ij] & as.matrix(xy1[,2])
             ==clat[ij])

  ano<-res2[fl2]
  dcmat[ij]=ano
  
}
 fcmat[,mlp]=dcmat
}

#////////////////////////////////////////////////////////////////


final.matrix<-matrix(NaN,nrow = dxy,ncol = 14)


 for (abj in 1:12) {

   pred.tem<-fcmat[,abj]+clim$climDelta[,2+abj]
   final.matrix[,2+abj]<-pred.tem
      
 }
 
final.matrix[,1:2]<-cbind(clat,clon)



# Change the file name before saving

 writeMat("finalMPI-RCP85-2100.mat",mmeansst= final.matrix)
 
 

#***************************************************************
# Comparing predictor data with predictions

exy<-dim(as.matrix(xy))
ecmat<-matrix(NaN,nrow = exy[1],ncol = 1)

for (jk in 1:exy[1]) {
  fl3<-which(as.matrix(xy1[,1])==xy[jk,1] & as.matrix(xy1[,2])
             ==xy[jk,2])
  
  ano1<-res2[fl3]
  ecmat[jk]=ano1
  
}

ccmat<-cbind(ecmat,tmon)
plot(ecmat,tmon)

pred.stat<-lm(tmon~ecmat)
abline(pred.stat)
summary(pred.stat)
#**************************************************************
