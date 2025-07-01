library(raster)
library(sf)
Dep <- raster::raster("../../General/Deprivation/povmap-grdi-v1.tif")
AOI<-read_sf("../../General/CountryShapes/AOI.shp")
Dep<-raster::crop(Dep,AOI)
Dep<-raster::as.data.frame(Dep,xy=T)
Dep<-Dep[complete.cases(Dep), ] 
names(Dep)[3]<-"Deprivaton"

#Cows
#Cows <- raster::raster("../../General/Cattle2015/5_Ct_2015_Da.tif")
#Cows<-raster::crop(Cows,AOI)
#Cows<-raster::as.data.frame(Cows,xy=T)
#Cows<-Cows[complete.cases(Cows), ] 
#names(Cows)[3]<-"Cows"
#Cows$Cows<-ceiling(Cows$Cows)
#Cows<-Cows%>%filter(Cows!=0)
