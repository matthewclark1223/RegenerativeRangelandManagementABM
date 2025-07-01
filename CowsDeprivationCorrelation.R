library(tidyverse)
library(raster)
library(sf)
Dep <- raster::raster("../../General/Deprivation/povmap-grdi-v1.tif")
AOI<-read_sf("../../General/CountryShapes/AOI.shp")
Dep<-raster::crop(Dep,AOI)

Cows <- raster::raster("../../General/Cattle2015/5_Ct_2015_Da.tif")
Cows<-raster::crop(Cows,AOI)

Dep <- resample(Dep, Cows)

Cows<-raster::as.data.frame(Cows,xy=T)
Dep<-raster::as.data.frame(Dep,xy=T)
Cows$Dep<-Dep$povmap.grdi.v1

Cows<-Cows%>%dplyr::filter(X5_Ct_2015_Da>0)%>%
  dplyr::filter(is.na(X5_Ct_2015_Da)==F)%>%
  dplyr::filter(is.na(Dep)==F)

cor(Cows$X5_Ct_2015_Da,Cows$Dep )
