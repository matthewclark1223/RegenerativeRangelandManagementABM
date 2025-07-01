library(tidyverse)
library(raster)

#AOI<-Bound

Cows<-raster("../General/Cattle2020.tif")
CowsRescale<-raster("../General/Area10km.tif")
Cows[]<-Cows[]*CowsRescale[]

Cows<-crop(Cows,AOI)
Cows<-mask(Cows,AOI)

Cowsdf<-raster::as.data.frame(Cows,xy=T)
names(Cowsdf)[3]<-"Cattle"
Cowsdf<-na.omit(Cowsdf)
ggplot(AOI)+geom_tile(data=Cowsdf,aes(x=x,y=y,fill=Cattle) )+
  scale_fill_viridis_c(option="plasma", direction = -1,
                       name="Cattle")+
  geom_sf(fill=alpha("white",alpha=0.01),color="black")+
  theme_void()
