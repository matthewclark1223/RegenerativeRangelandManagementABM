library(tidyverse)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis

#setwd("C:/Users/Matt/OneDrive - Imperial College London/Documents/ICL_Scaling/Herding4Health/ABMPaper")

nc_data <- nc_open('../Evapotrans/Evapotrans.nc')
# Save the print(nc) dump to a text file
{
  sink('gimms3g_ndvi_1982-2012_metadata.txt')
  print(nc_data)
  sink()
}


lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector


ndvi.array <- ncvar_get(nc_data, "evspsbl") # store the data in a 3-dimensional array
dim(ndvi.array) 

fillvalue <- ncatt_get(nc_data, "evspsbl", "_FillValue")
fillvalue
nc_close(nc_data) 
ndvi.array[ndvi.array == fillvalue$value] <- NA

 
  
ndvi.slice <- ndvi.array[, , 1] 
dim(ndvi.slice)
r <- raster(t(ndvi.slice), 
            xmn=min(lon), xmx=max(lon), 
            ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))


r <- flip(r, direction='y')

nslice<-dim(ndvi.array)[3] 

for(i in 2:nslice){
  ndvi.slice <- ndvi.array[, , i] 
  dim(ndvi.slice)
  r2 <- raster(t(ndvi.slice), 
              xmn=min(lon), xmx=max(lon), 
              ymn=min(lat), ymx=max(lat), 
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  
  r2 <- flip(r2, direction='y')
  r<-raster::stack(r,r2)
  
}

plot(r)


rdf<-raster::as.data.frame(r,xy=T)
rdf<-rdf%>%pivot_longer(cols=3:374, values_to = "EVT",names_to = "Time")




ggplot(AOI)+geom_tile(data=rdf,aes(x=x,y=y,fill=EVT))+
  geom_sf(fill=alpha("white",alpha=0.01))




library(gganimate)
ggplot(AOI)+geom_tile(data=rdf,aes(x=x,y=y,fill=EVT))+
  geom_sf(fill=alpha("white",alpha=0.01))+ 
  # Here comes the gganimate code
  transition_states(
    as.factor(Time),
   # transition_length = 2,
    state_length = 0.25
  ) 

r1<-r[[1]]
r1<-crop(r1,AOI)
r1<-mask(r1,AOI)
rdf1<-raster::as.data.frame(r1,xy=T)
names(rdf1)[3]<-"EVT"
rdf1<-na.omit(rdf1)
ggplot(AOI)+geom_tile(data=rdf1,aes(x=x,y=y,fill=EVT))+
  scale_fill_viridis_c(option="cividis", direction = -1,
                       name="Evapotranspiration\n2050" )+
  geom_sf(fill=alpha("white",alpha=0.01),color="black")+
  theme_void()
