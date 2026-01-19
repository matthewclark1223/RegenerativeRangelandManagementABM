library(sf)


# AOI<-read_sf("./AOI.shp")
# AOI<-AOI%>%st_buffer(dist =0.01 )%>%st_union() #small buffer because countries don't perfectly align and it makes a gap. 
# 
# grids<-sf::st_make_grid(AOI, cellsize = 0.25 )
# grids <- grids %>% st_as_sf()
# #index of grids fully or partially covered by the AOI
# intersections_index <- st_intersects(AOI, grids, sparse = F)
# 
# # sf object of only the grids we want
# grids <- grids[intersections_index, ]
# library(tidyverse)
# ggplot(grids)+geom_sf()+geom_sf(data=AOI,color="green",fill=NA)
# 
# grids$Index<-1:nrow(grids)
# 
# library(terra)

#####THIS CODE MAKES THE RASTER WE LOAD BELOW, SO NOT NEEDED, JUST HERE TO SEE PAST PROCESSING
#Load veg height
#VegHeight<-terra::rast("./gpw_short.veg.height_egbt_m_30m_s_20240101_20241231_go_epsg.4326_v1.tif")
#VegHeight<-terra::crop(VegHeight,st_bbox(AOI))
#terra::writeRaster(VegHeight, "./AOI_VegHeight.tif", overwrite=TRUE)
#VegHeight<-terra::rast("./AOI_VegHeight.tif")
#raster::plot(VegHeight)

#Load veg class
#VegClass<-terra::rast("./gpw_nat.semi.grassland_rf.med.filt_p_30m_20240101_20241231_go_epsg.4326_v2.tif")
#VegClass<-terra::crop(VegClass,st_bbox(AOI))
#terra::writeRaster(VegClass, "./AOI_VegNatGrassProb.tif", overwrite=TRUE)
#VegClass<-terra::rast("./AOI_VegNatGrassProb.tif")
#raster::plot(VegClass)

#Don't consider vegetation height for pixels that are not likely to be natural or semi-natural vegetation.
#VegHeight[VegClass < 50] <- NA
#or are unclassified
#VegHeight[is.na(VegClass) ] <- NA
#terra::writeRaster(VegHeight, "./AOI_VegHeight_NatOnly.tif", overwrite=TRUE)
#VegHeight<-terra::rast("./AOI_VegHeight_NatOnly.tif")

#ggplot(AOI)+geom_sf()+
#  geom_raster(data=VegHeight)

#raster::plot(VegHeight)
#hist(VegHeight, 
 #    main = "Histogram of Raster Values", # Add a main title
  #   xlab = "Pixel Values",               # Label the x-axis
   #  col = "blue"                         # Specify a bar color
#)



#VegHeightDF<-terra::extract(VegHeight,grids, fun=median,na.rm=TRUE)
#grids$MedNatGrassHeight<-VegHeightDF$gpw_short.veg.height_egbt_m_30m_s_20240101_20241231_go_epsg.4326_v1
#st_write(grids, "./GriddedNatGrassHeightAOI.shp")
grids<-read_sf("./GriddedNatGrassHeightAOI.shp")
grids<-grids%>%mutate(PercGrassHeight=(MedNatGrassHeight/max(MedNatGrassHeight,na.rm=T))*100)
P1<-ggplot(grids)+geom_sf(color=NA,aes(fill=PercGrassHeight))+
  scale_fill_viridis_c(name="Median natural and\nsemi-natural short\nvegetation height\n(% of max)")+
geom_sf(data=AOI,fill=NA,color="black")+
  theme_minimal()

###Now get data from toy model
Ras<-raster::stack("./GithubClone/Ts_Data_toy/Landscapes/FOREC_No_one_ADOP_TRUE_FODD_FALSE_PROPCONS_0.1_FORECERR_0_beta_0.1/good/rep001/ts001.tif")
Ras<-Ras[[7]] #the forage cover layer
Ras_df <- raster::as.data.frame(Ras, xy = TRUE, na.rm = TRUE)
P2<-ggplot(Ras_df)+geom_tile(color="white",aes(x=x,y=y,fill=ts001_7))+
  scale_fill_viridis_c(name="Example simulated\nstarting vegetation\ncover (% of max)")+
  theme_minimal()+theme(axis.text = element_blank(),axis.title = element_blank(),
                        axis.ticks = element_blank())+
  coord_equal()

##### 
P3<-ggplot(Ras_df,aes(x=ts001_7))+
  geom_density(fill="blue",alpha=0.75)+
  geom_density(data=grids,aes(x=PercGrassHeight),fill="red",alpha=0.75)+
  theme_minimal()+ylab("Pixel density")+xlab("Percent of vegetation maximum")+
  annotate(geom = "text",x=54,y=0.02, label="Empirical\nlandscape",fontface="bold",color="red")+
  annotate(geom = "text",x=7,y=0.025, label="Simulated\nlandscape",fontface="bold",color="blue")


#library(patchwork)
PC<-(P1 + P2) / P3
PC
