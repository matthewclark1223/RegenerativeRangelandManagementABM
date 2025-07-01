

Dep <- raster("../General/Deprivation/povmap-grdi-v1.tif")
Pop <- raster("../General/LandScan/landscan-global-2022.tif")
Cows<-raster("../General/Cattle2015/5_Ct_2015_Da.tif")

Grass<-raster("../General/GrassCover1km2020.tif")

plotRast<-function(raster,palette,title,direction=1){
  r1<-crop(raster,AOI)
  r1<-mask(r1,AOI)
  rdf1<-raster::as.data.frame(r1,xy=T)
  names(rdf1)[3]<-"layer"
  rdf1<-na.omit(rdf1)
  ggplot(AOI)+geom_tile(data=rdf1,aes(x=x,y=y,fill=layer) )+
    scale_fill_viridis_c(option=palette, direction = direction,
                         name=title, #breaks=c(0,50,1000,80000),
                         trans = scales::log10_trans())+
    geom_sf(fill=alpha("white",alpha=0.01),color="black")+
    theme_void()
}

plotRast(Dep,"magma","Deprivation",-1)
plotRast(Pop,"viridis","Human population",1)
plotRast(Cows,"plasma","Cattle population",-1)
plotRast(Grass,"mako","Grass cover",1)

