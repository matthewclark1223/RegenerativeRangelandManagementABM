source("../RCode/MakeStylizedLandscape.R")
source("../RCode/LandscapeTheme.R")
library(tidyverse)
df<-raster::as.data.frame(rstack,xy=T) #Rstack to DF
Plots<-terra::rast(rstack[["PlotID"]]) #vectorize plots
Plots<-terra::as.polygons(Plots)
Plots<-sf::st_as_sf(Plots)

Community<-terra::rast(rstack[["CommID"]])#vectorize communities
Community<-terra::as.polygons(Community)
Community<-sf::st_as_sf(Community)


#Plot
p1<-ggplot(Plots)+geom_tile(data=df,aes(x=x,y=y,fill=T0Grass) )+
  scale_fill_distiller(palette ="Greens", direction = 1,
                       name="T0 grass cover",
                       limits=c(0,100),
                       breaks=c(0,20,40,60,80,100),
                       labels=c(0,20,40,60,80,100)
                       #trans = scales::log10_trans()
                       )+
  geom_sf(fill=NA,color="#f0f0f0")+
  geom_sf(data=Community,aes(geometry=geometry),
          fill=NA,
          color="black",linewidth=1)+LandscapeTheme

ggsave("../ConceptDiagram/T0Grass.png",p1,dpi=350,units = "in",width = 6.5, height = 4, bg="black")

#Conservation and resting
p2<-ggplot(Plots)+
  geom_tile(data=df,aes(x=x,y=y,fill=as.character(Cons) ))+
  scale_fill_manual(values = c("grey","darkgreen"),
                    name="T0 Community engaged\nin conservation",
                    labels=c("False","True"))+
  ggnewscale::new_scale("fill") +
  geom_tile(data=filter(df,Grazed==0),aes(x=x,y=y,fill=as.character(Grazed)) )+
  scale_fill_manual(values = c("green"),
                    name=NULL,
                    labels=c("Plot resting\n(not grazed)"))+
  geom_sf(fill=NA,color="#f0f0f0")+
  geom_sf(data=Community,aes(geometry=geometry),
          fill=NA,
          color="black",linewidth=1)+LandscapeTheme

ggsave("../ConceptDiagram/Conservation.png",p2,dpi=350,units = "in",width = 6.5, height = 4, bg="black")

#Animals
p3<-
  ggplot(Plots)+geom_tile(data=df,aes(x=x,y=y,fill=Animal) )+
  scale_fill_viridis_c(option="D", direction = -1,
                       name="T0 Animals"
                       #trans = scales::log10_trans()
  )+
  geom_sf(fill=NA,color="#f0f0f0")+
  geom_sf(data=Community,aes(geometry=geometry),
          fill=NA,
          color="black",linewidth=1)+LandscapeTheme

ggsave("../ConceptDiagram/Animals.png",p3,dpi=350,units = "in",width = 6.5, height = 4, bg="black")

#Rainfall
p4<-ggplot(Plots)+geom_tile(data=df,aes(x=x,y=y,fill=T0Rain) )+
  scale_fill_viridis_c(option="E", direction = -1,
                       name="T0 Rain"
                       #trans = scales::log10_trans()
  )+
  geom_sf(fill=NA,color="#f0f0f0")+
  geom_sf(data=Community,aes(geometry=geometry),
          fill=NA,
          color="black",linewidth=1)+LandscapeTheme

ggsave("../ConceptDiagram/Rain.png",p4,dpi=350,units = "in",width = 6.5, height = 4, bg="black")



