


PrecipTimeseries<-function(TimeSteps=50,StandardDev=0.2){
  
Change<-rnorm(n=TimeSteps,mean=0,sd=StandardDev)
Rain<-terra::rast(rstack[["T0Rain"]]) #vectorize plots
sim_rasters<-list()
sim_rasters[[1]]<-Rain #Include t0 rain
for(i in 1:TimeSteps){
  
  r_sim <- Rain
  r_sim[]<-Rain[]+Change[i]*Rain[]
  sim_rasters[[(i+1)]] <- r_sim
}


PrecipStack<-terra::rast(sim_rasters)
PrecipStack<-c(PrecipStack,terra::rast(rstack[["CommID"]]),terra::rast(rstack[["PlotID"]]))
PrecipStack<-terra::as.polygons(PrecipStack,dissolve=FALSE)
PrecipStack<-sf::st_as_sf(PrecipStack)

names(PrecipStack)[1:(TimeSteps+1)]<-0:TimeSteps
PrecipStack%>%tidyr::pivot_longer(cols=0:(TimeSteps+1),names_to = "Time",values_to = "Precip")%>%mutate(Time=as.integer(Time))

}

#matrix(PrecipStack)

#####
#RainP<-PrecipStack
#RainP%>%dplyr::filter(Time==45)%>%
#  ggplot(.)+geom_sf(aes(fill=Precip))

#library(gganimate)
#ggplot(RainP)+geom_sf(aes(fill=Precip))+
 # scale_fill_viridis_c(option="E", direction = -1,
  #                     name="Rain"
#  )+
 # geom_sf(fill=NA,color="#f0f0f0")+
  #geom_sf(data=Community,aes(geometry=geometry),
   #       fill=NA,
    #      color="black",linewidth=1)+
  # Here comes the gganimate code
#  transition_states(
 #   Time,
  #  transition_length = 1,
   # state_length = 1
  #)+LandscapeTheme

 #Make animation 
#####
