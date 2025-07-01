library(tidyverse)
library(raster)
library(sf)


AOI<-read_sf("../../General/CountryShapes/AOI.shp")
AOI<-AOI%>%st_buffer(dist =0.01 )%>%st_union() #small buffer because countries don't perfectly align and it makes a gap. 

grids<-sf::st_make_grid(AOI, cellsize = 0.25 )
grids <- grids %>% st_as_sf()
#index of grids fully or partially covered by the AOI
intersections_index <- st_intersects(AOI, grids, sparse = F)

# sf object of only the grids we want
grids <- grids[intersections_index, ]
ggplot(grids)+geom_sf()+geom_sf(data=AOI,color="green",fill=NA)

grids$Index<-1:nrow(grids)
library(rgee)




# Function to calculate annual precipitation for a single year
get_yearly_precip <- function(year) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year + 1, "-01-01")
  
  image <- ee$ImageCollection("UCSB-CHG/CHIRPS/PENTAD") %>%
    ee$ImageCollection$filterDate(start_date, end_date) %>%
    ee$ImageCollection$select("precipitation") %>%
    ee$ImageCollection$reduce(ee$Reducer$sum()) %>%
    ee$Image$rename(paste0("precip_", year))
  
  return(image)
}

#ee_check()  #this makes sure the machine that's running this has python that is linked to a GEE API

#ee_Initialize(user = 'matthewclark989@u.boisestate.edu')
years<-1981:2024

#years<-1981:1982

for(i in years){
year <- i

gridsSub1<-grids[1:(ceiling(nrow(grids)/2)),] 

gridsSub2<-grids[(nrow(gridsSub1)+1):nrow(grids),] 

# Apply the function to all year and convert to multiband image
annual_precip_images <- lapply(year, get_yearly_precip)

# Combine all year into one multi-band image
annual_precip_multiband <- ee$Image$cat(annual_precip_images)

# Extract the multi-band image to the features in 'grids'
gridsSub1 <- ee_extract(x = annual_precip_multiband, y = gridsSub1["Index"], sf = TRUE,scale=10000)


# Extract the multi-band image to the features in 'grids'
gridsSub2 <- ee_extract(x = annual_precip_multiband, y = gridsSub2["Index"], sf = TRUE,scale=10000)

gridsCombined<-rbind(gridsSub1,gridsSub2)
gridsCombined$Year<-i
names(gridsCombined)[2]<-"precip"

assign(paste0("grids",i),gridsCombined)

}


grids<-rbind(grids1981, grids1982, grids1983, grids1984, grids1985, grids1986, grids1987,
grids1988, grids1989, grids1990, grids1991, grids1992, grids1993, grids1994, 
 grids1995, grids1996 ,grids1997 ,grids1998 ,grids1999, grids2000 ,grids2001, 
 grids2002, grids2003, grids2004, grids2005, grids2006, grids2007, grids2008, 
grids2009, grids2010, grids2011, grids2012, grids2013, grids2014, grids2015 ,
grids2016, grids2017, grids2018, grids2019, grids2020, grids2021, grids2022, 
grids2023, grids2024)

st_write(grids, "../AOIHistoricRainfallGridded.shp")

grids<-read_sf("../AOIHistoricRainfallGridded.shp")
library(spdep)

#Get yearly Moran's i. W'll use the median for the stylized baseline. 

Morans<-data.frame(Year=rep(NA,length(years)),Stat=rep(NA,length(years)))

for(i in 2:length(years)){
  year<-years[i]

zz<-grids%>%filter(Year==year)%>%
filter(is.na(precip)==FALSE)

nb <- poly2nb(zz, queen = TRUE) # queen shares point or border
connected <- which(card(nb) > 0)  # Keep only polygons with neighbors
zz <- zz[connected, ]       # Subset only connected polygons
nb <- poly2nb(zz, queen = TRUE)  # Recompute neighborhood
nb <- nb2listw(nb, style = "W")    # Create weight list

# Global Moran's I
gmoran <- moran.test(zz$precip, nb,
                     alternative = "greater")
I_Val<-gmoran$estimate[[1]]
Morans[i,]$Year<-year
Morans[i,]$Stat<-I_Val
}

hist(Morans$Stat)


ggplot(grids)+geom_line(aes(x=Year,y=precip,group=Index),alpha=0.1)

library(tseries)

###This is bad. We don't want autocorrelation, we want variance around the mean!!

Grid_SD<-grids %>%
  filter(is.na(precip)==FALSE)%>%
  group_by(Index) %>%
  summarise(sd = sd(precip))

hist(Grid_SD$sd)
median(Grid_SD$sd)/mean(grids$precip,na.rm=T) #SD is 22% of mean



grids%>%filter(Year==2024)%>%
ggplot(.)+geom_sf(aes(fill=precip),color=alpha("#525252",0.5))+#geom_sf(data=AOI,color="green",fill=NA)+
  scale_fill_viridis_c(option="E", direction = -1, name="Total 2024\nprecipitation (mm)")+
  annotate(geom="text",x=20,y=-12,label="Moran's i = 0.98")+
  scale_x_continuous(n.breaks=3)+scale_y_continuous(n.breaks=3)+
  theme_minimal()+theme(axis.title = element_blank())

########### Make supplemental figure with stylized plots

p1<-grids%>%filter(Year==2024)%>%
  ggplot(.)+geom_sf(aes(fill=precip),color=alpha("#525252",0.5))+#geom_sf(data=AOI,color="green",fill=NA)+
  scale_fill_viridis_c(option="E", direction = -1, name="Total 2024\nprecipitation (mm)")+
  annotate(geom="text",x=20,y=-12,label="Moran's i = 0.98")+
  scale_x_continuous(n.breaks=3)+scale_y_continuous(n.breaks=3)+
  theme_minimal()+theme(axis.title = element_blank())

p2<-p4+ggtitle("Moran's i = 0.98")

cowplot::plot_grid(p1, p2,align="v",
          ncol = 2)
#After, add forecasted under climate change and stylized equivalent 



