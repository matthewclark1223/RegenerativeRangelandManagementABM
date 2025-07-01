library(tidyverse)
library(sf)
setwd("C:/Users/Matt/OneDrive - Imperial College London/Documents/ICL_Scaling/Herding4Health/ABMPaper")# home
setwd("C:/Users/mclark1/OneDrive - Imperial College London/Documents/ICL_Scaling/Herding4Health/ABMPaper")# work
BWA<-read_sf("../General/CountryShapes/BWA/gadm41_BWA_0.shp")
LSO<-read_sf("../General/CountryShapes/LSO/gadm41_LSO_0.shp")
MOZ<-read_sf("../General/CountryShapes/MOZ/gadm41_MOZ_0.shp")
NAM<-read_sf("../General/CountryShapes/NAM/gadm41_NAM_0.shp")
SA<-read_sf("../General/CountryShapes/SA/gadm41_ZAF_0.shp")
SWZ<-read_sf("../General/CountryShapes/SWZ/gadm41_SWZ_0.shp")
ZWE<-read_sf("../General/CountryShapes/ZWE/gadm41_ZWE_0.shp")

AOI<-rbind(BWA,LSO,MOZ,NAM,SA,SWZ,ZWE)

st_write(AOI,"../General/CountryShapes/AOI.shp")
