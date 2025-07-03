
# Load packages -----------------------------------------------------------

# library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(spdep)

# Create output folder if it doesn't exist
output_dir <- "annual_pred"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Load Data ---------------------------------------------------------------

# 12 months x 10 years = 120 layers per dataset
# Download with slightly higher ymax??!!!
pred1 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp370_r1i1p1f1_gr_20500116-20591216.nc")  # Worst Case (SSP3-7.0)
pred2 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_20500116-20591216.nc")  # Current Policy (SSP2-4.5)
pred3 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_20500116-20591216.nc")  # Optimistic (SSP1-2.6)

# Quick check
plot(pred1[[1]], main = names(pred1)[1]) # January 2050 Worst Case
plot(pred2[[1]], main = names(pred2)[1]) # January 2050 Current Policy
plot(pred3[[1]], main = names(pred3)[1]) # January 2050 Optimistic

plot(pred1[[120]], main = names(pred1)[120]) # December 2059 Worst Case
plot(pred2[[120]], main = names(pred2)[120]) # December 2059 Current Policy
plot(pred3[[120]], main = names(pred3)[120]) # December 2059 Optimistic

# Summarize per year ------------------------------------------------------

summarize_annual_precip <- function(rast, start_year = 2050) {
  n_layers <- nlyr(rast) # get number of layers
  # if (n_layers %% 12 != 0) {
  #   stop("Layer count is not a multiple of 12 (expected monthly data).")
  # }
  
  n_years <- n_layers / 12 # how many years
  
  annual_list <- vector("list", n_years)
  
  for (i in 1:n_years) {
    layer_indices <- ((i - 1) * 12 + 1):(i * 12) # select 12 monthly layers
    year_sum <- sum(rast[[layer_indices]]) # sum up element wise
    names(year_sum) <- paste0("Annual_", start_year + i - 1)
    annual_list[[i]] <- year_sum
  }
  
  annual_stack <- rast(annual_list)
  # annual_stack <- rast(do.call(c, annual_list))
  return(annual_stack)
}

annual_pred1 <- summarize_annual_precip(pred1, start_year = 2050)
annual_pred2 <- summarize_annual_precip(pred2, start_year = 2050)
annual_pred3 <- summarize_annual_precip(pred3, start_year = 2050)

# Quick Check
names(annual_pred1)
monthly_values <- terra::values(pred1)[1, ] # monthly values for single pixel
sum_2050 <- sum(monthly_values[1:12]) # for this pixel: sum across 2050
annual_value <- terra::values(annual_pred1)[1, 1] # comp. with sum from fct (pixel 1, year 2050)

# Map to AOI --------------------------------------------------------------

## Create grid -----------------------------------------------------------

AOI<-read_sf("AOI.shp")
AOI<-AOI%>%st_buffer(dist =0.01 )%>%st_union() #small buffer because countries don't perfectly align and it makes a gap. 

grids<-sf::st_make_grid(AOI, cellsize = 0.25 )
grids <- grids %>% st_as_sf()
#index of grids fully or partially covered by the AOI
intersections_index <- st_intersects(AOI, grids, sparse = F)

# sf object of only the grids we want
grids <- grids[intersections_index, ]
grids$Index<-1:nrow(grids)

ggplot(grids)+geom_sf()+geom_sf(data=AOI,color="green",fill=NA)

grids_vect <- vect(grids)  # terra-compatible spatial object

# Map predicted precipitation onto grid (I think??)
scenarios <- list(
  ssp370 = annual_pred1,
  ssp245 = annual_pred2,
  ssp126 = annual_pred3
)

all_grids_long <- list()

# Loop over each scenario
for (scenario_name in names(scenarios)) {
  
  message("Processing scenario: ", scenario_name)
  
  raster_stack <- scenarios[[scenario_name]]
  
  # Extract mean raster values to each grid cell -- OK???
  extracted <- terra::extract(raster_stack, grids_vect, fun = mean, na.rm = TRUE)
  
  grids_result <- cbind(grids, extracted[,-1])  # drop ID column
  
  # st_write(grids_result, file.path(output_dir, paste0("grids_", scenario_name, ".shp")), delete_layer = TRUE)
  
  # Plot for each year
  years <- 2050:2059
  for (y in years) {
    year_col <- paste0("Annual_", y)
    
    p <- ggplot(grids_result) +
      geom_sf(aes_string(fill = year_col)) +
      scale_fill_viridis_c(name = "Precip (kg/m²/s)") + # CHECK / CONVERT UNIT!!!!!
      labs(
        title = paste("Annual Precipitation -", y),
        subtitle = paste("Scenario:", scenario_name)
      ) +
      theme_minimal()
    
    print(p)
  
    ggsave(
      filename = file.path(output_dir, paste0("map_", scenario_name, "_", y, ".png")),
      plot = p, width = 6, height = 5
    )
  }
  
  # Reshape to long format
  grids_long <- grids_result %>%
    pivot_longer(
      cols = starts_with("Annual_"),
      names_to = "Year",
      names_prefix = "Annual_",
      values_to = "precip"
    ) %>%
    mutate(
      Year = as.integer(Year),
      Scenario = scenario_name
    ) %>%
    select(Index, Year, precip, Scenario, x) 
  # %>%
  #   st_drop_geometry()
  
  # Store in list
  all_grids_long[[scenario_name]] <- grids_long
}

# Combine all into one big data frame
grids_all_long <- bind_rows(all_grids_long)

head(grids_all_long)

# Moran's i / year --------------------------------------------------------

#Get yearly Moran's i. W'll use the median for the stylized baseline. 
Morans<-data.frame(Year=rep(NA,length(years)),Stat=rep(NA,length(years)))

# Test --------------------------------------------------------------------

for(i in 2:length(years)){ # WHY START AT 2 in Pull Summarize Climate Data??
  year<-years[i]
  
  zz<-grids_long%>%filter(Year==year)%>%
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


ggplot(grids_long)+geom_line(aes(x=Year,y=precip,group=Index),alpha=0.1)

library(tseries)



# grids_vect <- vect(grids)
# extracted <- terra::extract(annual_pred1, grids_vect, fun = mean, na.rm = TRUE) # is mean ok??
# 
# years <- 2050:2059
# 
# for (y in years) {
#   year_col <- paste0("Annual_", y)
#   p <- ggplot(grids_result) +
#     geom_sf(aes_string(fill = year_col)) +
#     scale_fill_viridis_c(name = "Precip (kg/m²/s)") +
#     labs(title = paste("Average Precipitation -", y)) +
#     theme_minimal()
#   print(p)
# }


# Archive -----------------------------------------------------------------

# Test --------------------------------------------------------------------

# grids_long <- grids_result %>%
#   pivot_longer(
#     cols = starts_with("Annual_"),
#     names_to = "Year",
#     names_prefix = "Annual_",
#     values_to = "precip"
#   ) %>%
#   mutate(Year = as.integer(Year)) %>%
#   select(c("Index", "Year", "precip")) %>%
#   st_drop_geometry()
# 
# grids_long


## Overview ----------------------------------------------------------------

# Convert AOI to SpatVector
aoi_vect <- vect(AOI)

# Crop/mask to AOI
annual_pred1_cropped <- mask(crop(annual_pred1, aoi_vect), aoi_vect)

plot(annual_pred1_cropped)

## Alternative Visualization -----------------------------------------------

plot_annual_maps <- function(rast, aoi_sf) {
  for (i in 1:nlyr(rast)) {
    year <- names(rast)[i]
    
    # Convert to data.frame for ggplot
    df <- as.data.frame(rast[[i]], xy = TRUE, na.rm = TRUE)
    colnames(df)[3] <- "precip"
    
    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = precip)) +
      geom_sf(data = aoi_sf, fill = NA, color = "black", size = 0.3) +
      coord_sf() +
      scale_fill_viridis_c(name = "kg/m²/s") + # CHECK!!!
      labs(title = paste0(
        sub(".*_", "", deparse(substitute(rast))), ": ", 
        "Annual Precipitation - ", gsub("Annual_", "", year))) 
    
    print(p)
  }
}

plot_annual_maps(annual_pred1, AOI)
plot_annual_maps(annual_pred2, AOI)
plot_annual_maps(annual_pred3, AOI)

