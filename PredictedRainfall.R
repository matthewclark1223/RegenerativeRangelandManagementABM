
# Load packages -----------------------------------------------------------

# library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(spdep)
library(tidyterra)

# Create output folder if it doesn't exist
output_dir <- "annual_pred"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Load Data ---------------------------------------------------------------
# General order of assigning: bad, ok, good (for easy overview in tables)
# check assignment !

# Download with slightly higher ymax??!!!

# IPSL-CM6A-LR (France) with 2050-2059
# 12 months x 30 years = 360 layers per dataset
# baseline <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_19610116-19901216.nc") # 1961 - 1990
# 12 months x 10 years = 120 layers per dataset
# pred1 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp370_r1i1p1f1_gr_20500116-20591216.nc")  # Worst Case (SSP3-7.0)
# pred2 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_20500116-20591216.nc")  # Current Policy (SSP2-4.5)
# pred3 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_20500116-20591216.nc")  # Optimistic (SSP1-2.6)

# IPSL-CM6A-LR (France) with 2050-2079
# baseline <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_19610116-19901216.nc") # 1961 - 1990
# pred1 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp370_r1i1p1f1_gr_20500116-20791216.nc")  # Worst Case (SSP3-7.0) 2050 - 2079!
# pred2 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_20500116-20791216.nc")  # Current Policy (SSP2-4.5) 2050 - 2079!
# pred3 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_20500116-20791216.nc")  # Optimistic (SSP1-2.6) 2050 - 2079!
# pred4 <- terra::rast("Data/pr_Amon_IPSL-CM6A-LR_ssp585_r1i1p1f1_gr_20500116-20791216.nc")  # Dytopian (SSP5-8.5) 2050 - 2079!

# MPI-ESM1-2-LR (Germany) with 2050-2059
baseline <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_18500116-19001216.nc")   # 1850 - 1900
pred1 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp585_r1i1p1f1_gn_20500116-20591216.nc")  # Dystopian (SSP5-8.5)
pred2 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp245_r1i1p1f1_gn_20500116-20591216.nc")  # Current Policy (SSP2-4.5)
pred3 <- terra::rast("Data/MPI-2050to2059/pr_Amon_MPI-ESM1-2-LR_ssp126_r1i1p1f1_gn_20500116-20591216.nc")  # Optimistic (SSP1-2.6)

# Quick check
plot(baseline[[1]], main = names(baseline)[1]) # January 1850
plot(pred1[[1]], main = names(pred1)[1]) # January 2050 Worst Case
plot(pred2[[1]], main = names(pred2)[1]) # January 2050 Current Policy
plot(pred3[[1]], main = names(pred3)[1]) # January 2050 Optimistic

plot(baseline[[1]], main = names(baseline)[1])
plot(pred1[[120]], main = names(pred1)[120]) # December 2059 Worst Case
plot(pred2[[120]], main = names(pred2)[120]) # December 2059 Current Policy
plot(pred3[[120]], main = names(pred3)[120]) # December 2059 Optimistic

# Summarize per year ------------------------------------------------------

summarize_annual_precip <- function(rast, start_year) {
  n_layers <- nlyr(rast) # get number of layers
  n_years <- n_layers / 12 # how many years
  
  annual_list <- vector("list", n_years)
  
  for (i in 1:n_years) {
    layer_indices <- ((i - 1) * 12 + 1):(i * 12) # select 12 monthly layers
    year_sum <- sum(rast[[layer_indices]]) # sum up element wise
    names(year_sum) <- paste0("Annual_", start_year + i - 1)
    annual_list[[i]] <- year_sum
  }
  
  annual_stack <- rast(annual_list)
  return(annual_stack)
}

annual_baseline <- summarize_annual_precip(baseline, start_year = 1961)
annual_pred1 <- summarize_annual_precip(pred1, start_year = 2050)
annual_pred2 <- summarize_annual_precip(pred2, start_year = 2050)
annual_pred3 <- summarize_annual_precip(pred3, start_year = 2050)
# annual_pred4 <- summarize_annual_precip(pred4, start_year = 2050)

# Quick check baseline
names(annual_baseline)
monthly_values <- terra::values(baseline)[1, ] # monthly values for single pixel
sum_1961 <- sum(monthly_values[1:12]) # for this pixel: sum across mos of 1961
annual_value <- terra::values(annual_baseline)[1, 1] # comp. with sum from fct (pixel 1, year 2050)
sum_1961 == annual_value

# Quick check pred1
names(annual_pred1)
monthly_values <- terra::values(pred1)[1, ] # monthly values for single pixel
sum_2050 <- sum(monthly_values[1:12]) # for this pixel: sum across mos of 2050
annual_value <- terra::values(annual_pred1)[1, 1] # comp. with sum from fct (pixel 1, year 2050)
sum_2050 == annual_value

# Map to AOI --------------------------------------------------------------

## Create grid -----------------------------------------------------------

AOI <-read_sf("AOI.shp")
AOI <- st_transform(AOI) %>%
               # , crs = crs(raster_stack)
  st_buffer(dist = 0.05) %>% # small buffer because countries don't perfectly 
  # align and it makes a gap
  st_union() 

grids<-sf::st_make_grid(AOI, cellsize = 0.25) %>%
  st_as_sf() %>%
  filter(st_intersects(AOI, ., sparse = FALSE)[1, ]) %>%   #index of grids fully 
  # or partially covered by the AOI
  mutate(Index = row_number())

# Transform grid into the CRS of the grid again as a precaution
grids <- st_transform(grids
                      # , crs = crs(raster_stack)
                      ) 

ggplot(grids) + 
  geom_sf() + 
  geom_sf(data = AOI,color = "green",fill = NA)

# terra-compatible raster with target resolution
grids_vect <- vect(grids)
target_raster <- terra::rast(
  grids_vect, resolution = 0.25
  # , crs = crs(raster_stack)
  )

## Map predicted precipitation onto grid -----------------------------------

scenarios <- list(
  baseline = annual_baseline,
  'bad' = annual_pred1,
  'ok' = annual_pred2,
  'good' = annual_pred3
)

# Calculate global min and max values for consistent legend in plots across years and scenarios
global_min <- min(unlist(lapply(scenarios, function(s) terra::minmax(s)[1, ])), na.rm = TRUE)
global_max <- max(unlist(lapply(scenarios, function(s) terra::minmax(s)[2, ])), na.rm = TRUE)

all_grids_long <- list()

for (scenario_name in names(scenarios)) {
  
  message("Processing scenario: ", scenario_name)
  
  raster_stack <- scenarios[[scenario_name]]
  
  # Interpolate each layer to target grid resolution
  raster_stack_interp <- terra::resample(
    raster_stack, target_raster, method = "bilinear")
  
  # Crop/mask to grids
  raster_stack_cut <- terra::crop(raster_stack_interp, vect(AOI))
  raster_stack_cut <- terra::mask(raster_stack_cut, vect(AOI))
  
  # Extract year values from layer names
  years <- names(raster_stack_cut)
  years <- years[grepl("^Annual_\\d{4}$", years)]  # make sure to keep only annual layers
  years <- as.integer(sub("Annual_", "", years))   # extract the year number
  
  for (y in years) {
    layer_name <- paste0("Annual_", y)
    
    r_cut <- raster_stack_cut[[layer_name]]
    
    # Convert grids_vect to sf for plotting just the outline
    grids_outline <- sf::st_union(sf::st_as_sf(grids_vect))
    
    # Plot raster with outline only
    p <- ggplot() +
      geom_spatraster(data = r_cut) +
      geom_sf(data = grids_outline, fill = NA, color = "black", linewidth = 0.5) +
      scale_fill_viridis_c(
        name = "Precip (kg/m²/s)", # adapt unit?
        na.value = NA,
        limits = c(global_min, global_max)  # for consistent fill across all years and scenarios
      ) +
      # scale_fill_viridis_c(name = "Precip (kg/m²/s)", na.value = NA) + # adapt unit?
      labs(
        title = paste("Annual Precipitation -", y),
        subtitle = paste("Scenario:", scenario_name)
      )
    
    print(p)
    
    ggsave(
      filename = file.path(output_dir, paste0("map_", scenario_name, "_", y, ".png")),
      plot = p, width = 6, height = 5
    )
  }
  
  raster_df <- as.data.frame(raster_stack_cut, xy = TRUE, na.rm = TRUE)
  
  raster_long <- raster_df %>%
    pivot_longer(
      cols = starts_with("Annual_"),
      names_to = "Year",
      names_prefix = "Annual_",
      values_to = "precip"
    ) %>%
    mutate(
      Year = as.integer(Year),
      Scenario = scenario_name
    )
  
  all_grids_long[[scenario_name]] <- raster_long
}

# Combine all into one big data frame
grids_long_combined <- bind_rows(all_grids_long)

head(grids_long_combined)

# Summary Stats -----------------------------------------------------------

# other ideas: 
# not use precipitation but other indices for grass growth?

## Pixel level -------------------------------------------------------------

scenarios <- c("bad", "ok", "good")

# Baseline / historic summary per pixel
baseline_df <- grids_long_combined %>%
  filter(Scenario == "baseline") %>%
  group_by(x, y) %>%
  summarize(
    mean_bl = mean(precip, na.rm = TRUE),
    sd_bl = sd(precip, na.rm = TRUE),
    .groups = "drop"
  )

# Function to compare each future scenario to baseline
compare_to_baseline <- function(scn) {
  scenario_df <- grids_long_combined %>%
    filter(Scenario == scn) %>%
    group_by(x, y) %>%
    summarize(
      mean_scn = mean(precip, na.rm = TRUE),
      sd_scn = sd(precip, na.rm = TRUE),
      .groups = "drop"
    )
  
# Join with baseline
  diff_df <- scenario_df %>%
    left_join(baseline_df, by = c("x", "y")) %>%
    mutate(
      mean_diff = mean_scn - mean_bl,
      sd_diff = sd_scn - sd_bl
    )
  
# Aggregate across all pixels to get a single summary value
  tibble(
    Scenario = scn,
    mean_diff_avg = mean((diff_df$mean_diff), na.rm = TRUE),
    sd_diff_avg = mean((diff_df$sd_diff), na.rm = TRUE)
  )
}

# Run for all scenarios and bind results
summary_table <- map_dfr(scenarios, compare_to_baseline)

summary_table <- summary_table %>%
  rename(
    `Mean Difference (Scenario - Baseline)` = mean_diff_avg,
    `SD Difference (Scenario - Baseline)` = sd_diff_avg
  )

summary_table

# Moran's i ---------------------------------------------------------------

grids_sf <- sf::st_as_sf(grids_vect)

points_sf <- grids_long_combined %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(grids_sf))

grids_with_precip <- st_join(points_sf, grids_sf, left = FALSE)

# Prep result table
years <- sort(unique(grids_with_precip$Year))
scenarios <- unique(grids_with_precip$Scenario)
moran_results <- expand.grid(Year = years, Scenario = scenarios, Moran_I = NA_real_)

# Loop through year/scenario
for (i in seq_len(nrow(moran_results))) {
  yr <- moran_results$Year[i]
  scn <- moran_results$Scenario[i]
  
  # Subset data
  subset_vals <- grids_with_precip %>%
    filter(Year == yr, Scenario == scn, !is.na(precip)) %>%
    st_drop_geometry() %>%
    group_by(Index) %>%
    summarize(precip = mean(precip), .groups = "drop")
  
  # Join data to grid geometry
  subset_sf <- grids_sf %>%
    inner_join(subset_vals, by = "Index")
  subset_sf <- st_as_sf(subset_sf)
  
  if (nrow(subset_sf) < 10) next
  
  # Create neighborhood structure
  nb <- tryCatch(poly2nb(subset_sf, queen = TRUE), error = function(e) NULL)
  if (is.null(nb) || all(card(nb) == 0)) next
  
  connected <- which(card(nb) > 0)
  subset_sf <- subset_sf[connected, ]
  nb <- poly2nb(subset_sf, queen = TRUE)
  
  # Spatial weights
  lw <- nb2listw(nb, style = "W")
  
  # Moran's I
  moran_test <- moran.test(subset_sf$precip, lw, alternative = "greater")
  moran_results$Moran_I[i] <- moran_test$estimate[["Moran I statistic"]]
}

moran_table <- moran_results %>%
  drop_na(Moran_I) %>%
  group_by(Scenario, Year) %>%
  summarize(mean_moran = mean(Moran_I), .groups = "drop")

ggplot(moran_table, aes(x = mean_moran)) +
  geom_histogram(binwidth = 0.001, fill = "steelblue", color = "white") +
  facet_wrap(~ Scenario, scales = "free_y") +
  labs(
    title = "Distribution of Mean Moran's I by Scenario",
    x = "Mean Moran's I",
    y = "Frequency"
  )

