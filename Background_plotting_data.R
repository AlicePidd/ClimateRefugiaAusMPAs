# Background data
  # Written by Alice Pidd (alicempidd@gmail.com)
    # June 2023



# Source the helpers and necessary bits -----------------------------------------------------------

  source("Helpers.R")
  disk <- "/Users/alicepidd/Documents/PhD/code/ClimateRefugiaAusMPAs/masks"
  
  
  
# Source data and shapefiles  --------------------------------------------------
  
  aus <- readRDS(paste0(disk, "/mask_landsea.RDS"))
  oceaniaAsia <- readRDS(paste0(disk, "/oceaniaAsia_shapefile.RDS"))
  eez <- readRDS(paste0(disk, "/EEZ_shapefile.RDS"))
  MPA_shp <- readRDS(paste0(disk, "/mpas_numbered_shapefile.RDS"))
  outsideMPA_shp <- readRDS(paste0(disk, "/outsideMPAs_shapefile.RDS"))
  # IUCN_V_VI_shps <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/IUCN-category_V-VI_shapefile.RDS")

  e1 <- ext(105, 175, -50, -5) # Extent
  base_r <- rast(ext = e1, res = 0.25) # Base raster for resampling and cropping
  
  reez <- terra::rasterize(eez, base_r) # Base raster for just MPAs
  reez <- mask(reez, aus)
  
  rmpa <- terra::rasterize(MPA_shp, base_r) # Base raster for all MPAs, including GBR
  routsidempa <- terra::rasterize(outsideMPA_shp, base_r)
  
  
  
# Mask the data to refugia areas only, and find the total area (km2) of the MPAs and EEZ ------------------------------------------
  
  # Area for MPAs
  area_mpa <- terra::cellSize(rmpa, unit = "km")
  area_mpa <- sum(values(area_mpa)[!is.na(values(rmpa))]) # Sum the values, without including NAs. Should be 1872959 km^2 (old area 2303489 km^2)
  
  # Area for the EEZ
  area_eez <- terra::cellSize(reez, unit = "km")
  area_eez <- sum(values(area_eez)[!is.na(values(reez))]) # Sum the values, without including NAs. Should be 6913648 km^2 (old area 1872959 km^2)
  
  # Area for outside of MPAs
  area_outsidempa <- terra::cellSize(routsidempa, unit = "km") # Gives a spatraster
  area_outsidempa <- sum(values(area_outsidempa)[!is.na(values(routsidempa))]) # Sum the values, without including NAs. Should be 5428299 km^2 (old area 4901916 km^2)

