# Background data
  # Written by Alice Pidd (alicempidd@gmail.com)
    # June 2023



# Source the helpers and necessary bits -----------------------------------------------------------

  source("Helpers.R")
  disk <- "/Users/alicepidd/Documents/GitHub/ClimateRefugiaAusMPAs/masks"
  

  
# Source data and shapefiles  --------------------------------------------------
  
  aus <- readRDS(paste0(disk, "/mask_landsea.RDS"))
  oceaniaAsia <- readRDS(paste0(disk, "/oceaniaAsia_shapefile.RDS"))
  eez <- readRDS(paste0(disk, "/EEZ_shapefile.RDS"))
  MPA_shp <- readRDS(paste0(disk, "/mpas_numbered_shapefile.RDS"))
  outsideMPAs <- readRDS(paste0(disk, "/outsideMPAs_shapefile.RDS"))
  # IUCN_V_VI_shps <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/IUCN-category_V-VI_shapefile.RDS")

  e1 <- ext(105, 175, -50, -5) # Extent
  base_r <- rast(ext = e1, res = 0.25) # Base raster for resampling and cropping
  
  reez <- terra::rasterize(eez, base_r) # Base raster for just MPAs
  reez <- mask(reez, aus)
  rmpa <- terra::rasterize(MPA_shp, base_r) # Base raster for all MPAs, including GBR
  routsidempa <- terra::rasterize(outsideMPAs, base_r)
  
