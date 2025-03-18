# Create land-sea masks
  # Code written by Alice Pidd
    # November 2023



# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")

  
# Folders ----------------------------------------------------------------------
  
  mask_fol <- make_folder(disk, "masks", "", "") 
  
  
  
# Bakcground data --------------------------------------------------
  
  e1 <- ext(105, 175, -50, -5) 
  base_r <- rast(ext = e1, res = 0.25) # Base raster for resampling and cropping
  # values(base_r) <- 1
  
  
# Crop world shp to Australia ----
  
  data("World")
  oceaniaAsia <- World %>%
    filter(continent %in% c("Oceania", "Asia")) %>%
    dplyr::select(area)
  bbox_e1 <- st_as_sfc(st_bbox(c(xmin = 105, xmax = 175, ymin = -50, ymax = -5), 
                               crs = st_crs(oceaniaAsia)))
  oceaniaAsia <- st_crop(oceaniaAsia, bbox_e1)
  saveRDS(oceaniaAsia, paste0(mask_fol, "/oceaniaAsia_shapefile.RDS"))
  # plot(oceaniaAsia)
  
  # oceaniaAsiaAnt <- World %>%
  #   filter(continent %in% c("Oceania", "Asia", "Antarctica")) %>%
  #   dplyr::select(area)
  # # plot(oceaniaAsiaAnt)
  
  australia <- World %>%
    filter(name %in% c("Australia")) %>%
    dplyr::select(area)
  bbox_e1 <- st_as_sfc(st_bbox(c(xmin = 105, xmax = 175, ymin = -50, ymax = -5), 
                               crs = st_crs(australia)))
  australia <- st_crop(australia, bbox_e1)
  saveRDS(australia, paste0(mask_fol, "/australia_shapefile.RDS"))
  # plot(australia)
  
  
  
  
# # Make the land-sea mask (Jase's method) ---------------------------------------
#     
#     land <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% # Create land file
#       dplyr::mutate(value = 1) %>%
#       dplyr::select(value) %>% 
#       terra::vect() # Turn it into spatvector
#     
#     mask <- terra::rast(ncol = 360*4, nrow = 180*4) # Create mask spatraster
#     # Can change the res by increasing/decreasing the multiplication of ncol and nrow. I stuck with 0.25 degree  
#     b_rast <- rast(paste0(mask_folder, "/base_rastermaskEXT.nc")) # Get the base raster file
#     
# 
#     ## Make a NAs/1 landmask ----
#     r <- terra::rasterize(land, mask, field = "value", cover = TRUE) %>% # Get them on the same page
#       terra::crop(., base_r) # Crop it to the base raster ext
#     plot(r)
#     
#     r[is.na(r)] <- 0 # Set NA values to 1 (ocean)
#     r[r > 0 ] <- NA # Set values between 0-1 as 0 (land)
#     r[r == 0 ] <- 1 # Set values between 0-1 as 0 (land)
#     
#     # r[r >= 0 & !is.na(r)] <- 0 # Set values between 0-1 as 0 (land)
#     # r[r <= 1 & r > 0] <- 1 # Set values between 0-1 as 0 (land)
#     # r[is.na(r)] <- 0 # Set NA values to 1 (ocean)
#     # r[r == 0] <- NA # Trying this: Subsequently setting the 0's to NAs (land) so that we can deal with negative rates of change (cooling) ## This doesn't work, just interpolates for all of Australia again.
#     plot(r)

  