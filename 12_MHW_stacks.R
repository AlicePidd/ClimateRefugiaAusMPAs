# Stack marine heatwave layers
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Metric -----------------------------------------------------------------------
  
  var_nm <- "VoCCMag"

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "VoCC", var_nm, "calc") 
  crop_fol <- make_folder(source_disk, "VoCC", var_nm, "calc_cropped") 
  outfol <- make_folder(source_disk, "VoCC", var_nm, "threat_layers1") # Raster stacks per SSP
  
  
  
# Crop to study region ---------------------------------------------------------
  
  files <- dir(infol, full.names = TRUE)

  crop_to_ext <- function(f) {
    r <- readRDS(f) 
    r <- crop(r, e1) * 10      #** Decadal **
    nm <- basename(f) %>% 
      str_replace(., "rg", "cropped") %>% 
      str_replace(., "Operiod", "decadal")
    saveRDS(r, paste0(crop_fol, "/", nm))
  }
  walk(files, crop_to_ext)

  

# Create rasterstack of threat layers ------------------------------------------
  
  do_threat_stack <- function(pth, mask) {
    dir_files <- dir(pth, full.names = TRUE)
    
    rasters <- lapply(dir_files, function(file) {
      r <- rast(file) 
      voccMag <- r[[1]]
    })
    
    threat_rast <- rast(rasters)
    threat_masked <- terra::mask(threat_rast, mask) 
    threat_stack <- stack(threat_masked)
    
    names(threat_stack) <- basename(dir_files) 
    subset_threatstack <- function(ssp){ 
      r_sub <- raster::subset(threat_stack, grep(ssp, names(threat_stack)))
      nm_order <- map(stack_order, ~grep(.x, names(r_sub))) %>% 
        unlist() 
      r_fin <- r_sub[[nm_order]]
      return(r_fin)
    }
    out <- map(ssp_num, ~ subset_threatstack(.x)) 
    out_df <- as.data.frame(stack(out))
    return(list(rast = out, df = out_df))
  }
  

  
# Do ---------------------------------------------------------------------------

  ## For EEZ, including all MPAs ----
  
  eez_list <- do_threat_stack(crop_fol, reez) 
    saveRDS(eez_list$df, paste0(outfol, "/", var_nm, "_eez_df.RDA"))
    saveRDS(eez_list$rast, paste0(outfol, "/", var_nm, "_eez_stack.RDA")) 
  
    
  ## For MPAs only ----
    
  mpa_list <- do_threat_stack(crop_fol, rmpa)
    saveRDS(mpa_list$df, paste0(outfol, "/", var_nm, "_mpa_df.RDA"))
    saveRDS(mpa_list$rast, paste0(outfol, "/", var_nm, "_mpa_stack.RDA")) 
  
    
  ## For area within the EEZ, but outside of MPAs ----
    
  outmpa_list <- do_threat_stack(crop_fol, routsidempa) 
    saveRDS(outmpa_list$df, paste0(outfol, "/", var_nm, "_outsidempa_df.RDA")) 
    saveRDS(outmpa_list$rast, paste0(outfol, "/", var_nm, "_outsidempa_stack.RDA")) 

  

# Get recent-term data ---------------------------------------------------------

  recent_dat <- dir(crop_fol, full.names = TRUE, pattern = "recent")[1]
  rt <- readRDS(recent_dat) 
  rt <- rt[[1]]
  names(rt) <- "VoCCMag_recent-past"

    
  ## For EEZ, including all MPAs ----
    
    r <- rt %>% 
      mask(., eez)
      saveRDS(r, paste0(outfol, "/", var_nm, "_eez_recent-term_stack.RDA")) 
    recent_df <- as.data.frame(r)
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_eez_recent-term_df.RDA"))
  
      
  ## For MPAs only ----
      
    r <- rt %>% 
      mask(., rmpa) 
      saveRDS(r, paste0(outfol, "/", var_nm, "_mpa_recent-term_stack.RDA"))
    recent_df <- as.data.frame(r) 
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_mpa_recent-term_df.RDA"))
  
      
  ## For area within the EEZ, but outside of MPAs ----
      
    r <- rt %>% 
      mask(., routsidempa) 
      saveRDS(r, paste0(outfol, "/", var_nm, "_outsidempa_recent-term_stack.RDA"))
    recent_df <- as.data.frame(r) 
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_outsidempa_recent-term_df.RDA"))
