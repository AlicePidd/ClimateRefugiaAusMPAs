# Stack rate of change in marine heatwave cumulative intensity layers
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Variable name ----------------------------------------------------------------
  
  var_nm <- mhwROC[1]
  
  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "MHW", var_nm, "calc1") 
  outfol <- make_folder(source_disk, "MHW", var_nm, "threat_layers1") 
  
  

# Create rasterstack of threat layers ------------------------------------------
  
  do_threat_stack <- function(pth, mask) {
    dir_files <- dir(pth, full.names = TRUE)
  
    rasters <- lapply(dir_files, function(file) {
      r <- rast(file) 
    })
    
    threat_rast <- rast(rasters)
    threat_masked <- terra::mask(threat_rast, mask) # In case there are differences
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
  
  eez_list <- do_threat_stack(infol, reez) 
    saveRDS(eez_list$df, paste0(outfol, "/", var_nm, "_eez_df.RDA")) 
    saveRDS(eez_list$rast, paste0(outfol, "/", var_nm, "_eez_stack.RDA")) 

    
  ## For MPAs only ----
    
  mpa_list <- do_threat_stack(infol, rmpa) 
    saveRDS(mpa_list$df, paste0(outfol, "/", var_nm, "_mpa_df.RDA")) 
    saveRDS(mpa_list$rast, paste0(outfol, "/", var_nm, "_mpa_stack.RDA")) 

    
  ## For area within the EEZ, but outside of MPAs ----
    
  outmpa_list <- do_threat_stack(infol, routsidempa) 
    saveRDS(outmpa_list$df, paste0(outfol, "/", var_nm, "_outsidempa_df.RDA")) 
    saveRDS(outmpa_list$rast, paste0(outfol, "/", var_nm, "_outsidempa_stack.RDA")) 
    
    
    
# Get recent-term data ---------------------------------------------------------

    recent_dat <- dir(infol, full.names = TRUE, pattern = "recent") # Get only recent term data
    
    
  ## For EEZ, including all MPAs ----
    
    recent_rast <- stack(recent_dat) %>% 
      mask(., eez)
    names(recent_rast) <- basename(recent_dat)
      saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_eez_recent-term_stack.RDA")) 
    recent_df <- as.data.frame(recent_rast) 
    names(recent_df) <- basename(recent_dat)
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_eez_recent-term_df.RDA"))
  
      
  ## For MPAs only ----
      
    recent_rast <- stack(recent_dat) %>% 
      mask(., MPA_shp) 
    names(recent_rast) <- basename(recent_dat)
      saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_mpa_recent-term_stack.RDA")) 
    recent_df <- as.data.frame(recent_rast) 
    names(recent_df) <- basename(recent_dat)
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_mpa_recent-term_df.RDA"))
  
      
  ## For area within the EEZ, but outside of MPAs ----
      
    recent_rast <- stack(recent_dat) %>% 
      mask(., outsideMPA_shp) 
    names(recent_rast) <- basename(recent_dat)
      saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_outsidempa_recent-term_stack.RDA")) 
    recent_df <- as.data.frame(recent_rast) 
    names(recent_df) <- basename(recent_dat)
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_outsidempa_recent-term_df.RDA"))
