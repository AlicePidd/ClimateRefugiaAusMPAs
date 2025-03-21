# Stack rate of change layers, and save in different formats for later scripts
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  var_nm <- tos[1]  
  # var_nm <- ph[1]  
  # var_nm <- o2[1] 
  # var_nm <- mhwROC[1]  
  
  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm, "calc") # ROC calc files for each reporting period
  outfol <- make_folder(source_disk, "ROC", var_nm, "threat_layers") # Raster stacks per SSP
  
  
  
# Create rasterstack of threat layers ------------------------------------------
  
  do_threat_stack <- function(pth, mask) {
    dir_files <- dir(infol, full.names = TRUE)
  
    rasters <- lapply(dir_files, function(file) {
      r <- rast(file) 
    })
    
    threat_rast <- rast(rasters)
    threat_masked <- terra::mask(threat_rast, mask) # In case there are differences
    threat_stack <- stack(threat_masked)
  
    names(threat_stack) <- basename(dir_files) 
    subset_threatstack <- function(ssp){ # Subset the raster stack by each SSP
      r_sub <- raster::subset(threat_stack, grep(ssp, names(threat_stack)))
      nm_order <- map(stack_order, ~grep(.x, names(r_sub))) %>% # Reorder the terms
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
  eez_list <- do_threat_stack(infol, reez) # Saves the data as a list in the working data folder
    saveRDS(eez_list$df, paste0(outfol, "/", var_nm, "_ROC_eez_df.RDA")) # save the df as an RDA file
    saveRDS(eez_list$rast, paste0(outfol, "/", var_nm, "_ROC_eez_stack.RDA")) # save the rasterstack

  ## For MPAs only ----
  mpa_list <- do_threat_stack(infol, rmpa) # Save the data as a list in the working data folder
    saveRDS(mpa_list$df, paste0(outfol, "/", var_nm, "_ROC_mpa_df.RDA")) # save the df as an RDA file
    saveRDS(mpa_list$rast, paste0(outfol, "/", var_nm, "_ROC_mpa_stack.RDA")) # save the rasterstack

  ## For area within the EEZ, but outside of MPAs ----
  outmpa_list <- do_threat_stack(infol, routsidempa) # Save the data as a list in the working data folder
    saveRDS(outmpa_list$df, paste0(outfol, "/", var_nm, "_ROC_outsidempa_df.RDA")) # save the df as an RDA file
    saveRDS(outmpa_list$rast, paste0(outfol, "/", var_nm, "_ROC_outsidempa_stack.RDA")) # save the rasterstack
    
    
    
# Get recent-term data ---------------------------------------------------------

    recent_dat <- dir(infol, full.names = TRUE, pattern = "recent") # Get only recent term data
    
  ## For EEZ, including all MPAs ----
    recent_rast <- stack(recent_dat) # Stack it into a rasterstack
    saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_ROC_recent-term_eez_stack.RDA")) # Save them
    recent_df <- as.data.frame(recent_rast) # Make a df
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_ROC_recent-term_eez_df.RDA"))
  
  ## For MPAs only ----
    recentrast <- stack(recent_dat) %>% # Stack it into a rasterstack
      mask(., MPA_shp) # Mask to MPAs
    saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_ROC_recent-term_mpa_stack.RDA")) # Save them
    recent_df <- as.data.frame(recent_rast) # Make a df
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_ROC_recent-term_mpa_df.RDA"))
  
  ## For area within the EEZ, but outside of MPAs ----
    recent_rast <- stack(recent_dat) %>% # Stack it into a rasterstack
      mask(., outsideMPAs) # Mask to outside MPAs
    saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_ROC_recent-term_outsidempa_stack.RDA")) # Save them
    recent_df <- as.data.frame(recent_rast) # Make a df
      saveRDS(recent_df, paste0(outfol, "/", var_nm, "_ROC_recent-term_outsidempa_df.RDA"))

