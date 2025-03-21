# Stack climate velocity layers
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
  


# Folders ----------------------------------------------------------------------
  gVoCC_folder <- make_CMIP_folder("gVoCC_folder", "13_VoCC_gVoCC", source_disk, metric, met_ttl)
  data_folder <- make_CMIP_folder("croppedVoCC_folder", "13_VoCC_gVoCCcropped", source_disk, metric, met_ttl)
  workingdata_folder <- make_CMIP_folder("threatlayer_folder", "threat_layers", source_disk, metric, met_ttl)
  
  
# Crop to study region ---------------------------------------------------------
  
  files <- dir(gVoCC_folder, full.names = TRUE)
  f <- files[1]

    crop_to_ext <- function(f) {
      e <- ext(e1) # Specify extent to crop to
      r <- readRDS(f) 
      r <- crop(r, e) * 10      #** Decadal **
      nm <- basename(f) %>% 
        str_replace(., "rg", "cropped")
    
      saveRDS(r, paste0(data_folder, "/", nm))
    }

    walk(files, crop_to_ext)


# Create rasterstack of threat layers ------------------------------------------
  # Create the threat stacks -----
  do_threat_stack <- function(dir_path, base_rastermask) {
    dir_files <- dir(data_folder, full.names = TRUE)
    
    process_raster <- function(f) {
      r <- readRDS(f) 
      voccMag <- r[[1]] # Get the first layer
      voccMag_rast <- as(voccMag, "SpatRaster") # Convert to SpatRaster
      resampled_rast <- terra::resample(voccMag, base_rastermask)  # Resample to match base_rastermask
      return(resampled_rast)
    }

    resampled_rasters <- lapply(dir_files, process_raster) # Do the resample function to all files
    threat_rast <- rast(resampled_rasters) # Make them spatrasters again
    threat_stack <- terra::mask(threat_rast, base_rastermask) # Mask them to whatever the mask is we specify
    
    names(threat_stack) <- basename(dir_files) # Keep filenames or they'll be "layer 1, layer 2, ..."
    subset_threatstack <- function(ssp){ # Subset the raster stack by each SSP.
      r_sub <- raster::subset(threat_stack, grep(ssp, names(threat_stack)))
      nm_order <- map(stack_order, ~grep(.x, names(r_sub))) %>% # Reorder the terms per the stack_order
        unlist() 
      r_fin <- r_sub[[nm_order]]
      return(r_fin)
    }
    out <- map(ssp_num, ~ subset_threatstack(.x)) # Do the function to each subset per each ssp_num
    out_stack <- rast(out)  # Convert the list of rasters to a single SpatRaster stack
    out_df <- terra::as.data.frame(out_stack, xy = TRUE, long = TRUE)
    
    return(list(rast = out_stack, df = out_df))
  }


# Do the thing:-----------------------------------------------------------------
  ## For MPAs ----
  outmpalist <- do_threat_stack(workingdata_folder, rmpa) # Save the data as a list in the working data folder
  #   # For IPCC cut ens-trends
    saveRDS(outmpalist$df, paste0(workingdata_folder, "/", met_ttl, "_", metric, "_METRICmpa", "_df.RDA")) # save the df as an RDA file
    saveRDS(outmpalist$rast, paste0(workingdata_folder, "/", met_ttl, "_", metric, "_METRICmpa", "_stack.RDA")) # save the rasterstack

  ## For entire EEZ ----
  outeez_list <- do_threat_stack(workingdata_folder, reez) # Saves the data as a list in the working data folder
  #   # For IPCC cut ens-trends
    saveRDS(outeez_list$df, paste0(workingdata_folder, "/", met_ttl, "_", metric, "_METRICeez", "_df.RDA")) # save the df as an RDA file
    saveRDS(outeez_list$rast, paste0(workingdata_folder, "/", met_ttl, "_", metric, "_METRICeez", "_stack.RDA")) # save the rasterstack

  ## For outside of MPAs ----
  outmpalist <- do_threat_stack(workingdata_folder, routsidempa) # Save the data as a list in the working data folder
  #   # For IPCC cut ens-trends
    saveRDS(outmpalist$df, paste0(workingdata_folder, "/", met_ttl, "_", metric, "_METRICmpaoutside", "_df.RDA")) # save the df as an RDA file
    saveRDS(outmpalist$rast, paste0(workingdata_folder, "/", met_ttl, "_", metric, "_METRICmpaoutside", "_stack.RDA")) # save the rasterstack
  