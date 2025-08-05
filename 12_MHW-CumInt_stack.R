# Stack marine heatwave (cumulaive intensity) layers
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Metric -----------------------------------------------------------------------
  
  var_nm <- mhw[1]

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "MHW", var_nm, "1_MHW_CumInt_ens1")
  ipcc_fol <- make_folder(source_disk, "MHW", var_nm, "2_MHW_CumInt_IPCCsplits1")
  summed_fol <- make_folder(source_disk, "MHW", var_nm, "3_MHW_CumInt_summed_period1")
  # crop_fol <- make_folder(source_disk, "MHW", var_nm, "calc_cropped")
  outfol <- make_folder(source_disk, "MHW", var_nm, "threat_layers1") # Raster stacks per SSP
  
  # calc_fol <- make_folder(source_disk, "MHW", var_nm, "calc")
  
  
  
# Crop to study region ---------------------------------------------------------
  
  files <- dir(infol, full.names = TRUE)
  files
  f <- files[1]
  
  crop_to_ext <- function(f) {
    r <- rast(f) 
    r <- crop(r, e1)
    nm <- basename(f) %>% 
      str_replace(., "r1i1p1f1", "cropped")
    saveRDS(r, paste0(infol, "/", nm))
  }
  plan(multisession, workers = 10)
  walk(files, crop_to_ext)
  plan(sequential)
  
  
  
# Calculate yearly MHW-CumInt from daily files ---------------------------------
  
  get_yearly <- function(f, start_date, end_date, outfol) {
    o_nm <- str_replace(basename(f), "Oday", "Oyear")
    
    cdo_script <- paste0("cdo -s -L -f nc4 ", 
                         "-yearsum ",  # Get yearly cumint from daily
                         "-selvar,tos ", 
                         f, " ", infol, "/", o_nm) # Output path and file name
    system(cdo_script)
  }
  files <- dir(infol, full.names = TRUE, pattern = "cropped")
  files

  plan(multisession, workers = 10)
  walk(files, get_yearly)
  plan(sequential)
  
  
  
# Split ensembles into IPCC reporting periods ----------------------------------
  
  do_IPCC_cut <- function(ensemble, term) {
    cut_IPCC_terms(ensemble, term = term, v = "tos", pth = ipcc_fol)
  }
  
  files <- dir(infol, full.names = TRUE, pattern = "Oyear")
  files
  
  plan(multisession, workers = 5)
  walk(IPCC_terms, ~ future_walk(files, do_IPCC_cut, term = .x))
  plan(sequential)
  
  
  
# Get total cumulative intensity for each reporting period ---------------------

  sum_period <- function(f) {
    term <- str_split_i(basename(files[1]), "_", 6)
    sum_nm <- basename(f) %>% 
      gsub("r1i1p1f1", "summed", .) %>% 
      gsub(".nc", ".RDS", .) %>%
      paste0(summed_fol, "/", .) 
    
    r <- rast(f) %>% 
      sum() %>% 
      saveRDS(., sum_nm)
  }
  files <- dir(ipcc_fol, full.names = TRUE)
  files
  walk(files, sum_period)
  
  
  
# Create rasterstack of threat layers ------------------------------------------
  
  do_threat_stack <- function(pth, mask) {
    dir_files <- dir(summed_fol, full.names = TRUE)
    
    rasters <- lapply(dir_files, function(file) {
      r <- readRDS(file)
    })
    
    rasters <- rast(rasters)
    threat_masked <- terra::mask(rasters, mask) 
    names(threat_masked) <- basename(dir_files) 
    threat_stack <- stack(threat_masked)
    
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
  
  eez_list <- do_threat_stack(summed_fol, reez)
  saveRDS(eez_list$df, paste0(outfol, "/", var_nm, "_eez_df.RDA"))
  saveRDS(eez_list$rast, paste0(outfol, "/", var_nm, "_eez_stack.RDA")) 
  
  
  ## For MPAs only ----
  
  mpa_list <- do_threat_stack(ipcc_fol, rmpa) 
  saveRDS(mpa_list$df, paste0(outfol, "/", var_nm, "_mpa_df.RDA")) 
  saveRDS(mpa_list$rast, paste0(outfol, "/", var_nm, "_mpa_stack.RDA")) 
  
  
  ## For area within the EEZ, but outside of MPAs ----
  
  outmpa_list <- do_threat_stack(ipcc_fol, routsidempa) 
  saveRDS(outmpa_list$df, paste0(outfol, "/", var_nm, "_outsidempa_df.RDA")) 
  saveRDS(outmpa_list$rast, paste0(outfol, "/", var_nm, "_outsidempa_stack.RDA")) 
  
  
  
# Get recent-term data ---------------------------------------------------------
  
  recent_dat <- dir(summed_fol, full.names = TRUE, pattern = "recent") 
  
  
  ## For EEZ, including all MPAs ----
  
    recent_rast <- stack(recent_dat) 
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
    saveRDS(recent_rast, paste0(outfol, "/", var_nm, "_outsidempa_recent-term_stack.RDA")) 
    recent_df <- as.data.frame(recent_rast) 
    saveRDS(recent_df, paste0(outfol, "/", var_nm, "_outsidempa_recent-term_df.RDA"))
    
    