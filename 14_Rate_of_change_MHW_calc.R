# Calculate rate of change metric for marine heatwaves
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" 
  dest_disk <- "/Volumes/AliceShield/clim_data" 
  

  
# Variable name ----------------------------------------------------------------
  
  var_nm <- mhwROC[1]
  
  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "MHW", "MHW-CumInt", "2_MHW_CumInt_IPCCsplits1") 
  outfol <- make_folder(source_disk, "MHW", var_nm, "calc1") 

  
  
# Calculate decadal rates of change --------------------------------------------
  
  get_trend <- function(f) {
    
    ofile1 <- basename(f) %>% 
      str_replace("ensemble", "ens-intercept") %>% 
      paste0(outfol, "/", .)

    ofile2 <- basename(f) %>% 
      str_replace("CumInt", "MHW_ROC-decadal") %>% 
      str_replace("ensemble", "ens-trend") %>% 
      str_remove("Oyear_") %>% 
      paste0(outfol, "/", .)
    
    cdo_code <- paste0("cdo -L -trend -mulc,10 ", f, " ", ofile1, " ", ofile2) # Decadal
    system(cdo_code) 
    
    remove_intercepts <- paste0("rm ", ofile1) # Don't need the intercept (created by "cdo trend")
    system(remove_intercepts)
  }

  files <- dir(infol, full.names = TRUE)
  files
  
  plan(multisession, workers = 10)
  walk(files, get_trend)
  plan(sequential)
  
