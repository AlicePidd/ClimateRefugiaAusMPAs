# Calculate rate of change metric
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  

  
# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  var_nm <- tos[1]  
  # var_nm <- ph[1]  
  # var_nm <- o2[1] 
  # var_nm <- mhwROC[1]  
  
  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "", var_nm, "8_ensembles_yearmean") # Ensemble decadal trend files
  ipcc_fol <- make_folder(source_disk, "", var_nm, "9_IPCC_splits") # Ens trend files, split into IPCC reporting periods
  outfol <- make_folder(source_disk, "ROC", var_nm, "calc1") # ROC calc files for each reporting period

  
  
# Split ensembles into IPCC reporting periods ----------------------------------
  
  files <- dir(infol, full.names = TRUE)
  files
  
  plan(multisession, workers = 5)
  do_IPCC_cut <- function(ensemble, term) {
    cut_IPCC_terms(ensemble, term = term, v = var_nm, pth = ipcc_fol)
  }
  
  walk(IPCC_terms, ~ future_walk(files, do_IPCC_cut, term = .x))
  plan(sequential)
  
  
  
# Calculate rates of change ----------------------------------------------------
  
  get_trend <- function(f) { 
    ofile1 <- f %>%
      str_replace(ipcc_fol, outfol) %>% 
      str_replace("ensemble" ,"ens-intercept") 
    
    ofile2 <- f %>%
      str_replace(ipcc_fol, outfol) %>% 
      str_replace("ensemble" ,"ens-trend") %>% 
      str_replace(paste0(get_CMIP6_bits(f)[2]), "ROC-decadal")
    
    #**From daily to decadal**
      cdo_code <- paste0("cdo -L -trend -mulc,3650 ", f, " ", ofile1, " ", ofile2) 
    
    #**From monthly to decadal**
      # cdo_code <- paste0("cdo -L -trend -mulc,120 ", f, " ", ofile1, " ", ofile2)
    
    system(cdo_code)
    
    remove_intercepts <- paste0("rm ", ofile1) # Remove interim intercept file
    system(remove_intercepts) 
  }
  
  files <- dir(ipcc_fol, full.names = TRUE, pattern = "126")
  files
  
  plan(multisession, workers = 10)
  future_walk(files, get_trend)
  plan(sequential)

  