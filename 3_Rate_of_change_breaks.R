# Compute rate of change breaks for the threat layers, and for climate refugia
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  
  

# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  var_nm <- tos[1]  
  # var_nm <- ph[1]  
  # var_nm <- o2[1] 
  # var_nm <- mhwROC[1]  
  
  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm, "threat_layers") # Raster stacks per SSP

  

# Create breaks for each threat ------------------------------------------------
  
  percentiles <- seq(0.1, 0.9, 0.025) # Breaks
  
  get_brks <- function(df) { 
    output <- pivot_longer(df, everything(), 
                           names_to = "scenario", values_to = var_nm) %>%  
      na.omit() %>% 
      dplyr::select(all_of(var_nm)) %>% 
      as.vector() %>% 
      unlist() %>% 
      unname() %>% 
      quantile(., percentiles) %>% 
      c(-Inf, ., Inf) 
    brks <- output
    return(brks)
  }

  
  ## For entire EEZ ----
    eez_df <- readRDS(paste0(infol, "/", var_nm, "_ROC_eez_df.RDA")) # Get data for all IPCC terms
    brks_eez <- get_brks(eez_df) # Get the breaks
    saveRDS(brks_eez, paste0(infol, "/", var_nm, "_ROC_eez_breaks.RDA"))

  ## For MPAs ----
    mpa_df <- readRDS(paste0(infol, "/", var_nm, "_ROC_mpa_df.RDA")) # Get data for all IPCC terms
    brks_mpa <- get_brks(mpa_df) # Get the breaks
    saveRDS(brks_mpa, paste0(infol, "/", var_nm, "_ROC_mpa_breaks.RDA"))

  ## For outside MPAs ----
    outeez_df <- readRDS(paste0(infol, "/", var_nm, "_ROC_outsidempa_df.RDA")) # Get data for all IPCC terms
    brks_outeez <- get_brks(outeez_df) # Get the breaks
    saveRDS(brks_outeez, paste0(infol, "/", var_nm, "_ROC_outsidempa_breaks.RDA"))


    
# Create "refugial" breaks -----------------------------------------------------
    
  # Classifies the threat layer data as either "refugial" or "non-refugial" based on the percentile set as the break/threshold - here, the first 30% (0.3) of change of the recent-term data
    
  get_recentterm_brks <- function(df, ref_per) { # Split the df at the specified refugia percentage
    dflist <- as.list(df)
    
    brks <- map(dflist, ~ {
      na.omit(.x) %>% 
        as.vector() %>% 
        unlist() %>% 
        unname() %>% 
        
        ##** CHOOSE: *
        quantile(., ref_per) %>%   # for tos
        # quantile(., 1-(1-ref_per)) %>%  # for pH and o2 where non-refugia occur in a negative direction (decreasing)

        c(-Inf, ., Inf) 
    })
    
    return(brks)
  }
  
  per <- 0.3    #**Percentage of the present day data we want to classify as refugia**

  
  ## For entire EEZ ----
  recent_dfeez <- readRDS(paste0(infol, "/", var_nm, "_ROC_recent-term_eez_df.RDA")) 
  brks_recent_eez <- get_recentterm_brks(recent_dfeez, per) 
    saveRDS(brks_recent_eez, paste0(infol, "/", var_nm, "_ROC_REFeez_breaks_", per*100, "per.RDA")) 

  ## For MPAs ----
  recent_dfmpa <- readRDS(paste0(infol, "/", var_nm, "_ROC_recent-term_mpa_df.RDA")) 
  brks_recent_mpa <- get_recentterm_brks(recent_dfmpa, per)
    saveRDS(brks_recent_mpa, paste0(infol, "/", var_nm, "_ROC_REFmpa_breaks_", per*100, "per.RDA")) 

  ## For outside MPAs ----
  recent_dfoutmpa <- readRDS(paste0(infol, "/", var_nm, "_ROC_recent-term_outsidempa_df.RDA")) 
  brks_recent_outmpa <- get_recentterm_brks(recent_dfoutmpa, per)
    saveRDS(brks_recent_outmpa, paste0(infol, "/", var_nm, "_ROC_REFoutsidempa_breaks_", per*100, "per.RDA")) 
  
  