# Calculating timeseries of each rate of change variable (tos, ph, o2) 
  # Written by Alice Pidd
    # June 2024


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  
  

# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  # var_nm <- tos[1]
  # var_nm <- ph[1]
  var_nm <- o2[1]

  
  
# Folders ----------------------------------------------------------------------

  anoms_fol <- make_folder(source_disk, var_nm[1], "", "7_anomalies")
  anoms_yearmean_fol <- make_folder(source_disk, var_nm[1], "", "7_anomalies_yearmean")
  ens_fol <- make_folder(source_disk, var_nm[1], "", "8_ensembles")
  ens_yearmean_fol <- make_folder(source_disk, var_nm[1], "", "8_ensembles_yearmean")
  
  yearmean_eez_fol <- make_folder(source_disk,  "timeseries1", var_nm[1], "esm_yearmean_eez")
  yearmean_mpa_fol <- make_folder(source_disk,  "timeseries1", var_nm[1], "esm_yearmean_mpa")
  yearmean_outmpa_fol <- make_folder(source_disk,  "timeseries1", var_nm[1], "esm_yearmean_outmpa")
  
  plotdf_fol <- make_folder(source_disk, "timeseries1", var_nm[1], "plotdfs")


  
# Compute yearmean in each ESM file --------------------------------------------
  
  make_annual <- function(f, in_fol, out_fol) {
    out_file <- str_replace(f,
                            str_split(basename(f), "_") %>%
                              unlist() %>%
                              .[2],
                            "Oyear") %>%
      str_replace(in_fol, out_fol) %>%
      str_replace("gn", "anom")
    cdo_code <- paste0("cdo -L -yearmean ", f, " ", out_file) 
    system(cdo_code)
  }
  
  
  ## For ESM anomaly files --------
  
  files <- dir(anoms_fol, full.names = TRUE)
  files
  walk(files, ~ make_annual(.x, anoms_fol, anoms_yearmean_fol)) 
  

  ## For ensemble anomaly files --------
  
  files <- dir(ens_fol, full.names = TRUE)
  files
  walk(files, ~ make_annual(.x, ens_fol, ens_yearmean_fol)) 

    
  
# Compute field mean for each ESM from the yearmean files ----------------------

  do_mask_fldmean <- function(f, region_nm, mask, o_fol){
    o_file <- basename(f) %>%
      str_replace(var_nm[1], paste0(region_nm, "-", var_nm[1])) %>%
      str_replace("anomaly", "anom") %>%
      paste0(o_fol, "/", .)
    cdo_code <- paste0("cdo -L -fldmean -selyear,1995/2100 -mul ", mask, " ", f, " ", o_file)
    system(cdo_code)
  }
  
  files <- dir(anoms_yearmean_fol, full.names = TRUE)
  files
  
  walk(files, ~ do_mask_fldmean(.x, "eez", "/Volumes/AliceShield/clim_data/masks/mask_EEZ_NAs.nc", yearmean_eez_fol)) 
  walk(files, ~ do_mask_fldmean(.x, "mpa", "/Volumes/AliceShield/clim_data/masks/mask_IUCN_MPAs_NAs.nc", yearmean_mpa_fol)) 
  walk(files, ~ do_mask_fldmean(.x, "outmpa", "/Volumes/AliceShield/clim_data/masks/mask_outsideMPAs_NAs.nc", yearmean_outmpa_fol)) 
  
  
  ## Eyeball the timeseries
  
    walk(dir(yearmean_eez_fol, full.names = TRUE), ~plot(rast(.x)[] %>% as.vector(), type = "l")) # Provides logical-looking plots...

  
  
# Create plotting dfs from ESM timeseries files --------------------------------
  
  get_plotting_dfs <- function(f) {
    bits <- basename(f) %>% 
      str_split("_") %>% 
      unlist()
    yr_strt <- bits[7] %>% 
      str_sub(1,4) %>% 
      as.numeric()
    yr_end <- str_split_i(basename(f), "_", 7) %>% 
      str_split_i(., "-", 2) %>% 
      str_sub(1,4)
    dat <- rast(f) %>% 
      values() %>% 
      as.vector() %>% 
      tibble(Zone = str_sub(bits[1], 1, 3),
             Model = bits[3],
             SSP = bits[4],
             Year = yr_strt:yr_end, 
             Value = .)
    return(dat)
  }
  

  do_it <- function(pth, region_nm){
    files <- dir(pth, full.names = TRUE)
    all_dat <- map(files, get_plotting_dfs) %>% 
      bind_rows()
    all_dat
    pdat <- all_dat %>% # Get plotting data (median, and the spread of model medians)
      filter(Model != "ensemble") %>% # Just in case. We only want individual ESM files
      droplevels() %>%
      group_by(Zone, SSP, Year) %>% 
      dplyr::summarise(fit_esm = median(Value, na.rm = TRUE),
                       lwr_esm = min(Value, na.rm = TRUE),
                       upr_esm = max(Value, na.rm = TRUE)) %>% 
      mutate(Variable = var_nm[1])
    saveRDS(pdat, paste0(plotdf_fol, "/", var_nm[1], "_", region_nm, "_timeseries_plottingfit.RDA"))
  }
  
  walk2(yearmean_eez_fol, "eez", do_it)
  walk2(yearmean_mpa_fol, "mpa", do_it)
  walk2(yearmean_outmpa_fol, "outmpa", do_it)
  
  
  
  
# Compute yearmean for ensembled anomaly files ---------------------------------
  
  files <- dir(ens, full.names = TRUE)
  files
  
  walk(files, ~ do_mask_fldmean(.x, "eez", "/Volumes/AliceShield/clim_data/masks/mask_EEZ_NAs.nc", yearmean_eez_fol)) 
  walk(files, ~ do_mask_fldmean(.x, "mpa", "/Volumes/AliceShield/clim_data/masks/mask_IUCN_MPAs_NAs.nc", yearmean_mpa_fol)) 
  walk(files, ~ do_mask_fldmean(.x, "outmpa", "/Volumes/AliceShield/clim_data/masks/mask_outsideMPAs_NAs.nc", yearmean_outmpa_fol)) 
  
  
  ## Eyeball the timeseries
  
  walk(dir(yearmean_eez_fol, full.names = TRUE), ~plot(rast(.x)[] %>% as.vector(), type = "l")) # Provides logical-looking plots...
  
  
  

