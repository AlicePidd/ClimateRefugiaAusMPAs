# Calculating timeseries of each rate of change variable (tos, ph, o2, MHW-ROC) 
  # Written by Alice Pidd
    # June 2024


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
  
  # met <- c("tos", "/Volumes/AliceShield/clim_data/tos/timeseries", "Sea surface temperature anomalies relative to the historic period (1995-2014)", "Sea surface temperature anomalies (°C)")
  # met <- c("ph", "/Volumes/AliceShield/clim_data/ph/timeseries", "Ocean acidification anomalies relative to the historic period (1995-2014)", "Ocean acidification anomalies (pH)")
  # met <- c("o2", "/Volumes/AliceShield/clim_data/o2/timeseries", "Ocean deoxygenation anomalies relative to the historic period (1995-2014)", "Ocean deoxygenation anomalies (mg/L)")
  
  
  calc_fol <- make_folder(source_disk, "timeseries1", "", "calc1") 
  anoms_fol <- make_folder(dest_disk, var_nm[1], "", "7_anomalies")
  anoms_yearmean_fol <- make_folder(dest_disk, var_nm[1], "", "7_anomalies_yearmean")
  
  yearmean_eez_fol <- make_folder(source_disk,  "timeseries1", "", "esm_yearmean_eez")
  yearmean_mpa_fol <- make_folder(source_disk,  "timeseries1", "", "esm_yearmean_mpa")
  yearmean_outmpa_fol <- make_folder(source_disk,  "timeseries1", "", "esm_yearmean_outmpa")
  
  plotdf_fol <- make_folder(source_disk, "timeseries1", "", "plotdfs1")
  
  
  # plotdf_eez_fol <- make_folder("plotdf_eez_folder", "", source_disk, "timeseries/plottingdfs_eez")
  # plotdf_outmpa_fol <- make_folder("plotdf_outmpa_folder", "", source_disk, "timeseries/plottingdfs_outsidempa")
  # plotdf_mpa_fol <- make_folder("plotdf_mpa_folder", "", source_disk, "timeseries/plottingdfs_mpa")
  # mask_fol <- make_folder("mask_folder", "landsea_masks", dest_disk, "masks") 
  
  
  
  
# Compute yearmean in each ESM file --------------------------------------------
  
  make_annual <- function(f) {
    out_file <- str_replace(f,
                            str_split(basename(f), "_") %>%
                              unlist() %>%
                              .[2],
                            "Oyear") %>%
      str_replace(anoms_fol, anoms_yearmean_fol) %>%
      str_replace("gn", "anom")
    cdo_code <- paste0("cdo -L -yearmean ", f, " ", out_file) 
    system(cdo_code)
  }
  
  files <- dir(anoms_fol, full.names = TRUE, pattern = ".nc")
  files
  
  plan(multisession, workers = 10)
  walk(files, make_annual)
  plan(sequential)
  
  
  
# Compute field mean for each ESM from the yearmean files ----------------------

  orig_files <- dir(anoms_yearmean_fol, full.names = TRUE) %>% 
    str_subset(".nc")
  f <- orig_files[1]
  mask <- eez_mask
  region_nm <- "eez"

  do_mask_fldmean <- function(f, region, region_nm, mask){
    
    o_file <- basename(f) %>%
      str_replace(var_nm[1], paste0(region_nm, "-", var_nm[1])) %>%
      str_replace("anomaly", "anom") %>%
      paste0(yearmean_eez_fol, "/", .)
    
    cdo_code <- paste0("cdo -L -fldmean -selyear,1995/2100 -mul ", mask, " ", f, " ", o_file)
    system(cdo_code)
    
    
    # o_file_eez <- basename(f) %>% 
    #   str_replace(met[1], paste0("EEZ", met[1])) %>% 
    #   str_replace("anomaly", "anom") %>% 
    #   # str_replace("19930101", "19950101") %>% 
    #   paste0(timeseries_eez_folder, "/", .)
    
    # o_file_outmpa <- basename(f) %>%
    #   str_replace(met[1], paste0("outsideMPA", met[1])) %>%
    #   str_replace("anomaly", "anom") %>%
    #   # str_replace("19930101", "19950101") %>%
    #   paste0(timeseries_outmpa_folder, "/", .)

    # o_file_mpa <- basename(f) %>%
    #   str_replace(met[1], paste0("MPA", met[1])) %>%
    #   # str_replace("19930101", "19950101") %>%
    #   paste0(timeseries_mpa_folder, "/", .)
    
    # cdo_code <- paste0("cdo -L -fldmean -selyear,1995/2100 -mul ", eez_mask, " ", f, " ", o_file_eez)
    # system(cdo_code)
    # cdo_code <- paste0("cdo -L -fldmean -selyear,1995/2100 -mul ", outsidempa_mask, " ", f, " ", o_file_outmpa)
    # system(cdo_code)
    cdo_code <- paste0("cdo -L -fldmean -selyear,1995/2100 -mul ", mpa_mask, " ", f, " ", o_file_mpa)
    system(cdo_code)
  }
  
  plan(multisession, workers = 10)
  walk(orig_files, do_mask_fldmean)
  plan(sequential)
  
  walk(dir(timeseries_eez_folder, full.names = TRUE), ~plot(rast(.x)[] %>% as.vector(), type = "l")) # Provides logical-looking plots...
  # plot(rast("/Volumes/AliceShield/clim_data/o2/timeseries_eez/EEZo2_Oyear_CanESM5_ssp585_r1i1p1f1_anom_199501-210012.nc")[] %>% as.vector(), type = "l")

  
  
  
# Create plotting dfs from timeseries files ------------------------------------
  
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
  
  
  ## Do it for eez files -------------
    eez_files <- dir(timeseries_eez_folder, full.names = TRUE, pattern = ".nc")
    all_dat <- map(eez_files, get_plotting_dfs) %>% 
      bind_rows()
    all_dat
    pdat <- all_dat %>% # Get plotting data (median, and the spread of model medians)
      filter(Model != "ensemble") %>% # Just in case
      droplevels() %>%
      group_by(Zone, SSP, Year) %>% 
      dplyr::summarise(fit = median(Value, na.rm = TRUE),
                       lwr = min(Value, na.rm = TRUE),
                       upr = max(Value, na.rm = TRUE)) %>% 
      mutate(Variable = met[1])
    pdat
    range(pdat$lwr)
    range(pdat$upr)
    saveRDS(pdat, paste0(plotdf_eez_folder, "/", met[1],"_timeseries_plottingfit_eez.RDA")) # Save

  
  ## Do it for outsidempa files -------------
    outsidempa_files <- dir(timeseries_outmpa_folder, full.names = TRUE, pattern = ".nc")
    all_dat <- map(outsidempa_files, get_plotting_dfs) %>% 
      bind_rows()
    all_dat
    pdat <- all_dat %>% # Get plotting data (median, and the spread of model medians)
      filter(Model != "ensemble") %>% # Just in case
      droplevels() %>%
      group_by(Zone, SSP, Year) %>% 
      dplyr::summarise(fit = median(Value, na.rm = TRUE),
                       lwr = min(Value, na.rm = TRUE),
                       upr = max(Value, na.rm = TRUE)) %>% 
      mutate(Variable = met[1])
    pdat
    range(pdat$lwr)
    range(pdat$upr)
    saveRDS(pdat, paste0(plotdf_outmpa_folder, "/", met[1],"_timeseries_plottingfit_outsidempa.RDA")) # Save
  
  
  ## Do it for mpa files -------------
    mpa_files <- dir(timeseries_mpa_folder, full.names = TRUE, pattern = ".nc") 
    all_dat <- map(mpa_files, get_plotting_dfs) %>% 
      bind_rows()
    all_dat
    pdat <- all_dat %>%  # Get plotting data (median, and the spread of model medians)
      filter(Model != "ensemble") %>% # Just in case
      droplevels() %>%
      group_by(Zone, SSP, Year) %>% 
      dplyr::summarise(fit = median(Value, na.rm = TRUE),
                       lwr = min(Value, na.rm = TRUE),
                       upr = max(Value, na.rm = TRUE)) %>% 
      mutate(Variable = met[1])
    pdat 
    range(pdat$lwr)
    range(pdat$upr)
    saveRDS(pdat, paste0(plotdf_mpa_folder, "/", met[1],"_timeseries_plottingfit_mpa.RDA")) # Save


    
# Looking at (but not saving) summary stat dfs -------------------------------------------------------
  
  tos_mpa_df <- readRDS(paste0(plotdf_mpa_folder, "/tos_timeseries_plottingfit_mpa.RDA"))
  tos_outmpa_df <- readRDS(paste0(plotdf_outmpa_folder, "/tos_timeseries_plottingfit_outsidempa.RDA"))
  tos_eez_df <- readRDS(paste0(plotdf_eez_folder, "/tos_timeseries_plottingfit_eez.RDA"))
  o2_mpa_df <- readRDS(paste0(plotdf_mpa_folder, "/o2_timeseries_plottingfit_mpa.RDA"))
  o2_outmpa_df <- readRDS(paste0(plotdf_outmpa_folder, "/o2_timeseries_plottingfit_outsidempa.RDA"))
  o2_eez_df <- readRDS(paste0(plotdf_eez_folder, "/o2_timeseries_plottingfit_eez.RDA"))
  ph_mpa_df <- readRDS(paste0(plotdf_mpa_folder, "/ph_timeseries_plottingfit_mpa.RDA"))
  ph_outmpa_df <- readRDS(paste0(plotdf_outmpa_folder, "/ph_timeseries_plottingfit_outsidempa.RDA"))
  ph_eez_df <- readRDS(paste0(plotdf_eez_folder, "/ph_timeseries_plottingfit_eez.RDA"))
  
  tos_dfs <- bind_rows(tos_mpa_df, tos_outmpa_df, tos_eez_df)
  ph_dfs <- bind_rows(ph_mpa_df, ph_outmpa_df, ph_eez_df)
  o2_dfs <- bind_rows(o2_mpa_df, o2_outmpa_df, o2_eez_df)
  
  tos_dfs %>%
    group_by(SSP, Zone) %>% 
    filter(fit == max(fit))

  o2_dfs %>%
    group_by(SSP, Zone) %>% 
    filter(fit == min(fit))
  
  ph_dfs %>%
    group_by(SSP, Zone) %>% 
    filter(fit == min(fit),
           SSP == "ssp245")
