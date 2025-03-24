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
  plotdf_fol <- make_folder(source_disk, "timeseries1", "", "plotting_dfs1")

  
  var_anoms_fol <- make_folder("var_anoms_folder", "7_anomalies", dest_disk, met[1])
  var_anoms_yearmean_fol <- make_folder("var_anoms_annual_folder", "7_anomalies_yearmean", source_disk, met[1])
  timeseries_eez_fol <- make_folder("timeseries_eez_folder", "timeseries_eez", source_disk, met[1])
  timeseries_outmpa_fol <- make_folder("timeseries_outmpa_folder", "timeseries_outsidempa", source_disk, met[1])
  timeseries_mpa_fol <- make_folder("timeseries_mpa_folder", "timeseries_mpa", source_disk, met[1])
  # plotdf_eez_fol <- make_folder("plotdf_eez_folder", "", source_disk, "timeseries/plottingdfs_eez")
  # plotdf_outmpa_fol <- make_folder("plotdf_outmpa_folder", "", source_disk, "timeseries/plottingdfs_outsidempa")
  # plotdf_mpa_fol <- make_folder("plotdf_mpa_folder", "", source_disk, "timeseries/plottingdfs_mpa")
  mask_fol <- make_folder("mask_folder", "landsea_masks", dest_disk, "masks") 
  
  
  