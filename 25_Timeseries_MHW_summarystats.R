# Calculating timeseries summary stats for the rate of change in marine heatwave cumulative intensity (MHW-ROC)
  # Written by Alice Pidd
    # June 2024


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  
  

# Metric -----------------------------------------------------------------------
  
  var_nm <- mhw
  
  
  
# Folders and dataframes -------------------------------------------------------

  plotdf_fol <- make_folder(source_disk, "timeseries1", var_nm[1], "plotdfs")
  summarystats_fol <- make_folder(source_disk, "timeseries1", "", "summarystats")
  # ens_fol <- make_folder(source_disk, var_nm[1], "", "8_ensembles")
  ens_yearmean_fol <- make_folder(source_disk, "MHW", var_nm[1], "1_MHW_CumInt_ens1")
  
  eez_df <- readRDS(paste0(plotdf_fol, "/", var_nm[1], "_eez_timeseries_plottingfit.RDA"))
  mpa_df <- readRDS(paste0(plotdf_fol, "/", var_nm[1], "_mpa_timeseries_plottingfit.RDA"))
  outmpa_df <- readRDS(paste0(plotdf_fol, "/", var_nm[1], "_outmpa_timeseries_plottingfit.RDA"))
  

  
# Create dfs with summary table for the median fits per 20 reporting period, for each SSP, in each zone -----------
  ## For Table 1 in main text of manuscript
  
  time_periods <- list(
    "recent-term" = 1995:2014,
    "near-term" = 2021:2040,
    "mid-term" = 2041:2060,
    "intermediate-term" = 2061:2080,
    "long-term" = 2081:2100
  )
  
  
  term_order <- c("recent-term",
                  "near-term",
                  "mid-term",
                  "intermediate-term",
                  "long-term")
  
  
  assign_time_period <- function(year, time_periods) {
    period <- names(time_periods)[sapply(time_periods, function(x) year %in% x)]
    if(length(period) > 0) return(period) else return(NA_character_)
  }
  
  
  dfs <- bind_rows(eez_df, mpa_df, outmpa_df) %>%
    mutate(Term = map_chr(Year, assign_time_period, time_periods))
  dfs
  d <- dfs %>%
    # filter(Zone != "eez") %>% # Don't include the whole EEZ
    mutate(Term = factor(Term, levels = term_order)) %>% 
    group_by(Term, SSP, Zone) %>%
    dplyr::summarise(
      median_fit = median(fit_esm, na.rm = TRUE),
      lwr = min(lwr_esm, na.rm = TRUE),
      upr = max(upr_esm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Variable = var_nm[1]) %>% 
    ungroup()
  d
  saveRDS(d, paste0(summarystats_fol, "/", var_nm[1],"_timeseries_medianfit_data_Table1-maintext.RDA")) 
  
  
