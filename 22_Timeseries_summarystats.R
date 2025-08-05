# Calculating timeseries summary stats for each rate of change variable (tos, ph, o2) 
  # Written by Alice Pidd
    # June 2024


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  
  

  # Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  # var_nm <- tos
  # var_nm <- ph
  var_nm <- o2

  
  
# Folders and dataframes -------------------------------------------------------

  plotdf_fol <- make_folder(source_disk, "timeseries1", var_nm[1], "plotdfs")
  summarystats_fol <- make_folder(source_disk, "timeseries1", "", "summarystats")
  ens_fol <- make_folder(source_disk, var_nm[1], "", "8_ensembles")
  ens_yearmean_fol <- make_folder(source_disk, var_nm[1], "", "8_ensembles_yearmean")
  
  eez_df <- readRDS(paste0(plotdf_fol, "/", var_nm[1], "_eez_timeseries_plottingfit.RDA"))
  mpa_df <- readRDS(paste0(plotdf_fol, "/", var_nm[1], "_mpa_timeseries_plottingfit.RDA"))
  outmpa_df <- readRDS(paste0(plotdf_fol, "/", var_nm[1], "_outmpa_timeseries_plottingfit.RDA"))
  
  
  
# Specific summary stats needed for wording in results section -----------
  
  mpa_df %>% 
    subset(Year == 2100)
  outmpa_df %>% 
    subset(Year == 2100)
  eez_df %>% 
    subset(Year == 2100)
  


  
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
  
  
  assign_time_period <- function(year, time_periods) { # Make a term column
    period <- names(time_periods)[sapply(time_periods, function(x) year %in% x)]
    if(length(period) > 0) return(period) else return(NA_character_)
  }
  
  
  dfs <- bind_rows(eez_df, mpa_df, outmpa_df) %>%
    mutate(Term = map_chr(Year, assign_time_period, time_periods))
  dfs
  d <- dfs %>%
    filter(Zone != "EEZ") %>% # Don't include the whole EEZ
    mutate(Term = factor(Term, levels = term_order)) %>% # Make the Terms appear in the right order
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
  saveRDS(d, paste0(summarystats_fol, "/", var_nm[1],"_timeseries_medianfit_data_Table1-maintext.RDA")) # Save
  
  

# Calculate climline for each zone ---------------------------------------------

  datdf_eez <- as.data.frame(eez_df)
  datdf_mpa <- as.data.frame(mpa_df)
  datdf_outmpa <- as.data.frame(outmpa_df)
  
  find_clim_line <- function(d) {
    max_dat <- d %>%  
      filter(Year <= 2014)
    max_anom <- max(max_dat$upr)
    min_anom <- min(max_dat$lwr)
    
    s <- c(min_anom, max_anom)
    return(s)
  }  
  
  clim_line_eez <- find_clim_line(datdf_eez)
  clim_line_mpa <- find_clim_line(datdf_mpa)
  clim_line_outmpa <- find_clim_line(datdf_outmpa)
  
  
  ## Area for each region (from Background_plotting_data.R)
    area_eez #(km^2)
    area_mpa
    area_outsidempa

  
  
# Calculate proportion of area that are over the climline cutoff ---------------
  
  calc_proportion_above_climline <- function(f, mask, climline, shp_area) {
    x <- rast(f)
    x_masked <- terra::mask(x, mask) 
    names(x_masked) <- seq(1995, 2100, by = 1)
    # x_masked_midterm <- x_masked[[as.character(seq(2041, 2060, by = 1))]] # Get just the data for the mid-term (2041-2060)
     
    if(var_nm[1] == "tos") {
      rr <- x_masked >= climline[2] # For tos
    } else {
      rr <- x_masked <= climline[2] # For ph, o2
    }
    rr
    rrr <- rr + 0 # Convert to 1s/0s SpatRaster
    rrr[rrr == 0] <- NA # Make all other values NA
    
    total_area <- shp_area
    
    # Process each layer in rrr (the rasterstack for each SSP)
    layer_results <- map_dfr(
      1:nlyr(rrr),
      function(i) {
        layer <- rrr[[i]]
        cell_size <- terra::cellSize(layer, unit = "km")
        area_above_climeline <- sum(values(cell_size)[!is.na(values(layer))])
        prop_above_climline <- area_above_climeline / total_area * 100
        
        tibble(
          area_above_climeline = area_above_climeline,
          prop_above_climline = prop_above_climline
        )
      }
    )
    
    variable <- str_split_i(basename(f), "_", 1)
    ssp <- str_split_i(basename(f), "_", 4)
    term <- "ensemble"
    zone <- names(mask)
    year <- names(x_masked[1])
    # year <- names(x_masked_midterm[1])
    
    df <- layer_results %>%
      mutate(
        variable = variable,
        zone = zone,
        ssp = ssp,
        year = year,
        term = term,
        total_area = total_area) %>%
      dplyr::select(
        variable,
        zone,
        ssp,
        year = year,
        term,
        total_area,
        area_above_climeline,
        prop_above_climline) %>% 
      filter(term != "recent.term")
    return(df)
  }
  
  files <- dir(ens_yearmean_fol, full.names = TRUE) # Get the yearmean files
  files
  
  clim_dfs_eez <- map_dfr(files, ~ calc_proportion_above_climline(.x, reez, clim_line_eez, area_eez))
    write_rds(clim_dfs_eez, paste0(plotdf_fol, "/", var_nm[1], "-timeseries_proportion_of_eez_above_climline.RDS"))
  clim_dfs_mpa <- map_dfr(files, ~ calc_proportion_above_climline(.x, rmpa, clim_line_mpa, area_mpa))
    write_rds(clim_dfs_mpa, paste0(plotdf_fol, "/", var_nm[1], "-timeseries_proportion_of_mpas_above_climline.RDS"))
  clim_dfs_outmpa <- map_dfr(files, ~ calc_proportion_above_climline(.x, routsidempa, clim_line_outmpa, area_outsidempa))
    write_rds(clim_dfs_outmpa, paste0(plotdf_fol, "/", var_nm[1], "-timeseries_proportion_of_outsidempas_above_climline.RDS"))

