# Compute summary stats for rate of change variables
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  # var_nm <- tos[1]
  # var_nm <- ph[1]
  var_nm <- o2[1]

  
  
# Folders and background data --------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm[1], "calc1") # Raster stacks per SSP
  plotdf_fol <- make_folder(source_disk, "ROC", var_nm, "plotdfs1")
  threat_fol <- make_folder(source_disk, "ROC", var_nm, "threat_layers1") # Raster stacks per SSP
  per <- 0.3
  
  
  
# Create plotting dfs ----------------------------------------------------------
  
  get_dat <- function(f) {
    out <- rast(f)
    ssp <- str_split_i(basename(f), "_", 4)
    term <- str_split_i(basename(f), "_", 6) %>% 
      str_replace(., ".RDS", "")
    nm <- paste0(var_nm, "_ROC")
    names(out) <- nm
    
    out_name <- paste0(plotdf_fol, "/", var_nm, "_ROC-decadal_", ssp, "_", term, "_as_df.RDS")
    
    out %>%
      as.data.frame(xy = TRUE) %>%
      pivot_longer(cols = -c(1:2), 
                   names_to = "Variable", names_prefix = "X", 
                   values_to = nm,
                   values_drop_na = TRUE) %>% 
      mutate(Variable = str_replace_all(Variable, "[.]", "-"),
             # Variable = as.Date(Variable),
             Term = term,
             Scenario = ssp) %>%
      saveRDS(., out_name)
  }
  
  files <- dir(infol, full.names = TRUE)
  files
  walk(files, get_dat) 

  
  
  ## Summary stats of the above plotting dfs --------------------------------------
  
    summarise_ROC <- function(f) {  
      term <- str_split_i(basename(f), "_", 4) %>% 
        str_replace(., ".RDS", "")
      ssp <- str_split_i(basename(f), "_", 3)
      nm <- paste0(var_nm, "_ROC")
      r <- readRDS(f)
      m <- unique(r$Variable)
      out <- r %>% 
        group_by(Term) %>% 
        dplyr::summarise(Scenario = ssp,
                         Variable = m,
                         Median = median(!!sym(m), na.rm = TRUE),
                         Q1 = quantile(!!sym(m), .25, na.rm = TRUE),
                         Q3 = quantile(!!sym(m), .75, na.rm = TRUE),
                         Q05 = quantile(!!sym(m), .05, na.rm = TRUE),
                         Q95 = quantile(!!sym(m), .95, na.rm = TRUE),
                         Min = min(!!sym(m), na.rm = TRUE),
                         Max = max(!!sym(m), na.rm = TRUE))
    }
    
    files <- dir(plotdf_fol, full.names = TRUE, pattern = "decadal")
    files
    df <- map(files, summarise_ROC) %>% 
      bind_rows() %>% 
      mutate(Term = factor(Term, levels = c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term"))) %>% 
      arrange(Term)
    df
    saveRDS(df, paste0(plotdf_fol, "/", var_nm, "_summary_stats_for_plotting.RDA"))
  
  
  
# Calculate the proportion of the area outside of and within MPAs that are climate refugia ---------------
  
  outmpa <- readRDS(paste0(threat_fol, "/", var_nm, "_ROC_mpa_stack.RDA"))
  outeez <- readRDS(paste0(threat_fol, "/", var_nm, "_ROC_eez_stack.RDA"))
  outmpaoutside <- readRDS(paste0(threat_fol, "/", var_nm, "_ROC_outsidempa_stack.RDA"))
  brkREF_mpa <- readRDS(paste0(threat_fol, "/", var_nm, "_ROC_mpa_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% 
    as.data.frame(.) %>% 
    .[2,]
  brkREF_eez <- readRDS(paste0(threat_fol, "/", var_nm, "_ROC_eez_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% 
    as.data.frame(.) %>% 
    .[2,]
  brkREF_mpaoutside <- readRDS(paste0(threat_fol, "/", var_nm, "_ROC_outsidempa_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% 
    as.data.frame(.) %>% 
    .[2,]
  
  

  ## Compute area are binomial raster that are refugia ----------------------------
    
    calculate_refugia_proportions <- function(x, b, zone) {
      # x_masked <- terra::mask(x, s) # Mask data to only the shapefile
      x_masked <- x # Data already masked
      
      ## Calculate the total area (km2) of the coverage
      total_cell_size <- terra::cellSize(rast(x_masked[[1]][[1]]), unit = "km")
      total_area <- sum(values(total_cell_size)[!is.na(values(rast(x_masked[[1]][[1]])))])
      
      # Mask to where the refugial breaks are (MHWs and tos) 
      if(var_nm == "tos") {
        rr <- x_masked <= b # For positive direction (tos)
      } else {
        rr <- x_masked >= b # Negative direction for ph, o2 
      }
      rr

      rrr <- rast(rr) + 0 # Convert to 1s/0s SpatRaster
      rrr[rrr == 0] <- NA # Make all other values NA
      
      # Process each layer in rrr (the rasterstack for each SSP)
      layer_results <- map_dfr(
        1:nlyr(rrr),
        function(i) {
          layer <- rrr[[i]]
          cell_size <- terra::cellSize(layer, unit = "km")
          refugia_area <- sum(values(cell_size)[!is.na(values(layer))])
          prop_refugia <- refugia_area / total_area * 100
          
          tibble(refugia_area = refugia_area,
                 proportion_refugia = prop_refugia)
          }
        )
      
      # Extract metadata from raster stack name
      variable <- str_split_i(names(x), "_", 1) %>% unique()
      ssp <- str_split_i(names(x), "_", 4) %>% unique()
      term <- str_split_i(names(x), "_", 6)
      
      # Combine metadata with layer results
      df <- layer_results %>%
        mutate(
          variable = variable,
          zone = zone,
          ssp = ssp,
          term = term,
          total_area = total_area) %>%
        dplyr::select(
          variable,
          zone,
          ssp,
          term,
          total_area,
          refugia_area,
          proportion_refugia) %>% 
        filter(term != "recent.term")
      
      return(df)
    }
    
    all_dfs_outside <- map_dfr(outmpaoutside, ~ calculate_refugia_proportions(.x, 
                                                                              brkREF_mpaoutside, 
                                                                              "outsideMPAs"))
      write_rds(all_dfs_outside, paste0(plotdf_fol, "/", var_nm, "_proportion_of_outsideMPAs_that_are_refugia.RDS"))
    
    all_dfs_mpas <- map_dfr(outmpa, ~ calculate_refugia_proportions(.x, 
                                                                    brkREF_mpa, 
                                                                    "MPAs"))
      write_rds(all_dfs_mpas, paste0(plotdf_fol, "/", var_nm, "_proportion_of_MPAs_that_are_refugia.RDS"))
    
  