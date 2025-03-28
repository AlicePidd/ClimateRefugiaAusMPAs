# Density plots - Plotting climate exposure densities for ROC
    # Written by Alice Pidd
      # November 2024


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  

  
# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  var_nm <- tos
  # var_nm <- ph
  # var_nm <- o2

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm[1], "calc1") 
  densityfol <- make_folder(source_disk, "ROC", var_nm[1], "density1") 
  # densityfol_eez <- make_folder(source_disk, "ROC", var_nm[1], "density1/eez_dfs") 
  # densityfol_mpa <- make_folder(source_disk, "ROC", var_nm[1], "density1/mpa_dfs") 
  # densityfol_outmpa <- make_folder(source_disk, "ROC", var_nm[1], "density1/outmpa_dfs") 
  
  
  
# Create dfs for density plotting --------------------------------------------------
  
  get_dfs <- function(f, mask) {
    out <- rast(f)
    out_masked <- mask(out, mask)
    zone <- deparse(substitute(mask))
    
    ssp <- str_split_i(basename(f), "_", 4)
    term <- str_split_i(basename(f), "_", 6)
    nm <- paste0(var_nm[1], "_ROC")
    names(out_masked) <- nm
    out_name <- paste0(densityfol, "/", nm, "_", ssp, "_", term, "_", zone, "_densitydf.RDA")
    
    out_masked %>%
      as.data.frame(xy = TRUE) %>%
      pivot_longer(cols = -c(1:2), 
                   names_to = "Variable", names_prefix = "X", 
                   values_to = nm,
                   values_drop_na = TRUE) %>% 
      mutate(Variable = str_replace_all(Variable, "[.]", "-"),
             Term = term,
             Scenario = ssp,
             Zone = zone) %>%
      saveRDS(., out_name)
  }
  files <- dir(infol, full.names = TRUE) # All terms
  files

  walk(files, ~ get_dfs(.x, reez))
  walk(files, ~ get_dfs(.x, rmpa))

  
  
# Join the datasets together, with one file for each SSP -----------------------
  
  run_code <- function(term) {
    join_dfs <- function(f, s) {
      m <- grep(s, f, value = TRUE)
      combined_dfs <- m %>%
        map(readRDS) %>%
        bind_rows()
      return(combined_dfs)
    }
    files <- dir(densityfol, full.names = TRUE, pattern = term) %>% 
      grep("densitydf.RDA", ., value = TRUE)
    combined_ssp_dfs <- future_map(ssp_list, ~ join_dfs(files, .x))
    names(combined_ssp_dfs) <- ssp_list
    saveRDS(combined_ssp_dfs, paste0(densityfol, "/", var_nm[1], "_ROC_combinedSSP_", term, "_dflist.RDS"))
  }
  
  ssp_list <- c("ssp126", "ssp245", "ssp370", "ssp585")
  term_list <- c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term")
  walk(term_list, run_code) 
  
  
  
# Find the median of whole dataset for MPAs and EEZ ----------------------------
  # Clunky, but don't have time to fix right now
  
  calculate_stat_by_zone <- function(f) {
    df <- readRDS(f) 
    variable_values <- df[4] 
    ssp <- unique(df$Scenario)
    zone <- unique(df$Zone)
    term <- unique(df$Term)
    
    # Group by Zone and compute the statistic (mean or median) on the 4th column
    median <- variable_values %>%
      dplyr::summarise(
        
        #**Choose**
        Median = median(variable_values$tos_ROC, na.rm = TRUE)) %>%
        # Median = median(variable_values$o2_ROC, na.rm = TRUE)) %>%
        # Median = median(variable_values$ph_ROC, na.rm = TRUE)) %>%
      
      mutate(
        Variable = paste0(var_nm[1], "_ROC"),
        Scenario = ssp, # Add the scenario name to identify the layer
        Term = term,
        Zone = zone
      )
    
    return(median)
  }
  
  files <- dir(densityfol, full.names = TRUE, pattern = "dflist.RDS") 
  files
  median_results <- map_dfr(files, calculate_stat_by_zone)
  median_results
    saveRDS(median_results, paste0(density_folder, "/", met_ttl, "_", metric, "_medians_per_variable-ssp-term-zone.RDA"))
    
    

