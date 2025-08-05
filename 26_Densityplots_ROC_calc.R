# Density plots - Plotting climate exposure densities for ROC
    # Written by Alice Pidd
      # November 2024


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  

  
# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  # var_nm <- tos
  # var_nm <- ph
  var_nm <- o2

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm[1], "calc1") 
  densityfol <- make_folder(source_disk, "ROC", var_nm[1], "density1") 

  
  
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

  calculate_stat_by_zone <- function(f) {
    df_list <- readRDS(f)
    v <- paste0(var_nm[1], "_ROC")
    
    median_list <- map(df_list, ~{
      df <- .x
      
      variable_values <- df[[v]]
      ssp <- unique(df$Scenario)
      zone <- unique(df$Zone)
      term <- unique(df$Term)
      
      median_df <- df %>%
        group_by(Zone) %>%
        summarise(Median = median(.data[[v]], na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(
          Scenario = unique(df$Scenario), 
          Term = unique(df$Term),  
          Variable = v
        )
      return(median_df)
    })
    
    combined_results <- bind_rows(median_list)
    return(combined_results)  
    }
  
  files <- dir(densityfol, full.names = TRUE, pattern = "dflist.RDS") 
  files
  
  median_results <- map_dfr(files, calculate_stat_by_zone)
  median_results
  
  saveRDS(median_results, paste0(densityfol, "/", var_nm[1], "_ROC_medians_per_variable-ssp-term-zone.RDS"))
    
    
