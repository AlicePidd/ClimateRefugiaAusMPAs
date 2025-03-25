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

  
  
# Summary stats of the above plotting dfs --------------------------------------
  
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
  
  