# Plot rates of change, and refugia
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Metric -----------------------------------------------------------------------
  
  var_nm <- VoCC[1]
  
  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "VoCC", var_nm, "calc_cropped") 
  layer_fol <- make_folder(source_disk, "VoCC", var_nm, "threat_layers1")
  plotdf_fol <- make_folder(source_disk, "VoCC", var_nm, "plotdfs1")
  
  

# Create plotting dfs ----------------------------------------------------------
  
  get_dat <- function(f, region) {
    out <- readRDS(f)
    out_masked <- mask(out, region)
    ssp <- str_split_i(basename(f), "_", 4)
    term <- paste0(str_split_i(basename(f), "_", 7), "-term")
    nm <- paste0(str_split_i(basename(f), "_", 1), "Mag")

    region_nm <- deparse(substitute(region))
    
    out_name <- paste0(plotdf_fol, "/", var_nm, "-decadal_", ssp, "_", term, "_", region_nm, "_as_df.RDS")
    
    out_masked$voccMag %>%
      as.data.frame(xy = TRUE) %>%
      pivot_longer(cols = -c(1:2), 
                   names_to = "Variable", names_prefix = "X", 
                   values_to = nm,
                   values_drop_na = TRUE) %>% 
      mutate(Variable = str_replace_all(Variable, "[.]", "-"),
             Region = region_nm,
             Term = term,
             Scenario = ssp) %>%
      saveRDS(., out_name)
  }
  
  files <- dir(infol, full.names = TRUE)
  files
  
  walk(files, function(f) get_dat(f, eez))
  walk(files, function(f) get_dat(f, rmpa))
  # walk(files, function(f) get_dat(f, routsidempa))

  
  
# Summary stats of the above plotting dfs --------------------------------------
  
  summarise_vocc <- function(region) {  
    files <- dir(plotdf_fol, full.names = TRUE, pattern = region)

    do_it <- function(f) {
      term <- str_split_i(basename(f), "_", 3)
      ssp <- str_split_i(basename(f), "_", 2)
      reg <- str_split_i(basename(f), "_", 4)
      r <- readRDS(f)
      m <- unique(r$Variable)
      out <- r %>% 
        group_by(Term) %>% 
        dplyr::summarise(Scenario = ssp,
                         Variable = m,
                         Region = reg,
                         Median = median(voccMag, na.rm = TRUE),
                         Q1 = quantile(voccMag, .25, na.rm = TRUE),
                         Q3 = quantile(voccMag, .75, na.rm = TRUE),
                         Q05 = quantile(voccMag, .05, na.rm = TRUE),
                         Q95 = quantile(voccMag, .95, na.rm = TRUE),
                         Min = min(voccMag, na.rm = TRUE),
                         Max = max(voccMag, na.rm = TRUE))
      return(out)
    }
    dat <- map(files, do_it) %>% 
      bind_rows() %>% 
      mutate(Term = factor(Term, levels = c("recent-term", "near-term", "mid-term", "intermediate-term", "long-term"))) %>% 
      arrange(Term)
    dat
    saveRDS(dat, paste0(plotdf_fol, "/", var_nm, "_summary_stats_", region, "_for_plotting.RDA"))
  }
  
  regions <- c("eez", "rmpa")
  walk(regions, summarise_vocc)
  
  
  
# Compute area of each region that are classified as refugia -------------------
  
  per <- 0.3  
  
  eez_stack <- readRDS(paste0(layer_fol, "/", var_nm, "_eez_stack.RDA"))
  mpa_stack <- readRDS(paste0(layer_fol, "/", var_nm, "_mpa_stack.RDA"))
  mpaoutside_stack <- readRDS(paste0(layer_fol, "/", var_nm, "_outsidempa_stack.RDA"))
  
  brks_eez <- readRDS(paste0(layer_fol, "/", var_nm, "_eez_breaks.RDA"))
  brks_mpa <- readRDS(paste0(layer_fol, "/", var_nm, "_mpa_breaks.RDA"))
  brks_mpaoutside <- readRDS(paste0(layer_fol, "/", var_nm, "_outsidempa_breaks.RDA"))
  
  brksREF_eez <- readRDS(paste0(layer_fol, "/", var_nm, "_eez_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% as.data.frame(.) %>% .[2, ]
  brksREF_mpa <- readRDS(paste0(layer_fol, "/", var_nm, "_mpa_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% as.data.frame(.) %>% .[2, ]
  brksREF_mpaoutside <- readRDS(paste0(layer_fol, "/", var_nm, "_outsidempa_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% as.data.frame(.) %>% .[2, ]
  
  
  compute_refugia_proportions <- function(layer, threshold, region_area, region) {
    binary_layer <- layer >= threshold
    
    cell_sizes <- raster::area(binary_layer, na.rm = TRUE, weights = FALSE) 
    refugia_area <- sum(raster::getValues(cell_sizes)[raster::getValues(binary_layer) == 0], na.rm = TRUE)
    proportion_refugia <- refugia_area / region_area * 100
    
    layer_name <- names(layer)
    variable <- strsplit(layer_name, "_")[[1]][1]
    ssp <- strsplit(layer_name, "_")[[1]][4]
    term <- strsplit(layer_name, "_")[[1]][7]
    region <- region
    
    tibble(
      variable = variable,
      region = region,
      ssp = ssp,
      term = term,
      total_area = region_area,
      refugia_area = refugia_area,
      proportion_refugia = proportion_refugia
    ) %>% 
      filter(term != "recent-term.RDS")
  }
  

  ## EEZ  -----
  
    all_dfs_eez <- future_map_dfr(eez_stack, function(stack) {
      map_dfr(1:nlayers(stack), function(i) {
        compute_refugia_proportions(stack[[i]], brksREF_eez, area_eez, "EEZ")
      })
    })
    write_rds(all_dfs_eez, paste0(plotdf_fol, "/", var_nm, "_proportion_of_eez_that_are_refugia.RDS"))
  

  ## MPAS -----
  
    all_dfs_mpas <- future_map_dfr(mpa_stack, function(stack) {
      map_dfr(1:nlayers(stack), function(i) {
        compute_refugia_proportions(stack[[i]], brksREF_mpa, area_mpa, "MPAs")
      })
    })
    write_rds(all_dfs_mpas, paste0(plotdf_fol, "/", var_nm, "_proportion_of_MPAs_that_are_refugia.RDS"))
  
  
  ## Outside of MPAs -----
  
    all_dfs_outside <- future_map_dfr(mpaoutside_stack, function(stack) {
      map_dfr(1:nlayers(stack), function(i) {
        compute_refugia_proportions(stack[[i]], brksREF_mpaoutside, area_outsidempa, "outsideMPAs")
      })
    })
    write_rds(all_dfs_outside, paste0(plotdf_fol, "/", var_nm, "_proportion_of_outsideMPAs_that_are_refugia.RDS"))
  

  
# # Proportion of area over 150km.decade per term --------------------------------
#   
#   ## Whole EEZ -----
#   
#   prop_over150_eez <- function(f) {
#     d <- readRDS(f)
#     
#     file_info <- strsplit(basename(f), "_")[[1]]
#     term <- str_split_i(basename(f), "_", 3)
#     ssp <- str_split_i(basename(f), "_", 2)
#     
#     # Calc proportion of area over 150 km decade^-1
#     proportion_exceeding <- sum(d$voccMag > 150) / nrow(d)
#     
#     tibble(
#       File = basename(f),
#       Scenario = ssp,
#       Term = term,
#       ProportionAreaExceeding150kmdecade = proportion_exceeding
#     )
#   }
#   
#   files <- dir(plotdf_fol, full.names = TRUE, pattern = "term_eez") 
#   files
# 
#   prop_results <- map_dfr(files, prop_over150_eez)  
#   prop_results
#   saveRDS(prop_results, paste0(plotdf_fol, "/VoCCMag_proportion_area_over150kmdecade_eez.RDA"))
#   
#   
#   ## For MPAs -----
#   
#   files <- dir(plotdf_fol, full.names = TRUE, pattern = "term_rmpa") 
#   files
#   
#   prop_results <- map_dfr(files, prop_over150_eez)  
#   prop_results
#   saveRDS(prop_results, paste0(plotdf_fol, "/VoCCMag_proportion_area_over150kmdecade_mpa.RDA"))
#   
#   
#   
# # Proportion of area over 150km.decade whole timeseries ------------------------
#   
#   # Grouped by SSP, to see what the proportion of area over 150 is for the whole timeseries, instead of per 20 year period
#   extract_ssp <- function(file) {
#     str_split_i(basename(file), "_", 3)
#   }
#   
#   files <- dir(plotdf_fol, full.names = TRUE, pattern = "term_eez") 
#   
#   prop_2100 <- files %>%
#     split(., map_chr(., extract_ssp)) %>% 
#     map_dfr(~ {
#       combined_data <- map_dfr(.x, readRDS)  
#       ssp <- extract_ssp(.x[1])  
#       
#       # Calculate proportion exceeding the threshold for the combined data
#       tibble(
#         Scenario = ssp,
#         ProportionAreaExceeding150kmdecade = sum(combined_data$voccMag > 150) / nrow(combined_data)
#       )
#     })
#   prop_2100
  
  
