# Plotting the heatmap of refugia overlap across all metrics
  # Written by Alice Pidd
    # June 2024

# Source workspace data and dirs -----------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data"
  source("Background_plotting_data.R")

  
  
# Metric name ----------------------------------------------------------------

  var_nm <- "binomial"
  per <- 0.3 

  
  
# Folders ----------------------------------------------------------------------

  ROCtos_binomial_term_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_tos_term_binomial-refugia")) # Repo re-run files
  ROCo2_binomial_term_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_o2_term_binomial-refugia"))
  ROCph_binomial_term_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_ph_term_binomial-refugia"))
  VoCC_binomial_term_fol <- make_folder(source_disk, "VoCC", "binomial_layers", paste0("2_VoCCMag_term_binomial-refugia"))
  ROCMHW_binomial_term_fol <- make_folder(source_disk, "MHW/MHW-ROC", "binomial_layers", paste0("2_MHW-ROC_term_binomial-refugia"))
  
  # ROCtos_binomial_term_fol <- make_folder(source_disk, "metrics_overlap", "ROC", paste0("2_tos_binomial-refugia_term")) # 1st submission round files
  # ROCo2_binomial_term_fol <- make_folder(source_disk, "metrics_overlap", "ROC", paste0("2_o2_binomial-refugia_term"))
  # ROCph_binomial_term_fol <- make_folder(source_disk, "metrics_overlap", "ROC", paste0("2_ph_binomial-refugia_term"))
  # VoCC_binomial_term_fol <- make_folder(source_disk, "VoCC", "binomial_layers", paste0("2_VoCCMag_term_binomial-refugia")) # Updated VoCC layers only
  # ROCMHW_binomial_term_fol <- make_folder(source_disk, "metrics_overlap", "MHW", paste0("2_MHW-ROC_binomial-refugia_term"))
  
  combined_plots_fol <- make_folder(source_disk, "metrics_overlap1", "plots", "")
  


# Plot as a combined plot ------------------------------------------------------
  
  ## Get the files that we want to combine -------------------------------------
  # select_ssp_files <- function(pat) {
  #   tosfiles <- dir(ROCtos_binomial_ssp_fol, full.names = TRUE, pattern = pat) 
  #   o2files <- dir(ROCo2_binomial_ssp_fol, full.names = TRUE, pattern = pat)
  #   phfiles <- dir(ROCph_binomial_ssp_fol, full.names = TRUE, pattern = pat) 
  #   voccfiles <- dir(VoCC_binomial_ssp_fol, full.names = TRUE, pattern = pat) 
  #   mhwfiles <- dir(ROCMHW_binomial_ssp_fol, full.names = TRUE, pattern = pat)
  #   
  #   l <- c(tosfiles, o2files, phfiles, voccfiles, mhwfiles
  #          )
  #   return(l)
  # }

  select_term_files <- function(pat) {
    tosfiles <- dir(ROCtos_binomial_term_fol, full.names = TRUE, pattern = pat)
    o2files <- dir(ROCo2_binomial_term_fol, full.names = TRUE, pattern = pat)
    phfiles <- dir(ROCph_binomial_term_fol, full.names = TRUE, pattern = pat)
    voccfiles <- dir(VoCC_binomial_term_fol, full.names = TRUE, pattern = pat)
    mhwfiles <- dir(ROCMHW_binomial_term_fol, full.names = TRUE, pattern = pat)

    l <- c(tosfiles, o2files, phfiles, voccfiles, mhwfiles)
    l <- grep("recent", l, value = TRUE, invert = TRUE) # Exclude files with "recent-term"

    return(l)
  }

  
    
  ## Plot for EEZ --------------------------------------------------------------
  
  plot_combined_eez <- function(s) {
    files <- select_term_files(s) 
    
    rr <- lapply(files, rast) 
    r <- rast(rr)
    summed <- sum(r)
    x_masked <- terra::mask(summed, reez) 
    colpal <- viridis(n = length(files), option = "inferno")
    
    p <- tm_shape(x_masked) + 
      tm_raster(col.scale = tm_scale(values = colpal,
                                     breaks = seq(0, (length(files)), by = 1))) +
      tm_shape(aus_shp) +
      tm_fill(fill = "grey40") +
      tm_shape(oceania_shp) +
      tm_fill(fill = "grey40") +
      tm_shape(eez) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_title(text = s) +
      tm_layout(frame = FALSE,  # Remove outer border
                inner.margins = c(0.02, 0.02, 0.02, 0.02))  # Adjust margins
    
    print(p) # Show it
    ssp <- str_split_i(basename(files)[1], "_", 4) %>% unique()
    filename <- paste0(combined_plots_fol, "/metric-overlap_refugia_eez_",
                       ssp, "_", "1995-2100_",  per*100, 
                       "per_NoRecentTerm_AUG25.pdf")
    
    tmap_save(p, filename, dpi = 2000, width = 12, height = 8, units = "in")
    return(p)
  }
  
    sort <- c("ssp126", "ssp245", "ssp370", "ssp585") 
    walk(sort, plot_combined_eez)

    
    
  ## Plot for MPAs -------------------------------------------------------------
    
    plot_combined_mpa <- function(s) {
      # files <- select_ssp_files(s)
      files <- select_term_files(s)
      
      rr <- lapply(files, rast)
      r <- rast(rr)
      c <- c(r)
      summed <- sum(r)
      
      x_masked <- terra::mask(summed, rmpa) 
      colpal <- viridis(n = length(files), option = "inferno") 
      
      p <- tm_shape(x_masked) + 
        tm_raster(col.scale = tm_scale(values = colpal,
                                       breaks = seq(0, (length(files)), by = 1))) + 
        tm_shape(aus_shp) +
        tm_fill(fill = "grey40") +
        tm_shape(oceania_shp) +
        tm_fill(fill = "grey40") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_title(text = s) +
        tm_layout(frame = FALSE,  # Remove outer border
                  inner.margins = c(0.02, 0.02, 0.02, 0.02))  # Adjust margins
      
      print(p)
      arranged <- tmap_arrange(p, ncol = 1, widths = 0.6)
      
      ssp <- str_split_i(basename(files)[1], "_", 4) %>%
        unique()
      term <- str_split_i(basename(files)[1], "_", 7) %>%
        unique()
      
      filename <- paste0(combined_plots_fol, "/metric-overlap_refugia_mpas_",
                         ssp, "_", "1995-2100_",  per*100, "per_NoRecentTerm_AUG25.pdf")
      
      # Save with explicit dimensions to avoid margin errors
      tmap_save(p, filename, dpi = 2000, width = 12, height = 8, units = "in")
    }
    
    sort <- c("ssp126", "ssp245", "ssp370", "ssp585") 
    walk(sort, plot_combined_mpa)
    
    
    
    
  ## Plot for outside MPAs -----------------------------------------------------
    
    plot_combined <- function(s) {
      # files <- select_ssp_files(s)
      files <- select_term_files(s)
      rr <- lapply(files, rast)
      r <- rast(rr)
      c <- c(r)
      summed <- sum(r)
      x_masked <- terra::mask(summed, outsideMPA_shp)
      colpal <- viridis(n = length(files), option = "inferno")
      
      p <- tm_shape(x_masked) +
        tm_raster(col.scale = tm_scale(values = colpal,
                                       breaks = seq(0, (length(files)), by = 1))) +
        tm_shape(aus_shp) +
        tm_fill(fill = "grey40") +
        tm_shape(oceania_shp) +
        tm_fill(fill = "grey40") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_title(text = s) +
        tm_layout(frame = FALSE,  # Remove outer border
                  inner.margins = c(0.02, 0.02, 0.02, 0.02))  # Adjust margins
      
      print(p)
      arranged <- tmap_arrange(p, ncol = 1, widths = 0.6)
      
      ssp <- str_split_i(basename(files)[1], "_", 4) %>%
        unique()
      term <- str_split_i(basename(files)[1], "_", 7) %>%
        unique()
      
      filename <- paste0(combined_plots_fol, "/metric-overlap_refugia_outsidempas_",
                         ssp, "_", "1995-2100_",  per*100, "per_NoRecentTerm_AUG25.pdf")
      
      # Save with explicit dimensions to avoid margin errors
      tmap_save(p, filename, dpi = 2000, width = 12, height = 8, units = "in")
    }
    
    sort <- c("ssp126", "ssp245", "ssp370", "ssp585") # Display by SSP, 1993-2100
    walk(sort, plot_combined)
    