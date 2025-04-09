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
  
  # ROCtos_binomial_ssp_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_tos_ssp_binomial-refugia"))
  # ROCo2_binomial_ssp_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_o2_ssp_binomial-refugia"))
  # ROCph_binomial_ssp_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_ph_ssp_binomial-refugia"))
  # VoCC_binomial_ssp_fol <- make_folder(source_disk, "VoCC", "binomial_layers", paste0("2_VoCCMag_ssp_binomial-refugia"))
  # ROCMHW_binomial_ssp_fol <- make_folder(source_disk, "MHW/MHW-ROC", "binomial_layers", paste0("2_MHW-ROC_ssp_binomial-refugia"))
  
  ROCtos_binomial_term_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_tos_term_binomial-refugia"))
  ROCo2_binomial_term_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_o2_term_binomial-refugia"))
  ROCph_binomial_term_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_ph_term_binomial-refugia"))
  VoCC_binomial_term_fol <- make_folder(source_disk, "VoCC", "binomial_layers", paste0("2_VoCCMag_term_binomial-refugia"))
  ROCMHW_binomial_term_fol <- make_folder(source_disk, "MHW/MHW-ROC", "binomial_layers", paste0("2_MHW-ROC_term_binomial-refugia"))
  
  combined_plots_fol <- make_folder(source_disk, "metrics_overlap1", "plots", "")
  


# Plot as a combined plot ------------------------------------------------------
  ## Create a combined ROC multivariate dataset by getting whatever the binomial folder variable is (tos, o2, ph) ----
  # fol_var <- binomialROC_ssp_folder %>%
  #   str_split_i(., "/", 6)
  # fol_var <- binomialROC_term_folder %>%
  #   str_split_i(., "/", 6)
  
  
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
    tosfiles <- dir(ROCtos_binomial_ssp_fol, full.names = TRUE, pattern = pat)
    o2files <- dir(ROCo2_binomial_ssp_fol, full.names = TRUE, pattern = pat)
    phfiles <- dir(ROCph_binomial_term_fol, full.names = TRUE, pattern = pat)
    voccfiles <- dir(VoCC_binomial_term_fol, full.names = TRUE, pattern = pat)
    mhwfiles <- dir(ROCMHW_binomial_term_fol, full.names = TRUE, pattern = pat)

    l <- c(tosfiles, o2files, phfiles, voccfiles, mhwfiles)
    # l <- grep("recent-term", l, value = TRUE, invert = TRUE) # Exclude files with "recent-term"

    return(l)
  }
  # select_term_files("ssp370")

    
  ## Plot for EEZ --------------------------------------------------------------
  
  plot_combined <- function(s) {
    # files <- select_ssp_files(s)
    files <- select_term_files(s) # Get files for each SSP
    # files <- grep("recent-term", files, value = TRUE)  # For getting ONLY recent term files
    
    rr <- lapply(files, rast) 
    r <- rast(rr)
    c <- c(r)
    summed <- sum(r)
    
    #** CHOOSE*
    x_masked <- terra::mask(summed, reez) # Mask data to only Australian waters 
    colpal <- viridis(n = length(files), option = "inferno") # By term

     p <- tm_shape(x_masked) + 
        tm_raster(palette = colpal,
                  #** CHOOSE*
                  breaks = seq(0, (length(files)), by = 1)) + # By SSP - breaks the palette up into number of components being overlaid (4 SSPs x 5 vars)
                  # breaks = seq(0, 3, by = 1)) + # By variable
        tm_shape(oceaniaAsia) +
        tm_fill("grey40") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_layout(title = s) # name it by the SSP
     print(p) # Show it
     arranged <- tmap_arrange(p, ncol = 1, widths = 0.6)
  
     ssp <- str_split_i(basename(files)[1], "_", 4) %>%
      unique()
     term <- str_split_i(basename(files)[1], "_", 7) %>%
      unique()

     #** CHOOSE*
     ## Plot by SSP
        filename <- paste0(combined_plots_fol, "/metric-overlap_refugia_eez_",
                          ssp, "_", "1995-2100_",  per*100, 
                          # "per_WithRecentTerm_JAN25.pdf"
                          # "per_RecentTermOnly_JAN25.pdf"
                          "per_NoRecentTerm_APR25.pdf"
                          )
     ## Plot by term
     # filename <- paste0(combined_plots_folder, "/metric-overlap_refugia_eez_",
     # "allSSPs_", term, "_", per*100, "per.pdf") # By term
     tmap_save(p, filename, dpi = 2000)
  }
  
    #** CHOOSE* 
      ## By each SSP across all terms
        # tos_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "tos", .)
        # o2_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "o2", .)
        # ph_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "ph", .)
        # vocc_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "VoCC", .)
        # # mhw_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "MHW", .)
        sort <- c("ssp126", "ssp245", "ssp370", "ssp585") # Display by SSP, 1993-2100

    walk(sort, plot_combined)

    
    
  ## Plot for MPAs -------------------------------------------------------------
    
    mask_mpa <- paste0(mask_folder, "/mask_IUCN_MPAs_NAs.nc") %>% 
      rast() # Call the mpa mask

    plot_combined <- function(s) {
      # files <- select_ssp_files(s)
      files <- select_term_files(s)
      
      rr <- lapply(files, rast)
      r <- rast(rr)
      c <- c(r)
      summed <- sum(r)
      
      #** CHOOSE*
      x_masked <- terra::mask(summed, mask_mpa) # Mask data to only Australian waters 
      colpal <- viridis(n = length(files), option = "inferno") # By term

      p <- tm_shape(x_masked) + 
        tm_raster(palette = colpal,
                  #** CHOOSE*
                  breaks = seq(0, (length(files)), by = 1)) + # By SSP
                  # breaks = seq(0, 3, by = 1)) + # By variable
        tm_shape(oceaniaAsia) +
        tm_fill("grey40") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_layout(title = s)
      print(p)
      arranged <- tmap_arrange(p, ncol = 1, widths = 0.6)
      
      ssp <- str_split_i(basename(files)[1], "_", 4) %>%
        unique()
      term <- str_split_i(basename(files)[1], "_", 7) %>%
        unique()
      
      #** CHOOSE*
      ## Plot by SSP
      filename <- paste0(combined_plots_folder, "/metric-overlap_refugia_mpas_",
                         ssp, "_", "1995-2100_",  per*100, "per_WithRecentTerm_JAN25.pdf")
      ## Plot by term
      # filename <- paste0(combined_plots_folder, "/metric-overlap_refugia_eez_",
      # "allSSPs_", term, "_", per*100, "per.pdf") # By term
      tmap_save(p, filename, dpi = 2000)
    }
    
    #** CHOOSE* 
    ## By each SSP across all terms
      # tos_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "tos", .)
      # o2_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "o2", .)
      # ph_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "ph", .)
      # vocc_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "VoCC", .)
      # # mhw_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "MHW", .)
    sort <- c("ssp126", "ssp245", "ssp370", "ssp585") # Display by SSP, 1993-2100
    
    walk(sort, plot_combined)
    
    
    
    
  ## Plot for outside MPAs -------------------------------------------------------------
    
    mask_outsidempa <- paste0(mask_folder, "/mask_outsideMPAs_NAs.nc") %>% 
      rast() # Call the mpa mask
  
    plot_combined <- function(s) {
      # files <- select_ssp_files(s)
      files <- select_term_files(s)
      
      rr <- lapply(files, rast)
      r <- rast(rr)
      c <- c(r)
      summed <- sum(r)
      
      #** CHOOSE*
      x_masked <- terra::mask(summed, mask_outsidempa) # Mask data to only Australian waters 
      colpal <- viridis(n = length(files), option = "inferno") # By term
      
      p <- tm_shape(x_masked) + 
        tm_raster(palette = colpal,
                  #** CHOOSE*
                  breaks = seq(0, (length(files)), by = 1)) + # By SSP
        # breaks = seq(0, 3, by = 1)) + # By variable
        tm_shape(oceaniaAsia) +
        tm_fill("grey40") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_layout(title = s)
      print(p)
      arranged <- tmap_arrange(p, ncol = 1, widths = 0.6)
      
      ssp <- str_split_i(basename(files)[1], "_", 4) %>%
        unique()
      term <- str_split_i(basename(files)[1], "_", 7) %>%
        unique()
      
      #** CHOOSE*
      ## Plot by SSP
      filename <- paste0(combined_plots_folder, "/metric-overlap_refugia_outsidempas_",
                         ssp, "_", "1995-2100_",  per*100, "per_WithRecentTerm_JAN25.pdf")
      ## Plot by term
      # filename <- paste0(combined_plots_folder, "/metric-overlap_refugia_eez_",
      # "allSSPs_", term, "_", per*100, "per.pdf") # By term
      tmap_save(p, filename, dpi = 2000)
    }
    
    #** CHOOSE* 
    ## By each SSP across all terms
    # tos_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "tos", .)
    # o2_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "o2", .)
    # ph_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "ph", .)
    # vocc_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "VoCC", .)
    # # mhw_binomial_fol <- paste0(binomialROC_term_folder) %>% gsub(fol_var, "MHW", .)
    sort <- c("ssp126", "ssp245", "ssp370", "ssp585") # Display by SSP, 1993-2100
    
    walk(sort, plot_combined)
    