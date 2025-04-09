# Density plots - Plotting climate exposure densities for ROC for the mid-term only
    # Written by Alice Pidd
      # November 2024


# Source things ----------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data"


  
# Metric -----------------------------------------------------------------------
  
  var_nm <- VoCC

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "VoCC", var_nm[1], "calc_cropped") 
  density_fol <- make_folder(source_disk, "VoCC", var_nm[1], "density1") 
  densityplot_fol <- make_folder(source_disk, "VoCC", var_nm[1], "density1/plots") 
  threatlayer_fol <- make_folder(source_disk, "VoCC", var_nm[1], "threat_layers1") 
  
  
  
# Data -------------------------------------------------------------------------

  combined_ssp_dfs <- readRDS(paste0(density_fol, "/", var_nm[1], "_combinedSSP_mid_dflist.RDS")) # Just mid-term
  combined_ssp_dfs
  per <- 0.3    #**CHANGE:* Percentage of the recent-term data we want to classify as refugia**
  refbrk_mpa <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_mpa_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  refbrk_eez <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_eez_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  refbrk_mpaoutside <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_outsidempa_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  
  medians_mpa <- readRDS(paste0(density_fol, "/", var_nm[1], "_medians_per_variable-ssp-term-zone.RDS")) %>% 
    subset(Zone == "rmpa" & Term == "mid")
  medians_eez <- readRDS(paste0(density_fol, "/", var_nm[1], "_medians_per_variable-ssp-term-zone.RDS")) %>% 
    subset(Zone == "reez" & Term == "mid")

  

# Density plot -----------------------------------------------------------------

  ## Truncated at 150 km/decade ------------
  
  combined_ssp_dfs_150 <- purrr::map(combined_ssp_dfs, ~ .x %>%
                                     mutate(VoCCMag = ifelse(VoCCMag > 150, 150, VoCCMag)) %>% 
                                     mutate(VoCCMag = ifelse(VoCCMag < -50, -50, VoCCMag)))
  combined_ssp_dfs_150
  
  
  plot_density_VoCC <- function(d) {
    
    imap(d, ~ {
      
      min_val <- min(purrr::map_dbl(d, ~ min(.x$VoCCMag, na.rm = TRUE)))
      max_val <- max(purrr::map_dbl(d, ~ max(.x$VoCCMag, na.rm = TRUE)))
      median_mpa <- medians_mpa %>% subset(Scenario == .y) %>% 
        pull(var = Median) 
      median_eez <- medians_eez %>% subset(Scenario == .y) %>% 
        pull(var = Median) 
      pal <- c("#0D383E", "#EB8933")  
      
      p <- ggplot(.x, aes(
        x = VoCCMag,
        fill = Zone)) +
        geom_rect(xmin = -Inf, xmax = refbrk_mpa, ymin = -Inf, ymax = Inf, # Refugia grey box in background
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + 
        geom_rect(xmin = -Inf, xmax = refbrk_eez, ymin = -Inf, ymax = Inf,
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + 
        geom_vline(xintercept = refbrk_mpa, # Refugial breaks for MPAs
                   linetype = "dashed", 
                   color =  pal[2],
                   linewidth = 0.5) +
        geom_vline(xintercept = refbrk_eez, # Refugial breaks for whole EEZ
                   linetype = "dashed", 
                   color = pal[1],
                   linewidth = 0.5) +
        geom_vline(xintercept = median_mpa, # Median for the MPAs
                   linetype = "solid", 
                   color =  pal[2],
                   linewidth = 0.6) +
        geom_vline(xintercept = median_eez, # Median for the EEZ
                   linetype = "solid", 
                   color = pal[1],
                   linewidth = 0.6) +
        geom_density(alpha = 0.6) +
        labs(title = paste0("Density plot of ", var_nm[1], " for ", .y),
             x = var_nm[1], y = "Density") +
        scale_fill_manual(values = pal) +
        scale_x_continuous(limits = c(min_val, max_val), labels = scales::label_number()) +
        ylim(c(0, 0.03)) +
        theme_minimal() +
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
      ggsave(paste0(densityplot_fol, "/density_plot_median_mid-term_orange_", .y, "_", var_nm[1], ".pdf"),
             p,
             width = 8,
             height = 6,
             dpi = 300)
    })
  }
  plot_density_VoCC(combined_ssp_dfs_150)

