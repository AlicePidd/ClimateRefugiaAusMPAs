# Density plots - Plotting climate exposure densities for ROC for the mid-term only
    # Written by Alice Pidd
      # November 2024


# Source things ----------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data"


  
# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  # var_nm <- tos
  # var_nm <- ph
  var_nm <- o2

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm[1], "calc1") 
  density_fol <- make_folder(source_disk, "ROC", var_nm[1], "density1") 
  densityplot_fol <- make_folder(source_disk, "ROC", var_nm[1], "density1/plots") 
  threatlayer_fol <- make_folder(source_disk, "ROC", var_nm[1], "threat_layers1") 
  
  
  
# Data -------------------------------------------------------------------------

  combined_ssp_dfs <- readRDS(paste0(density_fol, "/", var_nm[1], "_ROC_combinedSSP_mid-term_dflist.RDS"))
  combined_ssp_dfs
  per <- 0.3    #**CHANGE:* Percentage of the recent-term data we want to classify as refugia**
  refbrk_mpa <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_ROC_mpa_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  refbrk_eez <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_ROC_eez_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  refbrk_mpaoutside <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_ROC_outsidempa_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  
  medians_mpa <- readRDS(paste0(density_fol, "/", var_nm[1], "_ROC_medians_per_variable-ssp-term-zone.RDS")) %>% 
    subset(Zone == "rmpa" & Term == "mid-term")
  medians_eez <- readRDS(paste0(density_fol, "/", var_nm[1], "_ROC_medians_per_variable-ssp-term-zone.RDS")) %>% 
    subset(Zone == "reez" & Term == "mid-term")

  

# Density plot -----------------------------------------------------------------

  plot_density <- function(d) {
    
    variable <- paste0(var_nm[1], "_ROC")
    
    y_limits <- if(var_nm[1] == "tos") {
      c(0, 20)
      } else if(var_nm[1] == "o2") {
        c(0, 4200)
        } else if(var_nm[1] == "ph") {
          c(0, 1700)
          } else {
            c(0, NA)
            }
    
    ref_params <- if(var_nm[1] == "tos") {
      list(mpa = list(xmin = -Inf, xmax = refbrk_mpa),
           eez = list(xmin = -Inf, xmax = refbrk_eez))
      } else {
        list(mpa = list(xmin = refbrk_mpa, xmax = Inf),
             eez = list(xmin = refbrk_eez, xmax = Inf))
        }
    
    imap(d, function(df, ssp) {
      min_val <- min(purrr::map_dbl(d, ~ min(.x[[variable]], na.rm = TRUE)))
      max_val <- max(purrr::map_dbl(d, ~ max(.x[[variable]], na.rm = TRUE)))
      median_mpa <- medians_mpa %>% subset(Scenario == ssp) %>% # Get median for each zone and each ssp
        pull(var = Median) 
      median_eez <- medians_eez %>% subset(Scenario == ssp) %>% 
        pull(var = Median) 
      
      pal <- c("#0D383E", "#EB8933")
      term <- unique(df$Term)
      
      p <- ggplot(df, aes(
        x = !!sym(variable),
        fill = Zone)) +
        geom_rect(xmin = ref_params$mpa$xmin, xmax = ref_params$mpa$xmax, 
                  ymin = -Inf, ymax = Inf, # Refugia grey box in background
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + # Shade the recent term time period
        geom_rect(xmin = ref_params$eez$xmin, xmax = ref_params$eez$xmax, 
                  ymin = -Inf, ymax = Inf,
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + # Shade the recent term time period
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
        labs(title = paste0("Density plot of ", paste0(var_nm[1], "_ROC"), " for ", ssp),
             x = paste(var_nm[1], "_ROC"), y = "Density") +
        scale_fill_manual(values = pal) +
        scale_x_continuous(limits = c(min_val, max_val), labels = scales::label_number()) +
        ylim(y_limits) +
        theme_minimal() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      ggsave(paste0(densityplot_fol, "/density_plot_median_", term, "_orange_", ssp, "_", var_nm[1], "_ROC.pdf"),
             p,
             width = 8,
             height = 6,
             dpi = 300)
    })
  }
  plot_density(combined_ssp_dfs)
  
