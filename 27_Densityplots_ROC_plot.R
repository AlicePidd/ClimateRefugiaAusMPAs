# Density plots - Plotting climate exposure densities for ROC for the mid-term only
    # Written by Alice Pidd
      # November 2024


# Source things ----------------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data"


  
# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  var_nm <- tos
  # var_nm <- ph
  # var_nm <- o2

  
  
# Folders ----------------------------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm[1], "calc1") 
  density_fol <- make_folder(source_disk, "ROC", var_nm[1], "density1") 
  densityplot_fol <- make_folder(source_disk, "ROC", var_nm[1], "density1/plots") 
  threatlayer_fol <- make_folder(source_disk, "ROC", var_nm[1], "threat_layers1") 
  
  
  
# Data -------------------------------------------------------------------------

  combined_ssp_dfs <- readRDS(paste0(density_fol, "/", var_nm[1], "_ROC_combinedSSP_mid-term_dflist.RDS"))
  
  per <- 0.3    #**CHANGE:* Percentage of the present day data we want to classify as refugia**
  brksREF_mpa <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_ROC_mpa_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  brksREF_eez <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_ROC_eez_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  brksREF_mpaoutside <- readRDS(paste0(threatlayer_fol, "/", var_nm[1], "_ROC_outsidempa_refugia_breaks_", per*100, "per.RDA"))[[1]][2]
  
  medians_mpa <- readRDS(paste0(density_fol, "/", var_nm[1], "_ROC_medians_per_variable-ssp-term-zone.RDS")) %>% 
    subset(Zone == "rmpa" & Term == "mid-term")
  medians_eez <- readRDS(paste0(density_fol, "/", var_nm[1], "_ROC_medians_per_variable-ssp-term-zone.RDS")) %>% 
    subset(Zone == "reez" & Term == "mid-term")
  
  

# Density plot -----------------------------------------------------------------
  
  ## For tos ------
  plot_density_tos <- function(d) {
    
    df <- readRDS(d)
    variable <- paste0(var_nm[1], "_ROC")
    
    imap(df, ~ {
      
      min_val <- min(purrr::map_dbl(df, ~ min(.x$variable, na.rm = TRUE)))
      max_val <- max(purrr::map_dbl(df, ~ max(.x$variable, na.rm = TRUE)))
      brk_mpa <- brksREF_mpa#[[1]][2] # Get refugia breaks for each zone
      brk_eez <- brksREF_eez#[[1]][2]
      median_mpa <- medians_mpa %>% subset(Scenario == .y) %>% # Get median for each zone and each ssp
        pull(var = Median) # Pull the first 
      median_eez <- medians_eez %>% subset(Scenario == .y) %>% 
        pull(var = Median) # Pull the first 
      
      ### Palette for tos:
      # pal <- c("#0D383E", "#12A592") 
      pal <- c("#0D383E", "#EB8933")  # In a positive direction (tos only)
      
      term <- unique(df$Term)
      
      p <- ggplot(.x, aes(
        x = tos_ROC,
        fill = Zone)) +
        
        geom_rect(xmin = -Inf, xmax = brk_mpa, ymin = -Inf, ymax = Inf, # Refugia grey box in background
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + # Shade the recent term time period
        geom_rect(xmin = -Inf, xmax = brk_eez, ymin = -Inf, ymax = Inf,
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + # Shade the recent term time period
        geom_vline(xintercept = brk_mpa, # Refugial breaks for MPAs
                   linetype = "dashed", 
                   color =  pal[2],
                   linewidth = 0.5) +
        geom_vline(xintercept = brk_eez, # Refugial breaks for whole EEZ
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
        labs(title = paste0("Density plot of ", paste(var_nm[1], "_ROC"), " for ", .y),
             x = paste(var_nm[1], "_ROC"), y = "Density") +
        scale_fill_manual(values = pal) +
        scale_x_continuous(limits = c(min_val, max_val), labels = scales::label_number()) +
        ylim(c(0, 20)) +
        theme_minimal() +
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

      # ggsave(paste0(density_folder, "/density_plot_median_", term, "_orange_", .y, "_", var_nm[1], "_ROC.pdf"),
      #        p,
      #        width = 8,
      #        height = 6,
      #        dpi = 300)
    })
  }
  plot_density_tos(combined_ssp_dfs)
  
  df <- combined_ssp_dfs
  
  ## For pH and o2 -------
  plot_density_o2ph <- function(d) {

    imap(d, ~ {
      # Find the max and min of whole dataset to make x axes the same
          ##** Change the variable**
      min_val <- min(purrr::map_dbl(d, ~ min(
        # .x$o2_ROC,
        .x$ph_ROC,
        na.rm = TRUE)))
      max_val <- max(purrr::map_dbl(d, ~ max(
        # .x$o2_ROC,
        .x$ph_ROC,
        na.rm = TRUE)))
      brk_mpa <- brksREF_mpa[[1]][2]
      brk_eez <- brksREF_eez[[1]][2]
      median_mpa <- medians_mpa %>% subset(Scenario == .y) %>% 
        pull(var = Median) # Pull the first 
      median_eez <- medians_eez %>% subset(Scenario == .y) %>% 
        pull(var = Median) # Pull the first 
      
      ### Palette for ph, o2:
      # pal <- c("#2C8185", "#58DDC7") # In a negative direction (ph, o2 only)
      pal <- c("#0D383E", "#EB8933")  # In a positive direction (tos only)
      
      
      p <- ggplot(.x, aes(
        ##** Change the variable**
          # x = o2_ROC,
          x = ph_ROC,
          
        fill = Zone)) +
        geom_rect(xmin = brk_mpa,xmax = Inf, ymin = -Inf, ymax = Inf, # Refugia grey box in background
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + # Shade the recent term time period
        geom_rect(xmin = brk_eez, xmax = Inf, ymin = -Inf, ymax = Inf,
                  fill = "grey85",
                  colour = NA,
                  alpha = 0.08) + # Shade the recent term time period
        geom_vline(xintercept = brk_mpa, # Refugial breaks for MPAs
                   linetype = "dashed", 
                   colour = pal[2],
                   linewidth = 0.5) +
        geom_vline(xintercept = brk_eez, # Refugial breaks for whole EEZ
                   linetype = "dashed", 
                   colour = pal[1],
                   linewidth = 0.5) +
        geom_vline(xintercept = median_mpa, # Median for the MPAs
                   linetype = "solid", 
                   colour = pal[2],
                   linewidth = 0.6) +
        geom_vline(xintercept = median_eez, # Median for the EEZ
                   linetype = "solid", 
                   colour = pal[1],
                   linewidth = 0.6) +
        
        geom_density(alpha = 0.6) +
        labs(title = paste0("Density plot of ", paste(met_ttl, "_ROC"), " for ", .y),
          x = paste(met_ttl, "_ROC"), y = "Density") +
        scale_fill_manual(values = pal) +
        scale_x_continuous(limits = c(min_val, max_val), 
                           labels = scales::label_number()) +
        theme_minimal() +
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        ##** Change the variable**
        # ylim(c(0, 4000)) # For o2
        ylim(c(0, 1600)) # For pH
      
      
      ggsave(paste0(density_folder, "/density_plot_median_mid-term_orange_", .y, "_", met_ttl, "_", metric, ".pdf"),
             p,
             width = 8,
             height = 6,
             dpi = 300)
      })
    }
  plot_density_o2ph(combined_ssp_dfs)
  
