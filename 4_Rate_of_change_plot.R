# Plot rates of change, and refugia
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  var_nm <- tos[1]
  # var_nm <- ph[1]
  # var_nm <- o2[1]

  
  
# Folders and background data --------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm[1], "threat_layers1") # Raster stacks per SSP
  metricplots_fol <- make_folder(dest_disk, "ROC", var_nm[1], "plots_metric1")
  refplots_fol <- make_folder(dest_disk, "ROC", var_nm[1], "plots_refugia1") # Where refugia plots will go
  
  
  
# Plotting data ----------------------------------------------------------------
  
  per <- 0.3
  
  eez_stack <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_eez_stack.RDA"))
  mpa_stack <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_mpa_stack.RDA"))
  mpaoutside_stack <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_outsidempa_stack.RDA"))
  
  brks_eez <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_eez_breaks.RDA"))
  brks_mpa <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_mpa_breaks.RDA"))
  brks_mpaoutside <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_outsidempa_breaks.RDA"))

  brksREF_eez <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_eez_refugia_breaks_", per*100, "per.RDA"))
  brksREF_mpa <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_mpa_refugia_breaks_", per*100, "per.RDA"))
  brksREF_mpaoutside <- readRDS(paste0(infol, "/", var_nm[1], "_ROC_outsidempa_refugia_breaks_", per*100, "per.RDA"))
  
  
  
# Plot metric and save ---------------------------------------------------------
  
  ## Specify palette direction depending on variable -------
  
    if(var_nm[1] == "tos") {
      col_pal <- rev(RColorBrewer::brewer.pal("RdBu", n = 11)) # For positive direction (tos)
    } else {
      col_pal <- RColorBrewer::brewer.pal("RdBu", n = 11) # For negative variables (o2, pH)
    }
    col_pal
  

  ## Plot  -------
    
    plot_metric <- function(x, region, region_nm, brks, pal) {
      n_layers <- nlayers(x)
      x_masked <- terra::mask(x, region) # Mask data to whole EEZ
      
      p <- tm_shape(x_masked) + 
        tm_raster(col.scale = tm_scale(values = pal,
                                       breaks = brks,
                                       midpoint = 0),
                  col.legend = tm_legend(title = paste0("Rate of change in ", var_nm[1], " ", var_nm[3], " per decade"))) +
        tm_shape(aus_shp) +
        tm_fill(fill = "grey45") +
        tm_shape(oceania_shp) +
        tm_fill(fill = "grey45") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_facets(nrow = n_layers, 
                  free.coords = FALSE,  # Keep coordinates consistent
                  sync = TRUE) +       # Synchronize the maps
        tm_layout(frame = FALSE,  # Remove outer border
                  inner.margins = c(0.02, 0.02, 0.02, 0.02))  # Adjust margins
      
      nm <- str_split_i(names(x)[1], "_", 4) %>%
        paste0(metricplots_fol, "/", var_nm[1], "_ROC_METRICplot_oceania_", region_nm, "RdBu_", ., ".pdf") 
      
      # Save with explicit dimensions - make it wider to accommodate legend
      tmap_save(p, nm, width = 14, height = 10, units = "in")
    }
    
    map(eez_stack, ~ plot_metric(.x, eez, "eez", brks_eez, col_pal)) 
    map(mpa_stack, ~ plot_metric(.x, MPA_shp, "mpa", brks_mpa, col_pal))
    map(mpaoutside_stack, ~ plot_metric(.x, outsideMPA_shp, "outsidempa", brks_mpa, col_pal))
  
  
  
# Plot refugia and save ---------------------------------------------------------
  
  ## Specify legend labels and palettes per variable -------
  
    if(var_nm[1] == "tos") {
      legend_labs <- c(paste0("Refugia (> ", per*100, "% change)"),
                       paste0("Non-refugia (≤ ", per*100, "% change)")) # For positive direction (tos)
    } else {
      legend_labs <- c(paste0("Non-refugia (> ", per*100, "% change)"),
                       paste0("Refugia (≤ ", per*100, "% change)")) # For negative variables (o2, pH)
    }
    legend_labs
    
    
    if(var_nm[1] == "tos") {
      m_pal <- c("#EA7A0B", "#EBB65C")  # In a positive direction (tos only)
      e_pal <- c("#086788", "#A0DAE4")  # In a positive direction (tos only)
    } else {
      m_pal <- c("#EBB65C", "#EA7A0B") # In a negative direction (ph, o2)
      e_pal <- c("#A0DAE4", "#086788") # In a negative direction (ph, o2)  
    }
    e_pal
    m_pal
  
  
  ## Plot -------
    
    plotref_dif <- function(m, e) {
      n_layers_mpa <- nlayers(m)
      x_masked_mpa <- terra::mask(m, MPA_shp)
      
      n_layers_eez <- nlayers(e)
      x_masked_eez <- terra::mask(e, outsideMPA_shp) 
      
      p_eez <- tm_shape(e) +
        tm_raster(col.scale = tm_scale(values = e_pal,
                                       breaks = brksREF_mpaoutside[[1]],
                                       # breaks = brksREF_eez[[1]],
                                       labels = legend_labs),
                  col.legend = tm_legend(title = paste0("Rate of change in ", var_nm[1], " ", var_nm[3], " per decade"))) +
        tm_shape(aus_shp) +
        tm_fill(fill = "grey60") +
        tm_shape(oceania_shp) +
        tm_fill(fill = "grey60") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_facets(nrow = n_layers_eez, sync = TRUE) +
        tm_layout(frame = FALSE,  # Remove outer border
                  inner.margins = c(0.02, 0.02, 0.02, 0.02))  # Adjust margins
      
      p_mpa <- p_eez +
        tm_shape(m) +
        tm_raster(col.scale = tm_scale(values = m_pal,
                                       breaks = brksREF_mpa[[1]],
                                       labels = legend_labs),
                  col.legend = tm_legend(title = paste0("Rate of change in ", var_nm[1], " ", var_nm[3], " per decade"))) +
        tm_shape(aus_shp) +
        tm_fill(fill = "grey60") +
        tm_shape(oceania_shp) +
        tm_fill(fill = "grey60") +
        tm_shape(eez) +
        tm_borders(col = "black", lwd = 0.5) +
        tm_facets(nrow = n_layers_mpa, sync = TRUE) +
        tm_layout(frame = FALSE,  # Remove outer border
                  inner.margins = c(0.02, 0.02, 0.02, 0.02))  # Adjust margins
      
      arranged <- tmap_arrange(p_mpa, ncol = 1, widths = 0.4)
      nm <- str_split_i(names(m)[1], "_", 4) %>%
        paste0(refplots_fol, "/", var_nm[1], "_ROC_REFplot_dif-mpaeez-quadcolour_oceania_", ., "_", per*100, "per",".pdf")
      
      # Save with explicit dimensions
      tmap_save(arranged, nm, width = 14, height = 10, units = "in")
    }
    
    map2(mpa_stack, mpaoutside_stack, ~ plotref_dif(.x, .y))    
    
    