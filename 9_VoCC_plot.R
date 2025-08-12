# Plot rates of change, and refugia
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023


# Source data and set dirs -----------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  dest_disk <- "/Volumes/AliceShield/clim_data" # Where files are written to
  source("Background_plotting_data.R")
  
  

# Metric -----------------------------------------------------------------------
  
  var_nm <- VoCC
  
  
  
# Folders and background data --------------------------------------------------
  
  infol <- make_folder(source_disk, "VoCC", var_nm[1], "threat_layers1") # Raster stacks per SSP
  metricplots_fol <- make_folder(dest_disk, "VoCC", var_nm[1], "plots_metric1")
  refplots_fol <- make_folder(dest_disk, "VoCC", var_nm[1], "plots_refugia1") # Where refugia plots will go
  
  
  
# Plotting data ----------------------------------------------------------------
  
  per <- 0.3
  
  eez_stack <- readRDS(paste0(infol, "/", var_nm[1], "_eez_stack.RDA"))
  mpa_stack <- readRDS(paste0(infol, "/", var_nm[1], "_mpa_stack.RDA"))
  mpaoutside_stack <- readRDS(paste0(infol, "/", var_nm[1], "_outsidempa_stack.RDA"))
  
  brks_eez <- readRDS(paste0(infol, "/", var_nm[1], "_eez_breaks.RDA"))
  brks_mpa <- readRDS(paste0(infol, "/", var_nm[1], "_mpa_breaks.RDA"))
  brks_mpaoutside <- readRDS(paste0(infol, "/", var_nm[1], "_outsidempa_breaks.RDA"))

  brksREF_eez <- readRDS(paste0(infol, "/", var_nm[1], "_eez_refugia_breaks_", per*100, "per.RDA"))
  brksREF_mpa <- readRDS(paste0(infol, "/", var_nm[1], "_mpa_refugia_breaks_", per*100, "per.RDA"))
  brksREF_mpaoutside <- readRDS(paste0(infol, "/", var_nm[1], "_outsidempa_refugia_breaks_", per*100, "per.RDA"))
  
  
  
# Plot metric and save ---------------------------------------------------------
  
  col_pal <- rev(RColorBrewer::brewer.pal("RdBu", n = 11))
  
    
  ## Plot  -------
    
  plot_metric <- function(x, region, region_nm, brks, pal) {
    n_layers <- nlayers(x)
    xx <- clamp(x, lower = -50, upper = 150)
    x_masked <- terra::mask(xx, region) # Mask data to whole EEZ
    
    p <- tm_shape(x_masked) + 
      tm_raster(col.scale = tm_scale(values = pal,
                                     breaks = seq(-50, 150, 5),
                                     midpoint = 0),
                col.legend = tm_legend(title = paste0("Rate of change in ", var_nm[1], " ", var_nm[3], " per decade"))) +
      tm_shape(aus_shp) +
      tm_fill(fill = "grey45") +
      tm_shape(oceania_shp) +
      tm_fill(fill = "grey45") +
      tm_shape(eez) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_facets(nrow = n_layers, sync = TRUE) +
      tm_layout(frame = FALSE,
                inner.margins = c(0.01, 0.01, 0.01, 0.01),
                outer.margins = c(0.01, 0.01, 0.01, 0.01),
                between.margin = 0.01,
                legend.outside = TRUE,
                legend.outside.position = "right",
                legend.outside.size = 0.15)
    
    nm <- str_split_i(names(x)[1], "_", 4) %>%
      paste0(metricplots_fol, "/", var_nm[1], "_ROC_METRICplot_", region_nm, "RdBu_", ., ".pdf") 
    
    # Save with explicit dimensions and remove tmap_arrange
    tmap_save(p, nm, width = 14, height = 10, units = "in")
  }
    
    map(eez_stack, ~ plot_metric(.x, eez, "eez", brks_eez, col_pal)) 
    map(mpa_stack, ~ plot_metric(.x, MPA_shp, "mpas", brks_mpa, col_pal))
    map(mpaoutside_stack, ~ plot_metric(.x, outsideMPA_shp, "outsidempas", brks_mpa, col_pal))
  
  
  
# Plot refugia and save ---------------------------------------------------------
  
    m_pal <- c("#EA7A0B", "#EBB65C")  # In a positive direction (tos only)
    e_pal <- c("#086788", "#A0DAE4")  # In a positive direction (tos only)
    

  ## Plot -------
    
  plotref_dif <- function(m, e) {
    n_layers_mpa <- nlayers(m)
    # x_masked_mpa <- terra::mask(m, MPA_shp)
    x_masked_mpa <- m
    
    n_layers_eez <- nlayers(e)
    # x_masked_eez <- terra::mask(e, outsideMPA_shp) 
    x_masked_eez <- e 

    p_eez <- tm_shape(e) +
      tm_raster(col.scale = tm_scale(values = e_pal,
                                     breaks = brksREF_mpaoutside[[1]],
                                     labels = c(paste0("Refugia (> ", per*100, "% change)"),
                                                paste0("Non-refugia (≤ ", per*100, "% change)")))) +
      tm_shape(aus_shp) +
      tm_fill(fill = "grey60") +
      tm_shape(oceania_shp) +
      tm_fill(fill = "grey60") +
      tm_shape(eez) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_facets(nrow = n_layers_eez, sync = TRUE) +
      tm_layout(frame = FALSE,
                inner.margins = c(0.01, 0.01, 0.01, 0.01))
    
    p_mpa <- p_eez +
      tm_shape(m) +
      tm_raster(col.scale = tm_scale(values = m_pal,
                                     breaks = brksREF_mpa[[1]],
                                     labels = c(paste0("Refugia (> ", per*100, "% change)"),
                                                paste0("Non-refugia (≤ ", per*100, "% change)")))) +
      tm_shape(aus_shp) +
      tm_fill(fill = "grey60") +
      tm_shape(oceania_shp) +
      tm_fill(fill = "grey60") +
      tm_shape(eez) +
      tm_borders(col = "black", lwd = 0.5) +
      tm_facets(nrow = n_layers_mpa, sync = TRUE) +
      tm_layout(frame = FALSE,
                inner.margins = c(0.01, 0.01, 0.01, 0.01))
    
    nm <- str_split_i(names(m)[1], "_", 4) %>%
      paste0(refplots_fol, "/", var_nm[1], "_REFplot_dif-mpaeez-quadcolour_oceania_", ., "_", per*100, "per",".pdf")
    
    # Save with explicit dimensions and remove tmap_arrange
    tmap_save(p_mpa, nm, width = 14, height = 10, units = "in")
  }
  
  map2(mpa_stack, mpaoutside_stack, ~ plotref_dif(.x, .y))
  
  
    