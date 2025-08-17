# Calculating the proportion of each latitudinal band that is within an MPA
  # Written by Alice Pidd (alicempidd@gmail.com)
    # August 2025


# Source workspace data and set disk dirs --------------------------------------

  source("Helpers.R")
  dest_disk <- "/Volumes/AliceShield/clim_data"
  source("Background_plotting_data.R")
  
  
  
# Folders and metrics ----------------------------------------------------------

  mask_folder <- make_folder(dest_disk, "masks", "", "")
  fig_folder <- make_folder(dest_disk, "figures", "latitudinal_MPA-split", "")

  aus_shp <- readRDS(paste0(disk, "/aus_shapefile.RDS"))
  MPA_shp <- readRDS(paste0(disk, "/joined_shps_shapefile.RDS"))

  

# Calculate area for the different regions -------------------------------------
  
## Area for MPAs
  mask_mpa <- paste0(mask_folder, "/mask_MPAs_NAs.nc") %>% 
    rast() # Call the mpa mask
  plot(mask_mpa)
  area_mpa <- terra::cellSize(mask_mpa, unit = "km") # Gives a spatraster
  total_area_mpa <- sum(values(area_mpa)[!is.na(values(mask_mpa))]) # Sum the values, without including NAs. 
  total_area_mpa # Should be 2520930 km^2 (pre-review 2303489 km^2)
  


  
# Calculate proportion of the MPA network that is in each band of latitude -----

  # Latitude bands
  lat_bands <- seq(-50, -5, by = 1) # By 1 degree
  
  # Make empty list to store results
  lat_proportions_list <- list()
  
  # Calc the proportion of area covered by 1s in the mask for each latitude band specified earlier
  for (i in 1:(length(lat_bands) - 1)) {
    ymin <- lat_bands[i]
    ymax <- lat_bands[i + 1]
    
    cropped_mask <- crop(mask_mpa, ext(105, 175, ymin, ymax)) # Get only the current latitude band
    
    ## Get the pixel area in km² (this gives us the correct area per pixel)
      pixel_area <- cellSize(cropped_mask, unit = "km")  # Area in km²
      pixel_area_value <- values(pixel_area)[1]  # Extract the numeric value (assuming uniform pixel size)
    
    ## Calc the area where the mask value is 1 (MPA areas)
      mask_area <- sum(values(cropped_mask) == 1, na.rm = TRUE) * pixel_area_value
    
    ## Calc the proportion of MPA area covered by 1s relative to total MPA area
      proportion <- mask_area / total_area_mpa
    
    ## Add the results to the list
      lat_proportions_list[[i]] <- tibble(
        Latitude_Band = paste0("Lat_", ymin, "_to_", ymax),
        Proportion = proportion
      )
  }
  
  # Combine all into a tibble
  lat_proportions_df <- bind_rows(lat_proportions_list)
  lat_proportions_df  
  sum(lat_proportions_df$Proportion) 
  
  
  
 # Plot it ---------------------------------------------------------------------
  
  # For 5 degree bins
  ggplot(lat_proportions_df, aes(y = Latitude_Band, x = Proportion)) +
  geom_bar(stat = "identity", 
           fill = "#EB8933",
           alpha = 0.9) +  # Bar color and transparency
  labs(x = "Proportion of MPA Area", y = "Latitude Band") +
  # scale_x_continuous(labels = scales::label_percent(scale = 1)) +  # Format x-axis as percentage
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Rotate y-axis labels if necessary
  
  ggsave(
    # paste0(fig_folder, "/MPA_proportion_per_5deg_latitude_bar.pdf"),
    paste0(fig_folder, "/MPA_proportion_per_1deg_latitude_bar.pdf"),
         width = 8,
         height = 6,
         dpi = 300)  
  

  ggplot(lat_proportions_df, aes(x = Latitude_Band, y = Proportion, group = 1)) +
    geom_line(colour = "#EB8933",
             size = 2,
             alpha = 0.9) +  # Use "identity" to directly map the y values
    labs(x = "Latitude Band", y = "Proportion of MPA Area") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate the x labels to be vertical

  ggsave(paste0(fig_folder, "/MPA_proportion_per_1deg_latitude_line.pdf"),
         width = 8,
         height = 6,
         dpi = 300)
  

  
# Plot Australia with lat and lon for comparison
  
  # Create the plot
  ggplot() +
    geom_sf(data = MPA_shp, fill = "#EB8933", colour = "#EB8933") +
    geom_sf(data = aus_shp, fill = "black", color = "black") +
    theme_minimal()
  
  ggsave(paste0(fig_folder, "/australia_MPAs_and_latitudes.pdf"),
         width = 8,
         height = 6,
         dpi = 300)


  
  