# Background data
  # Written by Alice Pidd (alicempidd@gmail.com)
    # June 2023



# Source the helpers and necessary bits -----------------------------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data"
  

# Source data and shapefiles  --------------------------------------------------
  
## Create a spatraster with the correct extent and resolution ----
  
  e1 <- ext(105, 175, -50, -5) # Extent
  base_r <- rast(ext = e1, res = 0.25) # Base raster for resampling and cropping

  
## Get a shapefile of the World and crop to Australia ----

  data("World")
  oceaniaAsia <- World %>%
    filter(continent %in% c("Oceania", "Asia")) %>%
    dplyr::select(area)
  bbox_e1 <- st_as_sfc(st_bbox(c(xmin = 108, xmax = 175, ymin = -50, ymax = -5), crs = st_crs(oceaniaAsia)))
  oceaniaAsia <- st_crop(oceaniaAsia, bbox_e1)
  saveRDS(oceaniaAsia, "/Volumes/AliceShield/clim_data/masks/landsea_masks/oceaniaAsia_shapefile.RDS")
  # plot(oceaniaAsia)

  oceaniaAsiaAnt <- World %>%
    filter(continent %in% c("Oceania", "Asia", "Antarctica")) %>%
    dplyr::select(area)
  # plot(oceaniaAsia)

  australia <- World %>%
    filter(name %in% c("Australia")) %>%
    dplyr::select(area)
  bbox_e1 <- st_as_sfc(st_bbox(c(xmin = 108, xmax = 175, ymin = -50, ymax = -5), crs = st_crs(australia)))
  australia <- st_crop(australia, bbox_e1)
  saveRDS(australia, "/Volumes/AliceShield/clim_data/masks/landsea_masks/australia_shapefile.RDS")
  # plot(australia)
  

## Get the Australian EEZ shapefile ----

  e <- get_shps(paste0(source_disk, "/shapefiles/au_eez_pol_april2022.shp"))
  # plot(e)
  eez <- e %>% slice(c(3, 5)) # Get only the features we want (contiguous EEZ, Lord Howe, Norfolk - i.e., NOT cocos keeling/christmas/macquarie)
  eez <- eez %>%
    dplyr::select(MRGID, GeoName, Area_km2, geometry) %>%
    mutate(OBJECTID = MRGID,
           NETNAME = GeoName,
           IUCN = "eez",
           AREA_KM2 = Area_km2) %>%
    dplyr::select(-c(1:3))
  # plot(eez$geometry)
  saveRDS(eez, paste0("/Volumes/AliceShield/clim_data/masks/landsea_masks", "/EEZ_shapefile.RDS"))



## Get the MPA shapefile that has all networks ----

  mpaAll_geom <- get_shps(paste0(source_disk, "/shapefiles/AustralianMarineParks.shp")) %>%
    filter(NETNAME != "HIMI" & NETNAME != "Indian Ocean Territories") %>%
    mutate(OBJECTID = 1:nrow(.)) %>% # Make new column with rows numbered
    dplyr::rename("IUCN" = "ZONEIUCN") %>%
    dplyr::select(OBJECTID, NETNAME, IUCN, AREA_KM2, geometry) # select only these two columns



## Get the GBR shapefile ----

  # GBR_geom <- get_shps(paste0(source_disk, "/shapefiles/Great_Barrier_Reef_coast_marine_park_zoning.shp")) %>%
  GBR_geom <- st_read(paste0(source_disk, "/shapefiles/Great_Barrier_Reef_Marine_Park_Zoning.shp")) %>%
    mutate(NETNAME = "GBR") %>%
    mutate(AREA_KM2 = "Unknown") %>%
    dplyr::select(OBJECTID, NETNAME, IUCN, AREA_KM2, geometry) %>% # Select only necessary elements
    st_cast(., "POLYGON") # Turn the multipolygon into a polygon

  extent(GBR_geom) # xmin 142.5315, xmax 154.001, ymin -24.4985, ymax -10.68189, we're all good.
  GBR_geom <- sf::st_make_valid(GBR_geom) # There are some invalid values, need to make them valid
  sf::st_is_valid(GBR_geom) # Check it worked - if it did, there will be no FALSE's


## Join the MPA and GBR shapefiles together ----

      joined_shps <- rbind(mpaAll_geom, GBR_geom) # Join the two

    # Get IUCN V and VI - the ones we don't include as MPAs in Chapter 2
      IUCN_V_VI_shps <- joined_shps %>%
      filter(IUCN == "V" | IUCN == "VI")
      # plot(IUCN_V_VI_shps$geometry)
      saveRDS(IUCN_V_VI_shps, "/Volumes/AliceShield/clim_data/masks/landsea_masks/IUCN-category_V-VI_shapefile.RDS")


    # Get all the MPAs joined and exclude IUCN category V and VI
      joined_shps <- joined_shps %>%
        mutate(NETNAME = ifelse(NETNAME %in% c("Coral Sea", "GBR"), "Coral Sea and GBR", NETNAME)) %>%
        filter(IUCN != "V" & IUCN != "VI") %>% # Get rid of categories that allow for fishing
        mutate(IUCN = if_else(IUCN == "Ia", "IA", IUCN)) # Make naming consistent

      extent(joined_shps) # Checking the extent
      projection(joined_shps)
      # plot(joined_shps$geometry)
      saveRDS(joined_shps, "/Volumes/AliceShield/clim_data/masks/landsea_masks/joined_shps_shapefile.RDS")


# Make shapefile for outside MPAs, i.e., IUCN category V-VI mpas + the remaining EEZ outside of cat I-IV mpas
      # EEZ - Joined_shps



# # Individual networks ------------
      
#     southeast <- joined_shps %>%
#       subset(NETNAME == "South-east")
#     
#     north <- joined_shps %>%
#       subset(NETNAME == "North")
#     
#     northwest <- joined_shps %>%
#       subset(NETNAME == "North-west")
#     
#     CSandGBR <- joined_shps %>%
#       subset(NETNAME == "Coral Sea" | NETNAME == "GBR")
#     
#     southwest <- joined_shps %>%
#       subset(NETNAME == "South-west")
#     
#     tempeast <- joined_shps %>%
#       subset(NETNAME == "Temperate East")
    # 
    # netw_list <- as.list(north, CSandGBR, tempeast, southeast, southwest, northwest)
    # netw_list <- list("north", "CSandGBR", "tempeast", "southeast", "southwest", "northwest")

      
# Create rasters out of the polygons using fasterize, and crop/regrid to base_r --------

  eez <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/EEZ_shapefile.RDS")
  joined_shps <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/joined_shps_shapefile.RDS")
  oceaniaAsia <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/oceaniaAsia_shapefile.RDS")
  IUCN_V_VI_shps <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/IUCN-category_V-VI_shapefile.RDS")
  outsideMPAs <- readRDS("/Volumes/AliceShield/clim_data/masks/landsea_masks/outsideMPAs_shapefile.RDS")
  # plot(eez$geometry)
  
  
  rmpa <- terra::rasterize(joined_shps, base_r) # Base raster for all MPAs, including GBR
    ##**NOTE: this doesn't have any info in it except the extent and res etc*
  reez <- terra::rasterize(eez, base_r) # Base raster for just MPAs
    ## Mask with Australia
    aus <- rast("/Volumes/AliceShield/clim_data/masks/landsea_masks/mask_landsea_NAs.nc")
    # plot(aus)
    reez <- mask(reez, aus)
    # plot(reez)
  
  routsidempa <- terra::rasterize(outsideMPAs, base_r)

# SSP naming and ordering ------------------------------------------------------
  
  ssp_num <- list("ssp126", "ssp245", "ssp370", "ssp585") # SSP names
  stack_order <- c("recent", "present", "near", "mid", "intermediate", "long")
  