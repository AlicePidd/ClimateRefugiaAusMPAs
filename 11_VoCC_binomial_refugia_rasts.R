# Create binary rasts for plotting refugia/non-refugia (1/0)
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
  
  infol <- make_folder(source_disk, "VoCC", var_nm, "threat_layers1")
  ssp_fol <- make_folder(source_disk, "VoCC", "binomial_layers", paste0("1_", var_nm, "_ssp_splits"))
  binomial_ssp_fol <- make_folder(source_disk, "VoCC", "binomial_layers", paste0("2_", var_nm, "_ssp_binomial-refugia"))
  plots_fol <- make_folder(source_disk, "VoCC", var_nm, "plots_binomial1")
  
  per <- 0.3 
  
  eez_stack <- readRDS(paste0(infol, "/", var_nm, "_eez_stack.RDA"))
  brks <- readRDS(paste0(infol, "/", var_nm, "_eez_refugia_breaks_", per*100, "per.RDA")) %>% 
    unlist() %>% as.data.frame(.) %>% .[2, ]

  
  
# Split rasterbricks by SSP for 1995-2100 -------------
  
  splitbricks_by_ssp <- function(list) {
    map(list, ~ {
      brick <- .x
      spat_brick <- as(brick, "RasterBrick")
      
      ssp <- str_split_i(names(brick)[1], "_", 4) %>% unique()
      filename <- paste0(ssp_fol, "/", var_nm, "-refugia_",
                         ssp, "_1995-2100_",
                         per*100, "per", ".RDA")
      saveRDS(spat_brick, filename)
    })
  }
  splitbricks_by_ssp(eez_stack)
  
  
  
# Create binomial rasters for each individual file -----------------------------

  get_binomial <- function(f) {
    r <- readRDS(f) 
    r_rast <- terra::rast(r)
    e <- terra::ext(105, 175, -50, -5)
    r_cropped <- terra::crop(r_rast, e)
    r_masked <- terra::mask(r_cropped, eez) 

    rr <- r_masked <= brks # For positive direction (tos)
    rr <- terra::ifel(rr, 1, 0) # Make it binary instead of T/F
    names(rr) <- names(r_masked)
    
    ssp <- str_split_i(names(r), "_", 4) %>% unique()
    filename <- paste0(binomial_ssp_fol, "/", var_nm, "-binomial-refugia_eez_", ssp, "_", per*100, "per", ".nc")
    terra::writeCDF(rr, filename, overwrite = TRUE) 
  }
  
  files <- dir(ssp_fol, full.names = TRUE) 
  files
  walk(files, get_binomial)
  

  ## Eyeball the plots ------
    
    files <- dir(binomial_ssp_fol, full.names = TRUE)
    files
    plot_comb <- function(f) {      
      r <- rast(f)
      plot(r)
      title(main = basename(f))
    }
    map(files, plot_comb)
    