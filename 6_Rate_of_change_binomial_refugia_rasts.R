# Create binary rasts for plotting refugia/non-refugia (1/0)
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

  per <- 0.3 
  
  
  
# Folders and background data --------------------------------------------------
  
  infol <- make_folder(source_disk, "ROC", var_nm, "threat_layers1") 
  ssp_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("1_", var_nm, "_ssp_splits"))
  binomial_ssp_fol <- make_folder(source_disk, "ROC", "binomial_layers", paste0("2_", var_nm, "_ssp_binomial-refugia"))
  # plots_fol <- make_folder(source_disk, "ROC", "binomial_layers", "plots")
  

  
# Split the rasterbricks into individual files ---------------------------------
  
  eez_stack <- readRDS(paste0(infol, "/", var_nm, "_ROC_eez_stack.RDA"))
  mpa_stack <- readRDS(paste0(infol, "/", var_nm, "_ROC_mpa_stack.RDA"))
  mpaoutside_stack <- readRDS(paste0(infol, "/", var_nm, "_ROC_outsidempa_stack.RDA"))
  
  brksREF_eez <- readRDS(paste0(infol, "/", var_nm, "_ROC_eez_refugia_breaks_", per*100, "per.RDA"))
  brksREF_mpa <- readRDS(paste0(infol, "/", var_nm, "_ROC_mpa_refugia_breaks_", per*100, "per.RDA"))
  brksREF_mpaoutside <- readRDS(paste0(infol, "/", var_nm, "_ROC_outsidempa_refugia_breaks_", per*100, "per.RDA"))
  
  
  
  ## Split rasterbricks by SSP for 1995-2100 -------------
  
  splitbricks_by_ssp <- function(list) {
    map(list, ~ {
      brick <- .x
      spat_brick <- as(brick, "RasterBrick")
      
      ssp <- str_split_i(names(brick)[1], "_", 4) %>% unique()
      filename <- paste0(ssp_fol, "/ROC_", var_nm, "-refugia_",
                         ssp, "_1995-2100_",
                         per*100, "per", ".RDA")
      saveRDS(spat_brick, filename)
    })
  }
  splitbricks_by_ssp(eez_stack)
  
  
  
# Create binomial rasters for each individual file -----------------------------

  brks <- unlist(brksREF_eez) 
  names(brks) <- gsub("\\.30%", "", names(brks)) 
  d <- as.data.frame(brks) %>% 
    filter(brks != "-Inf" & brks != "Inf") 
  brks <- d$brks[[1]]

  
  get_binomial <- function(f) {
    r <- readRDS(f) 
    r_rast <- terra::rast(r)
    extent <- terra::ext(105, 175, -50, -5)
    r_cropped <- terra::crop(r_rast, extent)
    r_masked <- terra::mask(r_cropped, eez) 

    # Relate data to the break for refugia
    if(var_nm[1] == "tos") {
      rr <- r_masked <= brks # For positive direction (tos)
    } else {
      rr <- r_masked >= brks # For negative variables (o2, pH)
    }
    return(rr)
    rr <- terra::ifel(rr, 1, 0) # Make it binary instead of T/F
    
    ssp <- str_split_i(names(r), "_", 4) %>% unique()
    filename <- paste0(binomial_ssp_fol, "/ROC_", var_nm, "-binomial-refugia_eez_", ssp, "_", per*100, "per", ".nc")
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
    