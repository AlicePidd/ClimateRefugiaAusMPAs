# Helper functions for Pidd et al. (2025). Climate refugia could disappear from Australia’s marine protected areas by 2040. 
	# For working with CMIP6 netCDFs
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023

install.packages("usethis")



# Packages ---------------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(stringr)
library(raster)
library(terra)
library(sf)
library(tidyr)
# library(ggplot2)
library(viridis)
library(tmap) # Retiring!
library(tmaptools)
library(RColorBrewer)
library(lubridate)
library(ncdf4)
library(fasterize)
library(purrr)
library(furrr)
library(future)
library(tictoc)
library(beepr)

# library(PCICt)
	# library(heatwaveR)

	# library(rnaturalearth)
# library(rnaturalearthhires)
# library(VoCC) # Not available because raster has been deprecated
	# library(scico)
	# library(gganimate)
# library(plyr)
  # library(data.table)
  # library(xts)

  # library(ggridges)
# library(PNWColors)
# library("wesanderson")
# library(MoMAColors)

	# IPCC Periods ------------------------------------------------------------

	recent_term <- 1995:2014
	near_term <- 2021:2040
	mid_term <- 2041:2060
	intermediate_term <- 2061:2080
	long_term <- 2081:2100
	periods <- c("recent_term", "near_term", "mid_term", "intermediate_term", "long_term")
	
	
# IPCC colours as hex ------------------------------------------------------------

	col_ssp119 <- rgb(84, 39, 143, maxColorValue = 255)
	col_ssp126 <- rgb(0, 52, 102, maxColorValue = 255)
	col_ssp245 <- rgb(112, 160, 205, maxColorValue = 255)
	col_ssp370 <- rgb(196, 121, 0, maxColorValue = 255)
	col_ssp534_over <- rgb(196, 121, 0, maxColorValue = 255)
	col_ssp585 <- rgb(153, 0, 2, maxColorValue = 255)

	IPCC_pal <- c(col_ssp126, col_ssp245, col_ssp370, col_ssp585)

# Palettes ---------------------------------------------------------------------
	
	# col_pal_ref <- c("#007acc", "orange")  # Binary palette (refugia, non-refugia)
	# col_pal_ref <- c("#205295", "orange")  # Binary palette (refugia, non-refugia)
	col_pal_ref <- c("#19A7CE", "#EE9322")  # Binary palette (refugia, non-refugia)
	
	# col_pal_ref <- c("#1D5B79", "#F6635C")  # Binary palette (refugia, non-refugia)
	# col_pal_ref <- c("#1450A3", "#FFC436")  # Binary palette (refugia, non-refugia)
	# col_pal_ref <- c("#19A7CE", "#FED049")  # Binary palette (refugia, non-refugia)
	# col_pal_ref <- c("#0079FF", "#F2BE22")  # Binary palette (refugia, non-refugia)
	
		
# Standard map palette ------------------------------------------------------------------------

	map_palette <- scico(11, alpha = NULL, begin = 0, end = 1, direction = 1, palette = "bilbao")
	
	
	
# Metric labels for plotting ----------------------------------------------
	
	tos <- c("tos", "temperature of surface (°C)")
	o2 <- c("o2", "dissolved oxygen concentration (units)")
	ph <- c("ph", "pH (mol H Kg¯¹)")
	chl <- c("chl", "Mass Concentration of Total Phytoplankton in Sea Water")
	VoCc <- c("VoCC", "Gradient-based climate velocity (km/decade)")
	mhw <- c("MHW_CumInt", "Marine heatwave cumulative intensity")
	mhwROC <- c("MHW-ROC", "Rate of change in marine heatwave cumulative intensity")
	

# Refugia/Non-refugia - Background layers/munging ------------------------------
	#** Alice's**
	percentiles <- seq(0.1, 0.9, 0.025) # A sequence of breaks from 0.1-0.9 (10-90%) at .1 intervals (2.5%)
	
	
	
# Get bits of CMIP6 file names -------------------------------------------------

	get_CMIP6_bits_dave <- function(file_name) {
	  bits <- str_split(basename(file_name), "_") %>% 
	    unlist()
	  date_start_stop <- bits[7] %>% 
	    str_split("[.]") %>%
	    map(1) %>% 
	    unlist() %>% 
	    str_split("-") %>%
	    unlist()
	  if(str_detect(file_name, "_.mon_")) {
	    date_start_stop <- paste0(date_start_stop, c("01", "31"))
	  } # Fix dates for monthly data
	  # if(str_detect(file_name, "_.year_")) {
	  if(str_detect(file_name, "_.day_")) {
	    date_start_stop <- paste0(date_start_stop, c("0101", "1231"))
	  } # Fix dates for annual data
	  date_start_stop <- as.Date(date_start_stop, format = "%Y%m%d")
	  output <- list(Variable = bits[1],
	                 Frequency = bits[2],
	                 Model = bits[3],
	                 Scenario = bits[4],
	                 Variant = bits[5],
	                 Grid = bits[6],
	                 Year_start = date_start_stop[1],
	                 Year_end = date_start_stop[2])
	  return(output)
	  # e.g., map_df(dir(folder), get_CMIP6_bits)
	}
	
	
    # file_name_test <- "ph_Omon_CMCC-ESM2_ssp126_r1i1p1f1_crop_201501-203412.nc"
    	#**Alice edits - made it call from basename not filepath, and made the date format generalisable*
    	# get_CMIP6_bits <- function(file_name, f_type) { # f_type is the frequency e.g. Omon, Oday
    	#   bits <- strsplit(basename(file_name), "_") %>% # Had to put basename here
    	#     unlist()
    	#   
    	#   # # Added by chatGPT
    	#   # print(bits)
    	# 
    	#   year_start <- substr(bits[7], 1, 4)
    	#   month_start <- substr(bits[7], 5, 6)
    	#   year_end <- substr(bits[7], 8, 11)
    	#   month_end <- substr(bits[7], 12, 13)
    	#   
    	#   start_date <- paste0(year_start, "", month_start)
    	#   end_date <- paste0(year_end, "", month_end)
    	#   
    	#   output <- list(Variable = bits[1],
    	#                  Frequency = bits[2],
    	#                  Model = bits[3],
    	#                  Scenario = bits[4],
    	#                  Run = bits[5],
    	#                  Grid = bits[6],
    	#                  # Year_start = paste(year_start, month_start, sep = ""),
    	#                  # Year_end = paste(year_end, month_end, sep = "")
    	#                  Year_start = start_date,
    	#                  Year_end = end_date)
    	#   return(output)
    	#   
    	#   # e.g., map_df(dir(folder), get_CMIP6_bits)
    	# }
    	# 
    	# 
    	# 
    	# # Long variation for when dates are in YYYYMMDD instead of just YYYYMM like the original function
    	# get_CMIP6_bits_long <- function(file_name, f_type) { # f_type is the frequency e.g. Omon, Oday
    	#   bits <- strsplit(basename(file_name), "_") %>% # Had to put basename here
    	#     unlist()
    	#   
    	#   # # Added by chatGPT
    	#   # print(bits)
    	#   
    	#   year_start <- substr(bits[7], 1, 4)
    	#   month_start <- substr(bits[7], 5, 6)
    	#   year_end <- substr(bits[7], 10, 13)
    	#   month_end <- substr(bits[7], 14, 15)
    	#   
    	#   start_date <- paste0(year_start, "", month_start)
    	#   end_date <- paste0(year_end, "", month_end)
    	#   
    	#   output <- list(Variable = bits[1],
    	#                  Frequency = bits[2],
    	#                  Model = bits[3],
    	#                  Scenario = bits[4],
    	#                  Run = bits[5],
    	#                  Grid = bits[6],
    	#                  # Year_start = paste(year_start, month_start, sep = ""),
    	#                  # Year_end = paste(year_end, month_end, sep = "")
    	#                  Year_start = start_date,
    	#                  Year_end = end_date)
    	#   return(output)
    	#   
    	#   # e.g., map_df(dir(folder), get_CMIP6_bits)
    	# }
	
	
	
# Make folders if they don't exist ---------------------------------------------
	#** Alice added these*
	
	# Folders for all working ScenarioMIP data
	make_CMIP_folder <- function(fol_obj_name, fol_dir_name, d, m, v) { # d = disk for switching between devices, m = metric e.g. ROC, VoCC, MHW
	  
	  if(!dir.exists(fol_obj_name)) {
	    fol_obj_name <- paste0(d, "/", m, "/", v, "/", fol_dir_name) ##** Change path for each variable**
	    dir.create(fol_obj_name, recursive = T) # If the folder doesn't exist, create it
	  } 
	  assign(fol_obj_name, fol_obj_name, envir = .GlobalEnv)
	  fol_obj_name
	}
	
	
	# Folders for all processed data
	# make_processed_folder <- function(fol_obj_name, fol_dir_name, d, m, v) {
	#   
	#   if(!dir.exists(fol_obj_name)) {
	#     fol_obj_name <- paste0(d, m, v, "_processed/", fol_dir_name) ##** Change path for each variable**
	#     dir.create(fol_obj_name, recursive = T) # If the folder doesn't exist, create it
	#   } 
	#   assign(fol_obj_name, fol_obj_name, envir = .GlobalEnv)
	#   fol_obj_name
	# }
	
	
	# Folders for all working historical data
	make_hist_folder <- function(fol_obj_name, fol_dir_name, d, v) {
	  
	  if(!dir.exists(fol_obj_name)) {
	    fol_obj_name <- paste0(d, "/", v, "/", fol_dir_name) ##** Change path for each variable**
	    dir.create(fol_obj_name, recursive = T) # If the folder doesn't exist, create it
	  } 
	  assign(fol_obj_name, fol_obj_name, envir = .GlobalEnv)
	  fol_obj_name
	}
	
	
	# Folders for all processed HIST data
	make_HISTprocessed_folder <- function(fol_obj_name, fol_dir_name, d, v) {
	  
	  if(!dir.exists(fol_obj_name)) {
	    fol_obj_name <- paste0(d, "/", v, "_processed/", fol_dir_name) ##** Change path for each variable**
	    dir.create(fol_obj_name, recursive = T) # If the folder doesn't exist, create it
	  } 
	  assign(fol_obj_name, fol_obj_name, envir = .GlobalEnv)
	  fol_obj_name
	}
	
	
	# # Folders for all wget files
	# make_wget_folder <- function(fol_obj_name, fol_dir_name) {
	#   
	#   if(!dir.exists(fol_obj_name)) {
	#     fol_obj_name <- paste0("/Volumes/Alice_Dock_1/wget/", fol_dir_name) ##** Change path for each variable**
	#     dir.create(fol_obj_name, recursive = T) # If the folder doesn't exist, create it
	#   } 
	#   assign(fol_obj_name, fol_obj_name, envir = .GlobalEnv)
	#   fol_obj_name
	# }
	
	
	make_folder <- function(fol_obj_name, fol_dir_name, d, v) { # d = disk for switching between devices, m = metric e.g. ROC, VoCC, MHW
	  
	  if(!dir.exists(fol_obj_name)) {
	    fol_obj_name <- paste0(d, "/", v, "/", fol_dir_name) ##** Change path for each variable**
	    dir.create(fol_obj_name, recursive = T) # If the folder doesn't exist, create it
	  } 
	  assign(fol_obj_name, fol_obj_name, envir = .GlobalEnv)
	  fol_obj_name
	}
	
	
	
# Move processed files to processed folder ---------------------------------------------
	#** Alice added*
	
	move_files <- function(source_fol, new_fol) {
	  
	  moved_files <- list.files(source_fol, pattern = ".nc", full.names = TRUE)
	  
	  walk(moved_files, ~{
	    new_path <- file.path(new_fol, basename(.))
	    # file.rename(., new_path)
	    
	    file.copy(., new_path)
	    file.remove(.) # Remove original file after copying
	  })
	}
	
	# Reconstitute CMIP6 file name -------------------------------------------------

	make_CMIP6_file_name <- function(bits_list) {
		y1 <- as.character(bits_list$Year_start) %>% 
			gsub("-", "", .)
		y2 <- as.character(bits_list$Year_end) %>% 
			gsub("-", "", .)
		paste0(c(unlist(bits_list)[1:6], paste0(y1, "-", y2, ".nc")), collapse = "_")
		}
	
	
# Get date-type details from netCDF --------------------------------------------

	netCDF_date_deets <- function(nc_file) {
		require(PCICt)
		require(ncdf4)
		nc <- nc_open(nc_file)
		u <- nc$dim$time$units
		dts <- nc$dim$time$vals
		cdr <- nc$dim$time$calendar
		or <- strsplit(u, " ") %>%
			unlist() %>%
			.[3] %>%
			strsplit("-") %>%
			unlist()
		actual_dts <- as.PCICt(dts*60*60*24, cal = cdr, format = "%m%d%Y", origin = paste(or, collapse = "-")) %>%
			substr(1, 10) %>%
			as.Date()
		return(list(strtDate = actual_dts[1], endDate = actual_dts[length(actual_dts)], calendar = cdr))
	}


# If output folder doesn't exist,  create it -----------------------------------

	make_output_folder <- function(folder) {
		if(!isTRUE(file.info(folder)$isdir)) dir.create(folder, recursive=TRUE)
		}

# # Get disk name ----------------------------------------------------------------
# 
# 	get_disk <- function(folder) {
# 		strsplit(folder, "/") %>% 
# 			unlist %>% 
# 			.[-length(.)] %>% 
# 			paste(., collapse = "/")
# 		}


# Splice the start of ssp585 onto ssp534-overshoot -----------------------------
	# For short date formats
	splice_over <- function(f) {
	  e_yr <- substr(f, nchar(f) - 15, nchar(f) - 12)
	  e_yr_string <- paste0(as.numeric(e_yr)-1, "12") 
	  # e_yr_string <- paste0(as.numeric(st_yr)-1, "12") 
	  
	  # pt1 <- gsub(paste0(st_yr_string, "-210012.nc$"), paste0("_201501-", e_yr_string, ".nc"), f)
	  pt1 <- gsub(paste0(st_yr_string, "-210012.nc"), paste0("_201501-", e_yr_string, ".nc"), f, fixed = T)
	  pt2 <- gsub(paste0(st_yr_string, "-210012.nc"), "_201501-210012.nc", f, fixed = T) %>% 
	    gsub("ssp534-over", "ssp585", .)
	  pt3 <- gsub(paste0(st_yr_string, "-210012.nc"), paste0("_201501-210012.nc"), f)
	  
	  
	  system(paste0("cdo -L -selyear,2015/", as.numeric(st_yr)-1, " ", outfold, "/", pt2, " ", outfold, "/", pt1))
	  system(paste0("cdo -L -mergetime ", outfold, "/", pt1, " ", outfold, "/", f, " ", outfold, "/", pt3))
	  
	  system(paste0("rm ", outfold, "/", pt1))
	  system(paste0("rm ", outfold, "/", f)) # Remove old 2040-2100 file
	}
	
	
	# For long date formats
	splice_over_long <- function(f) {
	  e_yr <- substr(f, nchar(f) - 19, nchar(f) - 16) # Get the end year
	  e_yr_string <- paste0(as.numeric(e_yr)-1, "1231") # make it a string for the last day of the year before
	  # e_yr_string <- paste0(as.numeric(st_yr)-1, "12") 
	  
	  # pt1 <- gsub(paste0(st_yr_string, "-210012.nc$"), paste0("_201501-", e_yr_string, ".nc"), f)
	  pt1 <- gsub(paste0(st_yr_string, "-21001231.nc"), paste0("_20150101-", e_yr_string, ".nc"), f, fixed = T) # The date gap (2015-2041)
	  pt2 <- gsub(paste0(st_yr_string, "-21001231.nc"), "_20150101-21001231.nc", f, fixed = T) %>% # New naming for 534-over files
	    gsub("ssp534-over", "ssp585", .)
	  pt3 <- gsub(paste0(st_yr_string, "-21001231.nc"), paste0("_20150101-21001231.nc"), f)
	  
	  
	  system(paste0("cdo -L -selyear,2015/", as.numeric(st_yr)-1, " ", outfold, "/", pt2, " ", outfold, "/", pt1))
	  system(paste0("cdo -L -mergetime ", outfold, "/", pt1, " ", outfold, "/", f, " ", outfold, "/", pt3))
	  
	  system(paste0("rm ", outfold, "/", pt1))
	  # system(paste0("rm ", outfold, "/", f)) # Remove old 2040-2100 file
	}
	
	
	
	
# Cut dates to date range ------------------------------------------------------
	#** Alice added*
	cut_dates <- function(f, start_date, end_date, v, start_dt_name, outfol) { # Is going to do the same thing to each file, with file name represented by "f"
	  bits <- get_CMIP6_bits_dave(f) 
	  pat <- paste0(str_replace_all(substr(bits$Year_start, 1, 7), "-", ""))
	  new_name <- str_replace_all(basename(f), fixed(pat), start_dt_name)
	  
	  cdo_script <- paste0("cdo -s -L -f nc4 ", 
	                       "-selyear,", start_date, "/", end_date, " ", 
	                       "-selvar,", v, " ", 
	                       f, " ", outfol, "/", new_name) # Output path and file name
	  
	  system(cdo_script) # Note that CDO reads the code backwards, so selvar happens first, then selyear, then 
	}
	
	
	
# SpatRaster to netCDF file ----------------------------------------------------
	
	mask2netCDF4 <- function(x, pth = pth, # Where the new file is going
	                         ncName = ncName, # What we're calling the file
	                         dname = dname, 
	                         dlname = dlname)	{
	  nc_name <- paste0(pth, "/", ncName) # Input netCDF
	  # Temporary files
	  nc1 <- nc_name %>% 
	    str_replace(".nc", "_tmp1.nc")
	  nc2 <- nc_name %>% 
	    str_replace(".nc", "_tmp2.nc")
	  # r1out <- x[] # Write mask as a matrix
	  
	  # Set up the temporal and spatial dimensions	
	  lon <- terra::xFromCol(x, 1:ncol(x)) # Lons - from raster
	  nlon <- length(lon)
	  # lat <- yFromRow(x, 1:nrow(x)) # Lats from raster
	  lat <- terra::yFromRow(x, 1:nrow(x)) # Lats from raster
	  nlat <- length(lat)
	  
	  time <- time_length(interval(ymd_hms("1850-01-01-00:00:00"), "1850-01-01"), unit = "day")
	  nt <- length(time)
	  tunits <- "days since 1850-01-011 00:00:00.0 -0:00"
	  
	  # Use this to build a multi-layer array	
	  # tmp_array <- array(r1out, dim=c(nlon, nlat, nt)) # Write as an array
	  tmp_array <- array(x[], dim=c(nlon, nlat, nt)) # Write as an array
	  # Set netCDF variables and dimensions
	  londim <- ncdim_def("lon","degrees_east", as.double(lon), calendar = "365_day", longname = "longitude")
	  latdim <- ncdim_def("lat","degrees_north", as.double(lat), calendar = "365_day", longname = "latitude")
	  timedim <- ncdim_def("time", tunits, as.double(time), calendar = "365_day", longname = "time")
	  fillvalue <- missvalue <- 1.00000002004088e+20 # Na values
	  tmp_def <- ncvar_def(dname,"deg_C", list(londim, latdim, timedim), missvalue, dlname, prec = "double")
	  
	  # Create netCDF file and assign arrays
	  ncout <- nc_create(nc1, list(tmp_def)) # Don't force it to be netCDF4, or CDO will fail
	  ncvar_put(ncout, tmp_def, tmp_array)
	  
	  # Put additional attributes into dimension and data variables
	  ncatt_put(ncout, "lon", "axis", "X")
	  ncatt_put(ncout, "lat", "axis", "Y")
	  ncatt_put(ncout, "time", "axis", "T")
	  
	  system(paste0("nccopy -k 4 ", nc1, " ", nc2)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
	  system(paste0("cdo -invertlat ", nc2, " ", nc_name)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
	  system(paste0("rm ", nc1, " ", nc2))
	}
	
	
	
# Select only the surface level of a netCDF file -------------------------------

	select_surface <- function(f, level){
	  output_file <- basename(f) %>% 
	    change_grid_code(., "_mrgsurf_") %>% ## Added
	    paste0(surface_folder, "/", .)
	  
	  # Select the first level of the 
	  netCDF <- nc_open(f)
	  # lev <- ncvar_get(netCDF, level)[1:5] # Trying 1st 5 levels to see if it helps with data loss
	  lev <- ncvar_get(netCDF, level)[1]
	  
	  
	  cdo_script <- paste0("cdo -sellevel,", lev ," ", f, " ", output_file)
	  system(cdo_script)
	}
	
	
	
	select_surface_level_all <- function(f){
	  output_file <- basename(f) %>% 
	    change_grid_code(., "_mrgsurf_") %>% ## Added
	    paste0(surface_folder, "/", .)
	  
	  # Select the first level of the 
	  netCDF <- nc_open(f)
	  var_names <- trimws(ncvar_get(netCDF))
	  
	  if ("lev" %in% names(ncvar_get(netCDF))) {
	    level_var <- "lev"
	  } else if ("deptht" %in% names(ncvar_get(netCDF))) {
	    level_var <- "deptht"
	  } else if ("olevel" %in% names(ncvar_get(netCDF))) {
	    level_var <- "olevel"
	  } else {
	    stop("Could not find variable in the NetCDF file.")
	  }
	  lev <- ncvar_get(netCDF, level_var)[1]
	  
	  cdo_script <- paste0("cdo -sellevel,", lev ," ", f, " ", output_file)
	  system(cdo_script)
	}
	
	
	
	
# Scenario regrid/crop ---------------------------------------------------------
	regrid_n_crop <- function(f,
	                              v,
	                              cell_res = 0.25, 
	                              infold, 
	                              outfold,
	                              xmin = 100, xmax = 170, ymin = -50, ymax = -5) {
	  
	  ofile <- change_grid_code(f, "_cropregrid_")
	  # ofile <- file.path(cropped_CMIP_folder, basename(ofile)) #move to cropped folder (should be the same with each variable)
	  
	  
	  cdo_text <- paste0("cdo -L -sellonlatbox,", xmin, ",", xmax, ",", ymin, ",", ymax,  
	                     " -remapbil,r", 360*(1/cell_res), "x", 180*(1/cell_res), 
	                     " -select,name=", v, " ", infold, "/", f, " ", outfold, "/", ofile)
	  system(cdo_text)
	  
	}
	
	
# Historical regrid/crop
	# regrid_n_crop_hist <- function(f,
	#                                   v,
	#                                   cell_res = 0.25, 
	#                                   infold = raw_HIST_folder, 
	#                                   outfold = cropped_HIST_folder,
	#                                   xmin = 100, xmax = 170, ymin = -50, ymax = -5) {
	#   
	#   ofile <- change_grid_code(f, "_cropregrid_")
	# 
	#   cdo_text <- paste0("cdo -L -sellonlatbox,", xmin, ",", xmax, ",", ymin, ",", ymax,  
	#                      " -remapbil,r", 360*(1/cell_res), "x", 180*(1/cell_res), 
	#                      " -select,name=", v, " ", infold, "/", f, " ", outfold, "/", ofile)
	#   system(cdo_text)
	#   
	# }
	
	

# # Crop netCDF ----------------------------------------------------------------
	#**Alice edits - tried to crop by existing extent from a shapefile but couldn't work it, had to hardwire*

  crop_fun <- function(f) {
	   lon1 <- -110
	   lon2 <- -60
	   lat1 <- 180
	   lat2 <- -5
	   
	   ofile <- change_grid_code(f, "_cropregrid_")
	   ofile <- file.path(cropped_CMIP_folder, basename(ofile)) #move to cropped folder (should be the same with each variable)
	   
	   # This one below works 
	   # pattern <- "(^(?:[^_]*_){7})([^_]+)"  # Regular expression pattern to capture the parts before and after position [8]
	   # replacement <- "\\1crop"  # Specify the replacement pattern without the underscore
	   # ofile <- sub(pattern, replacement, f)
	   
	   cdo_text <- paste0("cdo -s -L -f nc4 -z zip -sellonlatbox,", lon1, ",", lon2, ",", lat1, ",", lat2, " ", f, " ", ofile)
	   system(cdo_text) 
	  # system(paste0("rm ", f))
    }
    # e.g., walk(files, crop_fun)


# Change file name in "grid" position ------------------------------------------
	#** Alice added, with Dave's help*
	# Matches the file naming to the stage of munging
	
	change_grid_code <- function(f, pattern) {
	 file <- basename(f) # had to add this as get_CMIP6_bits was creating bits based on the entire path and filename
	 find <- paste0("_", get_CMIP6_bits_dave(file)$Grid, "_")
	 replacement <- pattern
	 return(str_replace_all(file, find, replacement))
	}
	
	
# Change file name in "year" position ------------------------------------------
	#** Alice added, with Dave's help*
	# Matches the file naming to the stage of munging
	
	change_year_code <- function(f, pattern) {
	  file <- basename(f) # had to add this as get_CMIP6_bits was creating bits based on the entire path and filename
	  find <- paste0("_", get_CMIP6_bits_dave(file)$Year_start, "_")
	  replacement <- pattern
	  return(str_replace_all(file, find, replacement))
	}
	
	
	
	
# Cut into IPCC terms ----------------------------------------------------------

	cut_IPCC_terms <- function(f, term, v, pth) {

	  new_name <- basename(f) %>%
	    change_grid_code(., paste0("_", term[3], "_")) %>%  # Rename the grid code spot to IPCC term
	    paste0(pth, "/", .)

	  n1 <- basename(new_name) %>% # Just the file name, not the path
	    str_split("_", simplify = TRUE) %>% # Split by "_"
	    as.vector()
	  n2 <- n1
	  n2[7] <- paste0(term[1], "01", "-", term[2], "12") # Replace the model name, which is in the third slot with "ensemble"
	  out_name <- paste(n2, collapse = "_") %>%  # Build the output file name
	    paste0(pth, "/", .)

	  cdo_script <- paste0("cdo -s -L -f nc4 ",
	                       "-selyear,", paste0(term[1], "/", term[2]), " ",
	                       "-selvar,", v, " ", f, " ", out_name, ".nc")
	  system(cdo_script)
	}


	
	
# Make rasters into netCDF4 files ----------------------------------------------
	
	rast2netCDF4 <- function(x, 
	                         pth = paste0(getwd(), "/", "Data"), 
	                         ncName = "mask.nc", 
	                         dname = "tos", 
	                         dlname = "tos")	{
	  nc_name <- paste0(pth, "/", ncName) # Input netCDF
	  # Temporary files
	  nc1 <- nc_name %>% 
	    str_replace(".nc", "_tmp1.nc")
	  nc2 <- nc_name %>% 
	    str_replace(".nc", "_tmp2.nc")
	  r1out <- x[] # Write mask as a matrix
	  # Set up the temporal and spatial dimensions	
	  lon <- terra::xFromCol(x, 1:ncol(x)) # Lons - from raster
	  nlon <- length(lon)
	  lat <- yFromRow(x, 1:nrow(x)) # Lats from raster
	  nlat <- length(lat)
	  time <- time_length(interval(ymd_hms("1850-01-01-00:00:00"), "1850-01-01"), unit = "day")
	  nt <- length(time)
	  tunits <- "days since 1850-01-011 00:00:00.0 -0:00"
	  # Use this to build a multi-layer array	
	  tmp_array <- array(r1out, dim=c(nlon, nlat, nt)) # Write as an array
	  # Set neCDF variables and dimensions
	  londim <- ncdim_def("lon","degrees_east", as.double(lon), calendar = "365_day", longname = "longitude") 
	  latdim <- ncdim_def("lat","degrees_north", as.double(lat), calendar = "365_day", longname = "latitude") 
	  timedim <- ncdim_def("time", tunits, as.double(time), calendar = "365_day", longname = "time")
	  fillvalue <- missvalue <- 1.00000002004088e+20 # Na values
	  tmp_def <- ncvar_def(dname,"deg_C", list(londim, latdim, timedim), missvalue, dlname, prec = "double")
	  # Create netCDF file and assign arrays
	  ncout <- nc_create(nc1, list(tmp_def)) # Don't force it to be netCDF4, or CDO will fail
	  ncvar_put(ncout, tmp_def, tmp_array)
	  # Put additional attributes into dimension and data variables
	  ncatt_put(ncout, "lon", "axis", "X")
	  ncatt_put(ncout, "lat", "axis", "Y")
	  ncatt_put(ncout, "time", "axis", "T")
	  system(paste0("nccopy -k 4 ", nc1, " ", nc2)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
	  system(paste0("cdo -invertlat ", nc2, " ", nc_name)) # Convert to netCDF4 "classic model" mode for CDO to be able to read it
	  system(paste0("rm ", nc1, " ", nc2))
	}	
	
	
	
	
# Merge netCDFs from the same model and scenario -------------------------------
	#**Alice/ChatGPT workaround using intermediate files*
	  ##** Alice edits - added the variable (v) and freq (f) for the file naming, so code is generic across vars. Also added ifelse in case files downloaded are already merged*
		merge_files <- function(model, scenario, variant, infold, outfold, v, f) { 
	  files <- dir(infold, pattern = model)
	  files <- files[grepl(scenario, files)]
	  y1 <- get_CMIP6_bits_dave(files[1])$Year_start %>%
	    as.character() %>%
	    gsub("-", "", .)
	  y2 <- get_CMIP6_bits_dave(files[length(files)])$Year_end %>%
	    as.character() %>%
	    gsub("-", "", .)
	  
	  input_files <- paste0(infold, "/", files)
	  output_file <- paste0(outfold, "/", v, "_", f, "_", model, "_", scenario, "_", variant, "_mrg_", y1, "-", y2, ".nc") 
	  intermediate_file <- paste0(outfold, "/", v, "_", f, "_", model, "_", scenario, "_", variant, "_mrg_temp.nc") 
	  
	  system(paste0("cdo -s -L mergetime ", paste(input_files, collapse = " "), " ", intermediate_file)) 
	  system(paste0("cdo -s -L select,name=", v, " ", intermediate_file, " ", output_file))
	  file.remove(intermediate_file)
	  }
	

	
	
# Merge HISTORICAL netCDFs from the same model ---------------------------------
	
	merge_HIST_files <- function(model, infold, outfold, v, f) { 
	  
	  files <- dir(infold, pattern = model)
	  model_files <- files[grepl(model, files)]
	  
	  # model_files <- files %>% filter(Model == model) # Can I use the same code from above
	  
	  y1 <- get_CMIP6_bits_dave(model_files[1])$Year_start %>%
	    as.character() %>%
	    gsub("-", "", .)
	  y2 <- get_CMIP6_bits_dave(model_files[length(model_files)])$Year_end %>%
	    as.character() %>%
	    gsub("-", "", .)
	  
	  input_files <- paste0(infold, "/", model_files)
	  output_file <- paste0(outfold, "/", v, "_", f, "_", model, "_historical_r1i1p1f1_mrg_", y1, "-", y2, ".nc") 
	  intermediate_file <- paste0(outfold, "/", v, "_", f, "_", model, "_historical_r1i1p1f1_mrg_temp.nc") 
	  
	  system(paste0("cdo -s -L mergetime ", paste(input_files, collapse = " "), " ", intermediate_file)) 
	  system(paste0("cdo -s -L select,name=", v, " ", intermediate_file, " ", output_file))
	  file.remove(intermediate_file)
	}
	
	
	# # Same as above, but using the long date format (get_CMIP6_bits_long) function
	# merge_HIST_files_long <- function(model, infold, outfold, v, f) { 
	#   
	#   files <- dir(infold, pattern = model)
	#   model_files <- files[grepl(model, files)]
	#   
	#   # model_files <- files %>% filter(Model == model) # Can I use the same code from above
	#   
	#   y1 <- get_CMIP6_bits_long(model_files[1])$Year_start %>%
	#     as.character() %>%
	#     gsub("-", "", .)
	#   y2 <- get_CMIP6_bits_long(model_files[length(model_files)])$Year_end %>%
	#     as.character() %>%
	#     gsub("-", "", .)
	#   
	#   input_files <- paste0(infold, "/", model_files)
	#   output_file <- paste0(outfold, "/", v, "_", f, "_", model, "_historical_r1i1p1f1_mrg_", y1, "-", y2, ".nc") 
	#   intermediate_file <- paste0(outfold, "/", v, "_", f, "_", model, "_historical_r1i1p1f1_mrg_temp.nc") 
	#   
	#   system(paste0("cdo -s -L mergetime ", paste(input_files, collapse = " "), " ", intermediate_file)) 
	#   system(paste0("cdo -s -L select,name=", v, " ", intermediate_file, " ", output_file))
	#   file.remove(intermediate_file)
	# }
	
	
# Join HISTORICAL and SCENARIO netCDFs together into the same time series------

	join_hist_n_scenario <- function(f) {
	  hist_file <- str_subset(map_chr(HISTfiles, basename), get_CMIP6_bits_dave(f)$Model)
	  ssp_year <- paste0(str_remove_all(substr(get_CMIP6_bits_dave(f)$Year_start, 1, 7), "-"))  # This gives "201501"
	  hist_year <- paste0(str_remove_all(substr(get_CMIP6_bits_dave(hist_file)$Year_start, 1, 7), "-"))  # This gives "1993-01-01"
	  output_file <- basename(f) %>%
	    str_replace(ssp_year, hist_year) %>%
	    paste0(joined_CMIP_folder, "/", .)
	  cdo_code <- paste0("cdo -L -mergetime ", merged_HIST_folder, "/", hist_file, " ", merged_CMIP_folder, "/", f, " ", output_file)
	  system(cdo_code)
	}
	
	
# Get data from one year only --------------------------------------------------

	get_Year <- function(nc_file, yr, infold, outfold) {
		system(paste0("cdo selyear,", yr, " ", infold, "/", nc_file, " ", outfold, "/", nc_file))
		}

	
# Get data from range of years --------------------------------------------------
  #**Alice edits - blocked this out as I've been checking the dates are between 2015-2100 as I go*
	get_Years <- function(nc_file, yr1, yr2, infold, outfold) {
		bits <- get_CMIP6_bits(nc_file)
			# y1 <- year(bits$Year_start)
			# y2 <- year(bits$Year_end)
			y1 <- as.integer(substr(bits$Year_start, 1, 4))
			y2 <- as.integer(substr(bits$Year_end, 1, 4))
		if(y1 < yr1 | y2 > yr2) {
			new_name <- nc_file %>%
				strsplit(paste0("_", as.character(y1))) %>%
				unlist(.) %>%
				.[1] %>%
				paste0(., "_", yr1, "01-", yr2, "12.nc")
			  # paste0(., "_", yr1, "0101-", yr2, "1231.nc")
			system(paste0("cdo selyear,", yr1, "/", yr2, " ", infold, "/", nc_file, " ", outfold, "/", new_name))
			# file.remove(paste0(infold, "/", nc_file))
			} else {
				cat("Nothing to do!")
				cat("\n")
				}
		}
	
	
	
# Dump leap days and set to standard calendar ----------------------------------

	fix_cal <- function(f, infold) {
			if(!grepl("gregorian", netCDF_date_deets(paste0(infold, "/", f))$calendar)) {
				cat(paste0(f, " does not have leap days"))
				cat("\n")
				system(paste0("cdo setcalendar,standard ", infold, "/", f, " ", infold, "/tmp_", f))
				file.remove(paste0(infold, "/", f))
				file.rename(paste0(infold, "/tmp_", f), paste0(infold, "/", f))
				} else {
					cat(paste0(f, " DOES have leap days to be removed"))
					cat("\n")
					system(paste0("cdo -L -setcalendar,standard -delete,month=2,day=29 ", infold, "/", f, " ", infold, "/tmp_", f))
					file.remove(paste0(infold, "/", f))
					file.rename(paste0(infold, "/tmp_", f), paste0(infold, "/", f))
				}
	}


# Adjust the extent of a raster object loaded from CDO so that it's logical --------------------------------------------------------------------------------
	
	fix_cdo_extent <- function(rf) {
	  rf %>% 
	    rasterToPoints() %>% 
	    data.frame() %>% 
	    mutate(x = x - .125) %>% 
	    mutate(x = ifelse(x < -180, x + 360, x)) %>% 
	    rasterFromXYZ(crs = crs(raster())) %>% 
	    extend(., extent(100, 170, -50, 0))
	}
	
# Shapefiles - get, transform projection, crop  -------------------------------------------------------------------
	#** Alice's**
	
	get_shps <- function(shp_dir){
	  shp <- st_read(shp_dir) %>% 
	    sf::st_transform(4326) %>% 
	    sf::st_crop(ext(base_r))
	}
	# e.g., eez <- get_shps("~/_MastersUSCMPAs/Data/_shapefiles/ausEEZ/au_eez_pol_april2022.shp")
	
	
	
# Present day data -------------------------------------------------------------
	#** Alice's**
	# MPAs only
	get_PDMPA <- function(r_list) { #r_list will be 'out'
	  
	  stacked <- stack(r_list)
	  PD_rast <- terra::subset(stacked, 
	                            grep("recent", names(stacked)))
	  PD_df <- as.data.frame(PD_rast) #This is the line that gets stuck
	  
	  return(list(rast = PD_rast, df = PD_df))
	  
	}
	
	
	# Whole EEZ
	get_PDEEZ <- function(r_list) { #r_list will be 'outeez'
	  
	  stacked <- stack(r_list)
	  PD_rastEEZ <- terra::subset(stacked, 
	                               grep("recent", names(stacked)))
	  PD_dfEEZ <- as.data.frame(PD_rastEEZ)
	  
	  return(list(rast = PD_rastEEZ, df = PD_dfEEZ))
	  
	}
	
	
	
# VoCC: Coarsen the temporal scales of netCDFs -------------------------------------------
  ## Function to convert climatic series (provided as RasterStack) into a coarser time frequency series for a period of interest. This function transforms the RasterStack into an xts time series object to extract the values for the period of interest and apply some summary function. It is mainly a wrapper from the apply. function family in the package xts (Ryan and Ulrich 2017).

  sumSeries <- function(r, p, yr0, l = nlayers(r), fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years"){
    # construct xts object
    m <- t(values(r))
    dates <- seq(as.Date(yr0), length = l, by = freqin)
    ts1 <- xts(m, order.by = dates)
    # subset for the period of interest
    x <- ts1[p]
    
    # calculate the annual series
    if(freqout == "weeks"){s <- apply.weekly(x, fun)}
    if(freqout == "months"){s <- apply.monthly(x, fun)}
    if(freqout == "quarters"){s <- apply.quarterly(x, fun)}    # Jan-Mar (Q1); Apr-Jn (Q2); Jl-Sep(Q3); Oct-Dec (Q4)
    if(freqout == "years"){s <- apply.yearly(x, fun)}
    if(freqout == "other"){s <- fun(x)}

    # create raster stack
    for(i in 1:nrow(s)){
      r2 <- rast(r[[1]])
      r2[] <-  as.numeric(s[i,])
      if(i == 1) {
        r1 <- r2
      } else{
        r1 <- c(r1, r2)  
      }
      
    }
    if(freqout != "other"){names(r1) <- seq(start(x), length = nlyr(r1), by = freqout)}
    return(r1)
  }


# VoCC: Temporal trend ---------------------------------------------------------------
  ## Calculate temporal trend from a raster series of a climatic variable. This trend is to be used for the calculation of the gradient-based climate velocity using gVoCC.
    # r = the coarsened netCDF file (monthly to yearly) computed using the sumSeries fn
    # th = threshold of how many data points per cell there are (e.g., th = 10 for an r with yearly data means there needs to be a minimum of 10 years of data)
  tempTrend <- function(r, th) {  
    y <- values(r)
    ocean <- which(rowSums(is.na(y))!= ncol(y))    # remove land cells
    y <- t(y[ocean, ])
    N <- apply(y, 2, function(x) sum(!is.na(x)))
    ind <- which(N >= th)
    y <- y[,ind]  # drop cells with less than th observations
    N <- apply(y, 2, function(x) sum(!is.na(x)))
    x <- matrix(nrow = nlyr(r), ncol = ncol(y))
    x[] <- 1:nlyr(r)
    # put NA values into the x values so they correspond with y
    x1 <- y
    x1[!is.na(x1)] <- 1
    x <- x*x1
    # calculate the sum terms
    sx <- apply(x, 2, sum, na.rm = T)
    sy <- apply(y, 2, sum, na.rm = T)
    sxx <- apply(x, 2, function(x) sum(x^2, na.rm = T))
    syy <- apply(y, 2, function(x) sum(x^2, na.rm = T))
    xy <- x*y
    sxy <- apply(xy, 2, sum, na.rm = T)
    # Estimate slope coefficients and associated standard errors and p-values
    slope <- (N*sxy-(sx*sy))/(N*sxx-sx^2)
    sres <- (N*syy-sy^2-slope^2*(N*sxx-sx^2))/(N*(N-2))
    SE <- suppressWarnings(sqrt((N*sres)/(N*sxx-sx^2)))
    Test <- slope/SE
    p <- mapply(function(x,y) (2*pt(abs(x), df = y-2, lower.tail = FALSE)), x = Test, y = N)
    
    slpTrends <- sigTrends <- seTrends <- rast(r[[1]])
    slpTrends[ocean[ind]] <- slope
    seTrends[ocean[ind]] <- SE
    sigTrends[ocean[ind]] <- p
    output <- c(slpTrends,seTrends,sigTrends)
    names(output) <- c("slpTrends", "seTrends", "sigTrends")
    return(output)
  }
  

# VoCC: Angle associated to the spatial gradient -------------------------------------
  # dx = the longitudinal gradient component
  # dy = the latitudinal gradient component
  angulo <- function(dx, dy){
    d <- cbind(dx, dy)
    angline <- function(rw){
      angle <- ifelse(rw[2] < 0, 180 + CircStats::deg(atan(rw[1]/rw[2])),
                      ifelse(rw[1] < 0, 360 + CircStats::deg(atan(rw[1]/rw[2])), CircStats::deg(atan(rw[1]/rw[2]))))
      return(angle)
    }
    return(apply(d, 1, angline))
  }
  
  
# VoCC: Spatial gradient -------------------------------------------------------------------------------
  ## Calculate the magnitude and direction of the spatial gradient associated to a climatic variable after Burrows et al. (2011). This trend is to be used for the calculation of the gradient-based climate velocity using gVoCC.
    # th = an integer indicating a lower thershold to truncate the spatial gradient with. Use -Inf (default) if no threshold required
    # projected = is the source raster in a projected coordinate system? If FALSE (default) a correction will be made to account for latitudinal distortion.
  spatGrad <- function(r, th = -Inf, projected = FALSE){
    if(terra::nlyr(r) > 1){r <- terra::mean(r, na.rm=TRUE)}
    # get resolution of the raster
    re <- terra::res(r)
    # Create a columns for focal and each of its 8 adjacent cells
    y <- data.table(terra::adjacent(r, 1:ncell(r), directions = 8, pairs = TRUE))
    y <- na.omit(y[, climFocal := values(r)[from]][order(from, to)])   # Get value for focal cell, order the table by raster sequence and omit NAs (land cells)
    y[, clim := values(r)[to]] # Insert values for adjacent cells
    y[, sy := rowFromCell(r, from)-rowFromCell(r, to)]  # Column to identify rows in the raster (N = 1, mid = 0, S = -1)
    y[, sx := colFromCell(r, to)-colFromCell(r, from)]  # Same for columns (E = 1, mid = 0, W = -1)
    y[sx > 1, sx := -1]   # Sort out the W-E wrap at the dateline, part I
    y[sx < -1, sx := 1]   # Sort out the W-E wrap at the dateline, part II
    y[, code := paste0(sx, sy)]    # Make a unique code for each of the eight neighbouring cells
    # Code cells with positions
    y[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
    y <- dcast(y[,c("from","code","clim")], from ~ code, value.var = "clim")
    y[, climFocal := values(r)[from]]   # Put climFocal back in
    y[, LAT := yFromCell(r, from)]         # Add focal cell latitude
    
    # Calculate individual spatial temperature gradients: grads (degC per km)
    # WE gradients difference in temperatures for each western and eastern pairs divided by the distance between the cells in each pair (corrected for  latitudinal distortion if unprojected)
    # Positive values indicate an increase in clim from W to E (i.e., in line with the Cartesian x axis)
    
    ifelse(projected == TRUE, d <- 1, d <- 111.325)
    ifelse(projected == TRUE, co <- 0, co <- 1)
    
    y[, gradWE1 := (climN-climNW)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    y[, gradWE2 := (climFocal-climW)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    y[, gradWE3 := (climS-climSW)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
    y[, gradWE4 := (climNE-climN)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    y[, gradWE5 := (climE-climFocal)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    y[, gradWE6 := (climSE-climS)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
    
    # NS gradients difference in temperatures for each northern and southern pairs divided by the distance between them (111.325 km per degC *re[2] degC)
    # Positive values indicate an increase in sst from S to N (i.e., in line with the Cartesian y axis)
    y[, gradNS1 := (climNW-climW)/(d*re[2])]
    y[, gradNS2 := (climN-climFocal)/(d*re[2])]
    y[, gradNS3 := (climNE-climE)/(d*re[2])]
    y[, gradNS4 := (climW-climSW)/(d*re[2])]
    y[, gradNS5 := (climFocal-climS)/(d*re[2])]
    y[, gradNS6 := (climE-climSE)/(d*re[2])]
    
    # Calulate NS and WE gradients. NOTE: for angles to work (at least using simple positive and negative values on Cartesian axes), S-N & W-E gradients need to be positive)
    y[, WEgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,2,1,1,2,1), na.rm = T)), .SDcols = 12:17]
    y[, NSgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,2,1,1,2,1), na.rm = T)), .SDcols = 18:23]
    y[is.na(WEgrad) & !is.na(NSgrad), WEgrad := 0L]     # Where NSgrad does not exist, but WEgrad does, make NSgrad 0
    y[!is.na(WEgrad) & is.na(NSgrad), NSgrad := 0L]     # same the other way around
    
    # Calculate angles of gradients (degrees) - adjusted for quadrant (0 deg is North)
    y[, angle := angulo(.SD$WEgrad, .SD$NSgrad), .SDcols = c("WEgrad", "NSgrad")]
    
    # Calculate the vector sum of gradients (C/km)
    y[, Grad := sqrt(apply(cbind((y$WEgrad^2), (y$NSgrad^2)), 1, sum, na.rm = TRUE))]
    
    # Merge the reduced file back into the main file to undo the initial na.omit
    from <- data.table(1:ncell(r)) # Make ordered from cells
    y <- y[from]   # merge both
    
    rAng <- rGrad <- terra::rast(r)
    rAng[y$from] <- y$angle
    rGrad[y$from] <- y$Grad
    rGrad[rGrad[] < th] <- th
    output <- c(rGrad,rAng)
    names(output) <- c("Grad", "Ang")
    return(output)
    
  }
  

# gVoCC ------------------------------------------------------------------------
  ## Function to calculate the velocity of climate change after Burrows et al. (2011) based on local climatic temporal trends and spatial gradients.
  
  gVoCC <- function(tempTrend, spatGrad){
    VoCC <- tempTrend[[1]]/spatGrad[[1]]
    # velocity angles have opposite direction to the spatial climatic gradient if warming and same direction (cold to warm) if cooling
    ind <- which(values(VoCC) > 0)
    VoCCang <- spatGrad[[2]]
    VoCCang[ind] <- spatGrad[[2]][ind] + 180
    VoCCang[] <- ifelse(VoCCang[] >= 360, VoCCang[] - 360, VoCCang[])
    output <- c(VoCC,VoCCang)
    names(output) <- c("voccMag", "voccAng")
    return(output)
  }

	
# VoCC: summary stats (written by Alice. P) ------------------------------------
	
	sum_stats <- function(f){
	  nm <- get_CMIP6_bits_dave(f, component)
	  nm <- paste0(nm$Model, "_", nm$Scenario) # make name for tibble
	  s <- readRDS(files[1]) # read in each file
	  
	  # mag <- na.omit(
	    values(s$component)
	    # ) # get rid of nas
	  # compute summary stats
	  m <- median(mag, na.rm = TRUE)
	  q25 <- quantile(mag, 0.25)
	  q75 <- quantile(mag, 0.75)
	  stats <- tibble(Name = nm, Median = m, Q25 = q25, Q75 = q75)
	  return(stats)
	}
	
	

# Trajectories -----------------------------------------------------------------
  voccTraj <- function(lonlat, vel, ang, mn, tyr, trajID = 1:nrow(lonlat)){

# make sure all raster has consistent NAs
  vel[is.na(ang)] <- NA
  vel[is.na(mn)] <- NA
  ang[is.na(vel)] <- NA
  mn[is.na(vel)] <- NA
# Set up variables to catch results, allocating the right amount of memory
  nc <- nrow(lonlat)
  remaining <- rep(tyr, nc) # String containing the time remaining for each trajectory
  llon <- rep(NA, (nc * tyr) + nc)  # Starting lons, plus one more set for each iteration
  llat <- rep(NA, (nc * tyr) + nc)
  rem <- rep(NA, (nc * tyr) + nc) # this is to keep track of trajectories trapped in internal sinks
# populate the first n slots with starting points
	llon[1:nc] <- lonlat[,1]
	llat[1:nc] <- lonlat[,2]
  rem[1:nc] <- remaining
  i <- 0      # set the iteration counter
  land <- "N"  # set to not hit land
  bounce <- "N"  # set to not bounced
# Calculate the trajectories
  pb <- utils::txtProgressBar(min = 0, max = 100, style = 3)
  while(sum(remaining <= 0) != nc){  # while there is at least one trajectory active
    utils::setTxtProgressBar(pb, 100*(sum(remaining <= 0)/nc))

    llold <- lonlat # Take a copy of lonlat (starting cell xy)
    resto <- which(remaining > 0)    # index with remaining active trajectories
    fcells <- terra::cellFromXY(vel, llold)     # focal cells
    # Extract lon and lat of landing point for the remaining active trajectories
    # limit the displacement to 2 cell lengths to reduce later the number of intermediate points
    dth <- max(res(vel))*222000
    dis <- ifelse(dth < (abs(vel[fcells[resto]])*remaining[resto]*1000), dth,(abs(vel[fcells[resto]])*remaining[resto]*1000))
    lonlat[resto,] <- terra::as.data.frame(geosphere::destPoint(llold[resto,], ang[fcells[resto]], dis)) # distance input in meters. The function adjusts internally for -180-180 and pole crossings
    tcells <- terra::cellFromXY(vel, lonlat)

# Step 1. where the trajectory is still in the same cell by tyr, it has terminated.
      # Flag those trajectories by resetting the reminding time to 0 to get them out of the next iteration.
      remaining[fcells == tcells] <- 0
      if(sum(remaining == 0) == nc){break}   # to avoid error when only 1 trajectory is left and finishes inside a cell

# Step 2. For the rest, get the last point in the starting cell and the first point in a new cell
      resto <- which(remaining > 0)     # update resto
      d <- round((distGeo(llold[resto,], lonlat[resto,])/1000), 0)
      d[d == 0] <- 1
      Trajxy = splitLine(A = llold[resto,], B = lonlat[resto,], n = d)
      # now get a list with the cells for the points in each trajectory
      if(is.list(Trajxy)){
      Trajcells <- lapply(Trajxy, cellFromXY, object = vel)
      # the first new cell in the trajectory
      index <- lapply(Trajcells, function(x) which(x != x[1])[1])
      newcell <- mapply(function(X,Y) {X[Y]}, X = Trajcells, Y = index)
      # the coordinates for that first point out of the focal cell
      newxy <- terra::as.data.frame(t(mapply(function(X,Y) {X[Y,]}, X=Trajxy, Y=index)))
      # the coordinates for the last focal cell point
      oldxy <- terra::as.data.frame(t(mapply(function(X,Y) {X[Y-1,]}, X=Trajxy, Y=index)))
      }else{       # if 1 trajectory the output from splitLine is a single matrix instead of a list
      Trajcells <- apply(Trajxy, 1, cellFromXY, object = vel)
      newcell <- Trajcells[which(Trajcells != Trajcells[1])[1]]
      newxy <- data.frame(x = Trajxy[which(Trajcells != Trajcells[1])[1],1], y = Trajxy[which(Trajcells != Trajcells[1])[1],2])
      oldxy <- data.frame(x = Trajxy[(which(Trajcells != Trajcells[1])[1])-1,1], y = Trajxy[(which(Trajcells != Trajcells[1])[1])-1,2])
      }
      # starting and destination cell ids
      oldcell <- fcells[resto]
      # Get the new velocity at the new cells
      velend <- terra::extract(vel, newcell)
      # set remaining time for each of the running trajectories
      remaining[resto][is.na(velend)] <- remaining[resto][is.na(velend)]-((distGeo(llold[resto,][is.na(velend),], oldxy[is.na(velend),])/1000)/abs(vel[fcells[resto]][is.na(velend)]))
      remaining[resto][!is.na(velend)] <- remaining[resto][!is.na(velend)]-((distGeo(llold[resto,][!is.na(velend),], newxy[!is.na(velend),])/1000)/abs(vel[fcells[resto]][!is.na(velend)]))
      # For those ending in marine cells update lonlat info to the new point
      lonlat[resto,][!is.na(velend),] <- newxy[!is.na(velend),]
      # For those ending in land cells update lonlat info to the last marine point
      lonlat[resto,][is.na(velend),] <- oldxy[is.na(velend),]

# Step 3. From step 3 onwards check if the trajectory has bounced back to the origin cell. If so, redirect along cell border.
     if(i >= 1){
      current <- newcell[!is.na(velend)]   # current cell
      last <- oldcell[!is.na(velend)] # last cell
      if(land == "Y" & bounce == "Y"){ # Given the counter increases each time the trajectory table is updated,
      # where some traj hit land AND bounced in the previous iteration the values were repeated twice for all cells. Hence, need to go 3 steps back instead of 1
      last2 <- terra::cellFromXY(vel, cbind(llon[(((i-3) * nc) + 1):(((i-3) * nc) + nc)][resto][!is.na(velend)], llat[(((i-3) * nc) + 1):(((i-3) * nc) + nc)][resto][!is.na(velend)]))  # last but one cell
      land <- "N"  # set back to no hit land
      bounce <- "N"
      }else if(xor(land == "Y", bounce == "Y")){  # if one of the two conditions then need to go two steps back
      last2 <- terra::cellFromXY(vel, cbind(llon[(((i-2) * nc) + 1):(((i-2) * nc) + nc)][resto][!is.na(velend)], llat[(((i-2) * nc) + 1):(((i-2) * nc) + nc)][resto][!is.na(velend)]))
      land <- "N"
      bounce <- "N"
      }else{    # if non of the two, then just one step back
      last2 <- terra::cellFromXY(vel, cbind(llon[(((i-1) * nc) + 1):(((i-1) * nc) + nc)][resto][!is.na(velend)], llat[(((i-1) * nc) + 1):(((i-1) * nc) + nc)][resto][!is.na(velend)]))
      }
      # Identify the bouncing trajectories
      ind <- which(current == last2 & current != last)
      if(length(ind) > 0){
      bounce <- "Y"
      # take the mean velocity from the appropriate lat/lon vel components.
      vb <- mapply(function(X,Y) {ifelse(abs(X-Y) > 1, mean(c((vel[X]*sin(pi*ang[X]/180)),(vel[Y]*sin(pi*ang[Y]/180)))),mean(c((vel[X]*cos(pi*ang[X]/180)),(vel[Y]*cos(pi*ang[Y]/180)))))},
      X = current[ind], Y = last[ind])
      # take the corresponding angle (0/180 / 90/270 for lat / lon movements)
      ab <- mapply(function(X,Y,Z) {ifelse(abs(X-Y) > 1 & Z > 0, 90, ifelse(abs(X-Y) > 1 & Z < 0, 270, ifelse(abs(X-Y) == 1 & Z > 0, 0, 180)))}, X = current[ind], Y = last[ind], Z = vb)
      if(is.matrix(ab)){ab <- ab[,1]}
      # Extract lon and lat of point previous to bounce and new destination point for the remaining active trajectories
      p <- oldxy[!is.na(velend),][ind,]
      dis <- ifelse(dth < (abs(vb)*remaining[resto][!is.na(velend)][ind]*1000), dth, (abs(vb)*remaining[resto][!is.na(velend)][ind]*1000))
      destp <- terra::as.data.frame(geosphere::destPoint(p, ab, dis))
      # where the trajectory is still in the same cell by tyr, it has terminated. Flag those trajectories by resetting the reminding time to 0 to get them out of the next iteration.
      same <- which(cellFromXY(vel, p) == cellFromXY(vel, destp))
      remaining[resto][!is.na(velend)][ind][same] <- 0
      # For the rest, get the correct destination point
      rest <- which(cellFromXY(vel, p) != cellFromXY(vel, destp))     # update rest
      if(length(rest)>0){
      d <- round((distGeo(p[rest,], destp[rest,])/1000), 0)
      d[d == 0] <- 1
      Trajxy = splitLine(A = p[rest,], B = destp[rest,], n = d)
      # now get a list with the cells for the points in each trajectory
      if(is.list(Trajxy)){
      Trajcells <- lapply(Trajxy, cellFromXY, object = vel)
      # the first new cell in the trajectory
      index <- lapply(Trajcells, function(x) which(x != x[1])[1])
      # update the coordinates for that first point out of the focal cell
      newxy[!is.na(velend),][ind,][rest,] <- terra::as.data.frame(t(mapply(function(X,Y) {X[Y,]}, X=Trajxy, Y=index)))
      # update points info for the old position in case the trajectory hit land and need to be repositioned
      i <- i+1
      llon[((i * nc) + 1):((i * nc) + nc)] <- lonlat[,1]
      llat[((i * nc) + 1):((i * nc) + nc)] <- lonlat[,2]
      llon[((i * nc) + 1):((i * nc) + nc)][resto][!is.na(velend)][ind][rest] <- oldxy[!is.na(velend),][ind,][rest,1]
      llat[((i * nc) + 1):((i * nc) + nc)][resto][!is.na(velend)][ind][rest] <- oldxy[!is.na(velend),][ind,][rest,2]
      # update the coordinates for the last focal cell point
      oldxy[!is.na(velend),][ind,][rest,] <- terra::as.data.frame(t(mapply(function(X,Y) {X[Y-1,]}, X=Trajxy, Y=index)))
      }else{       # if all the trajectories have same number of points the output from gcIntermediate is a matrix instead of a list
      Trajcells <- apply(Trajxy, 1, cellFromXY, object = vel)
      newxy[!is.na(velend),][ind,][rest,] <- data.frame(x = Trajxy[which(Trajcells != Trajcells[1])[1], 1], y = Trajxy[which(Trajcells != Trajcells[1])[1],2])
      i <- i+1
      llon[((i * nc) + 1):((i * nc) + nc)] <- lonlat[,1]  # necessary to not leave the other cells as NAs
      llat[((i * nc) + 1):((i * nc) + nc)] <- lonlat[,2]
      llon[((i * nc) + 1):((i * nc) + nc)][resto][!is.na(velend)][ind][rest] <- oldxy[!is.na(velend),][ind,][rest,1]
      llat[((i * nc) + 1):((i * nc) + nc)][resto][!is.na(velend)][ind][rest] <- oldxy[!is.na(velend),][ind,][rest,2]
      oldxy[!is.na(velend),][ind,][rest,] <- data.frame(x = Trajxy[(which(Trajcells != Trajcells[1])[1])-1,1], y = Trajxy[(which(Trajcells != Trajcells[1])[1])-1,2])
      }
      # Update main traj info
      # get remaining time for each of the running trajectories
      remaining[resto][!is.na(velend)][ind][rest] <- remaining[resto][!is.na(velend)][ind][rest]-((distGeo(newxy[!is.na(velend),][ind,][rest,], p[rest,])/1000)/abs(vb[rest]))
      rem[((i * nc) + 1):((i * nc) + nc)] <- remaining
      lonlat[resto,][!is.na(velend),][ind,][rest,] <- newxy[!is.na(velend),][ind,][rest,]
      # update the velocity at new cells (some of the redirected bouncing trajectories might have ended in a land cell)
      newcell[!is.na(velend)][ind][rest] <- cellFromXY(vel, newxy[!is.na(velend),][ind,][rest,])
      velend[!is.na(velend)][ind][rest] <- terra::extract(vel, cellFromXY(vel, newxy[!is.na(velend),][ind,][rest,]))
      }}
     }

# Step 4. For those traj ending on land redirect the trajectories if possible
     if(sum(is.na(velend)) > 0){
      land <- "Y"      # to know if land was hit in the next loop when looking at bouncing trajectories
      onland <- which(is.na(velend))  # Identify which rows of velend are on land
      # For each cell that ends on land, look for a suitable target cell...
      # Make list of candidate cell IDs: overlap between cells adjacent to "from" (fcells[is.na(sflags)][onland]) AND "to" (newcell[onland])cells
      # that are in the direction of movement (any other cells would mean "un-natural" movements).
      ccells <- suppressWarnings(mapply(function(X, Y) {
      Z <- as.numeric(intersect(adjacent(vel, X, directions = 8), adjacent(vel, Y, directions = 8)))
      Z[!is.na(vel[Z])]
      }, X = oldcell[onland], Y = newcell[onland], SIMPLIFY = FALSE))

      # Now check if a suitable cell, other than the focal cell, is available along the line of movement
      # given departure direction. Diagonals included.
      # First, calculate the velocity associated to each focal cell with which to move along the cell border given direction
      # if transfer is vertical (upper/lower cell) use horizontal velocity component, else (horizontal) use vertical component
      # negative values indicate E-W and N-S movements
      v <- mapply(function(X,Y) {ifelse(abs(X-Y) > 1, abs(vel[Y])*sin(pi*ang[Y]/180), abs(vel[Y])*cos(pi*ang[Y]/180))}, X = newcell[onland], Y = oldcell[onland])
      a <- rep(NA, length(v))  # to store angles for border movement

      for(k in 1:length(v)){
      target <- newcell[onland][k]
      focal <- oldcell[onland][k]
      if((target-focal) > 1 & v[k] > 0){    # vertical transfer, horziontal movement
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal+1,focal+ncol(vel)+1))
      a[k] <- 90
      }else if((target-focal) > 1 & v[k] < 0){
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal-1,focal+ncol(vel)-1))
      a[k] <- 270
      }else if((target-focal) < -1 & v[k] > 0){
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal+1,focal-ncol(vel)+1))
      a[k] <- 90
      }else if((target-focal) < -1 & v[k] < 0){
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal-1,focal-ncol(vel)-1))
      a[k] <- 270
      } else if((target-focal) == 1 & v[k] > 0){    # horizontal transfer, vertical movement
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal-ncol(vel),focal-ncol(vel)+1))
      a[k] <- 0
      }else if((target-focal) == 1 & v[k] < 0){
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal+ncol(vel),focal+ncol(vel)+1))
      a[k] <- 180
      }else if((target-focal) == -1 & v[k] > 0){
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal-ncol(vel),focal-ncol(vel)-1))
      a[k] <- 0
      }else if((target-focal) == -1 & v[k] < 0){
      ccells[[k]] <- subset(ccells[[k]], ccells[[k]] %in% c(focal+ncol(vel),focal+ncol(vel)-1))
      a[k] <- 180
      }}

      # Flag out the cells for which no suitable potential neighbours are available (all potential neighbours are NA; the trajectory has nowhere to go)
      empty <- as.logical(lapply(ccells, function(x) identical(x, numeric(0))))
      remaining[resto][onland][empty] <- 0
      lonlat[resto,][onland,][empty,] <- oldxy[onland,][empty,]
      # if there are cells with available neighbours
      if(sum(!empty) > 0){
      # select those cells
      leftcell <- ccells[!empty]
      focal <- oldcell[onland][!empty]
      target <- newcell[onland][!empty]
      nxy <- newxy[onland,][!empty,]
      oxy <- oldxy[onland,][!empty,]
      vleft <- v[!empty]
      aleft <- a[!empty]
      # Check if there is an actual suitable neighbour
      sst <- lapply(leftcell, function(x) mn[x])   # sst at focal and neighbouring cells
      # if warming (cooling), need a suitable neighbour with cooler (warmer) local temperatures
      for(k in 1:length(leftcell)){
      leftcell[k] <- ifelse(vel[focal[k]]>0 & sum(sst[[k]] < mn[focal[k]]) > 0, leftcell[[k]][which.min(sst[[k]])],
      ifelse(vel[focal[k]]<0 & sum(sst[[k]] > mn[focal[k]]) > 0, leftcell[[k]][which.max(sst[[k]])], NA))
      }

      # Flag cells for which no suitable neighbours are available (no suitable sst)
      remaining[resto][onland][!empty][is.na(as.numeric(leftcell))] <- 0
      lonlat[resto,][onland,][!empty,][is.na(as.numeric(leftcell)),] <- oldxy[onland,][!empty,][is.na(as.numeric(leftcell)),]

      # For those traj with a suitable neighbour, relocate the trajectory to the nearest newcell point and calculate the time needed to reach it
      if(length(target[!is.na(as.numeric(leftcell))])>0){
      dudcent <- xyFromCell(vel, target[!is.na(as.numeric(leftcell))]) # Coordinates of dud target cell on land
		  newcent <- xyFromCell(vel, as.numeric(leftcell[!is.na(as.numeric(leftcell))])) # Coordinates of new target cell
		  oldcent <- xyFromCell(vel, focal[!is.na(as.numeric(leftcell))]) # Coordinates of departure cell
		  loncent <- data.frame(oldx = oldcent[,1], NAx = dudcent[,1], newx = newcent[,1]) # An object containing the longitudes of the cells involved - needed for fixing dateline

      for(k in 1:nrow(loncent)){
      if(abs(max(loncent[k,]) - min(loncent[k,])) > 180){loncent[k,][loncent[k,] < 0] <- loncent[k,][loncent[k,] < 0] + 360}  # Remove sign change for dateline, if needed
      loncent$lonline[k] <- ifelse(abs(loncent[k,2] - loncent[k,3]) > abs(loncent[k,2] - loncent[k,1]),    # if lon difference between NA cell and new cell is larger than NA to old cell
			mean(c(loncent[k,1], loncent[k,3])), mean(c(loncent[k,1], loncent[k,2]))) # Figure out position of dividing longitude
      loncent$lonline[k] <- loncent$lonline[k] - (360 * floor((loncent$lonline[k] + 180) / 360)) # Return to -180o to 180o format
      loncent$lonline[k] <- ifelse(loncent$lonline[k] == 180 && newcent[k,1] < 0, -180, loncent$lonline[k])
      loncent$lonline[k] <- ifelse(loncent$lonline[k] == -180 && newcent[k,1] > 0, 180, loncent$lonline[k]) # Arrange lonline to accommodate dateline
      loncent$latline[k] <- ifelse(abs(dudcent[k,2] - newcent[k,2]) > abs(dudcent[k,2] - oldcent[k,2]), mean(c(oldcent[k,2], newcent[k,2])), mean(c(oldcent[k,2], dudcent[k,2])))
      loncent$dirlon[k] <- ifelse(newcent[k,1] > loncent$lonline[k], 1, -1) # Adjust direction of movement relative to lonline
	    loncent$dirlat[k] <- ifelse(newcent[k,2] > loncent$latline[k], 1, -1) # Same for latline
      loncent$lonnew[k] <- loncent$lonline[k] + (loncent$dirlon[k] * 0.0001) # add a small offset to put the traj on the new cell
      loncent$latnew[k] <- loncent$latline[k] + (loncent$dirlat[k] * 0.0001)
      loncent$latnew[k] <- ifelse(loncent$latnew[k] > 90, 90, ifelse(loncent$latnew[k] < -90, -90, loncent$latnew[k]))
      loncent$lonnew[k] <- loncent$lonnew[k] - (360 * floor((loncent$lonnew[k] + 180) / 360))
      }

      loncent$oldcell <- focal[!is.na(as.numeric(leftcell))]
      loncent$lonold  <- oxy[!is.na(as.numeric(leftcell)),1]
      loncent$latold  <- oxy[!is.na(as.numeric(leftcell)),2]
      loncent$dis <- (distGeo(oxy[!is.na(as.numeric(leftcell)),], loncent[,8:9])/1000)        # distance from dold to new point
      loncent$dur <- loncent$dis/abs(vleft[!is.na(as.numeric(leftcell))])                 # time taken to get there
      loncent$remaining <- remaining[resto][onland][!empty][!is.na(as.numeric(leftcell))]-loncent$dur

     # where remaining is < 0, the trajectory terminates before reaching new destination. Flag those traj out and place them in the corresponding final coordinates.
     if(sum(loncent$remaining < 0) > 0){
     loncent[loncent$remaining < 0, 8:9] <- as.matrix(geosphere::destPoint(oxy[!is.na(as.numeric(leftcell)),][loncent$remaining < 0,], aleft[!is.na(as.numeric(leftcell))][loncent$remaining < 0],
     (abs(vleft[!is.na(as.numeric(leftcell))][loncent$remaining < 0])*(remaining[resto][onland][!empty][!is.na(as.numeric(leftcell))][loncent$remaining < 0])*1000)))
     loncent$remaining[loncent$remaining < 0] <- 0
     }

     # Finally, update lonlat
     i <- i+1 # increase the counter by 1
     # first input the point before hitting land
     lonlat[resto,][onland,][!empty,][!is.na(as.numeric(leftcell)),] <- loncent[,11:12]
     llon[((i * nc) + 1): ((i * nc) + nc)] <- lonlat[,1]  # Add final lon to the list
	   llat[((i * nc) + 1): ((i * nc) + nc)] <- lonlat[,2] # Add final lat to the list
     # then the new point to carry the trajectory on with
     remaining[resto][onland][!empty][!is.na(as.numeric(leftcell))] <- loncent$remaining
     rem[((i * nc) + 1):((i * nc) + nc)] <- remaining
     lonlat[resto,][onland,][!empty,][!is.na(as.numeric(leftcell)),] <- loncent[,8:9]
     }}
    }

# Step 5. Update register before moving to the next projection step
    i <- i+1 # increase the counter by 1
    llon[((i * nc) + 1):((i * nc) + nc)] <- lonlat[,1]  # Add final lon to the list
    llat[((i * nc) + 1):((i * nc) + nc)] <- lonlat[,2] # Add final lat to the list
    rem[((i * nc) + 1):((i * nc) + nc)] <- remaining

# Step 6. Check for trajectories trapped in internal sinks and terminate them
    if(i >= 8){
    cs <- which(remaining != 0)
    # index the trajectories trapped in a sink (those for which distance travelled over the last 4 iterations is less than 0.5 km)
    if(length(cs) > 0){
    ind <- which(unlist(lapply(cs, function(x) all(tail(abs(diff(rem[seq(x, by = nc, length.out = i+1)], 1))[abs(diff(rem[seq(x, by = nc, length.out = i+1)], 1)) != 0], 4) < 0.5))))
    # Terminate trapped trajectories
    if(length(ind)>0){
    remaining[cs][ind] <- 0
    }}}

  }
  close(pb)

# prepare output
trajIDs = rep(trajID,(length(llon)/nc)) # rep(initialcells,(length(llon)/nc))
traj <- na.omit(data.frame(x = llon, y = llat, trajIDs = trajIDs))
# remove duplicated points (to keep track of the trajectorie's ID all trajectories are repeated over iterations even if they had terminated)
traj <- traj[!duplicated(traj),]
# remove trajectories that are a single point (artificially created because of the sea ice)
NPtTr <- table(traj$trajIDs)
traj <- subset(traj, traj$trajIDs %in% which(NPtTr > 1))

return(traj)
}
  
  



	