# Helper functions for Pidd et al. (2025). 'Climate refugia could disappear from Australia’s marine protected areas by 2040.'
	# For working with CMIP6 netCDFs
  # Written by Alice Pidd (alicempidd@gmail.com) and David Schoeman (david.schoeman@gmail.com)
	# June 2023

install.packages("usethis")



# Packages ---------------------------------------------------------------------

library(tidyverse)
library(raster) # Retiring!
library(terra)
library(sf)
library(tmap) # Retiring!
library(tmaptools)
library(ncdf4)
library(fasterize)
library(purrr)
library(furrr)
library(future)

library(RColorBrewer)
library(viridis)
library(stringr)
library(tictoc)
library(beepr)




# Make folders if they don't exist ---------------------------------------------

make_folder <- function(d, m, v, fol_dir_name) {
  
  folder_path <- file.path(paste0(d, "/", m, "/", v, "/", fol_dir_name))
  if (!dir.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
    message("✅ Folder created: ", folder_path)
  } else {
    message("📂 Folder already exists: ", folder_path)
  }
  return(folder_path)
}




# Lists, palettes, background data ---------------------------------------------

  ## IPCC Periods ------
  
  	recent_term <- 1995:2014
  	near_term <- 2021:2040
  	mid_term <- 2041:2060
  	intermediate_term <- 2061:2080
  	long_term <- 2081:2100
  	periods <- c("recent_past", "near_term", "mid_term", "intermediate_term", "long_term")
	
	
	## IPCC terms list  ------
	
  	IPCC_terms <- list(recent = c("1995", "2014", "recent-term"),
  	                   near = c("2021", "2040", "near-term"), 
  	                   mid = c("2041", "2060", "mid-term"), 
  	                   intermediate = c("2061", "2080", "intermediate-term"), # Added period to fill in gap
  	                   long = c("2081", "2100", "long-term"))

	
  ## IPCC colours as hex ------
  
  	col_ssp119 <- rgb(84, 39, 143, maxColorValue = 255)
  	col_ssp126 <- rgb(0, 52, 102, maxColorValue = 255)
  	col_ssp245 <- rgb(112, 160, 205, maxColorValue = 255)
  	col_ssp370 <- rgb(196, 121, 0, maxColorValue = 255)
  	col_ssp534_over <- rgb(196, 121, 0, maxColorValue = 255)
  	col_ssp585 <- rgb(153, 0, 2, maxColorValue = 255)
  
  	IPCC_pal <- c(col_ssp126, col_ssp245, col_ssp370, col_ssp585)
	

  ## Palettes ------
  	
  	col_pal_ref <- c("#19A7CE", "#EE9322")  # (refugia, non-refugia)

  	
  ## Metric labels for plotting ------
  	
  	tos <- c("tos", "temperature of surface (°C)", "(°C)")
  	o2 <- c("o2", "dissolved oxygen concentration (units)", "(units)")
  	ph <- c("ph", "pH (mol H Kg¯¹)", "(mol H Kg¯¹)")
  	VoCC <- c("VoCCMag", "Gradient-based climate velocity (km/decade)", "(km/decade)")
  	mhw <- c("MHW_CumInt", "Marine heatwave cumulative intensity", "(degree days)")
  	mhwROC <- c("MHW-ROC", "Rate of change in marine heatwave cumulative intensity", "(degree days / decade")
  	
  	
  	
  ## SSP naming and ordering ------
  	
  	ssp_num <- list("ssp126", "ssp245", "ssp370", "ssp585")
  	
  	stack_order <- c("recent", #"present", 
  	                 "near", 
  	                 "mid", 
  	                 "intermediate", 
  	                 "long")
  	
  	
	
# Cut files into IPCC reporting periods ----------------------------------------
	
	cut_IPCC_terms <- function(f, term, v, pth) {
	  
	  new_name <- basename(f) %>%
	    change_grid_code(., paste0("_", term[3], "_")) %>% 
	    paste0(pth, "/", .)
	  
	  n1 <- basename(new_name) %>% 
	    str_split("_", simplify = TRUE) %>% 
	    as.vector()
	  n2 <- n1
	  n2[7] <- paste0(term[1], "01", "-", term[2], "12") 
	  out_name <- paste(n2, collapse = "_") %>%
	    paste0(pth, "/", .)
	  
	  cdo_script <- paste0("cdo -s -L -f nc4 ",
	                       "-selyear,", paste0(term[1], "/", term[2]), " ",
	                       "-selvar,", v, " ", f, " ", out_name, ".nc")
	  system(cdo_script)
	}
	
	
	
	
# Get bits of CMIP6 file names -------------------------------------------------

	get_CMIP6_bits <- function(file_name) {
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
	}
	
	
	
	
# Move processed files to processed folder ---------------------------------------------

	move_files <- function(source_fol, new_fol) {
	  
	  moved_files <- list.files(source_fol, pattern = ".nc", full.names = TRUE)
	 
	  walk(moved_files, ~{
	    new_path <- file.path(new_fol, basename(.))

	    file.copy(., new_path)
	    file.remove(.)
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

	

# Splice the start of ssp585 onto ssp534-overshoot -----------------------------
	
	## For short date formats -----
	
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
	
	
	
	## For long date formats -----
	
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
	  # system(paste0("rm ", outfold, "/", f)) # If need to remove old 2040-2100 file
	}
	
	
	
# Cut dates to date range ------------------------------------------------------
	
	cut_dates <- function(f, start_date, end_date, v, start_dt_name, outfol) { 
	  bits <- get_CMIP6_bits_dave(f) 
	  pat <- paste0(str_replace_all(substr(bits$Year_start, 1, 7), "-", ""))
	  new_name <- str_replace_all(basename(f), fixed(pat), start_dt_name)
	  
	  cdo_script <- paste0("cdo -s -L -f nc4 ", 
	                       "-selyear,", start_date, "/", end_date, " ", 
	                       "-selvar,", v, " ", 
	                       f, " ", outfol, "/", new_name) 
	  system(cdo_script) 
	}
	
	
	
# Make rasters into netCDF4 files ----------------------------------------------
	
	rast2netCDF4 <- function(x, 
	                         pth = paste0(getwd(), "/", "Data"), 
	                         ncName = "mask.nc", 
	                         dname = "tos", 
	                         dlname = "tos")	{
	  nc_name <- paste0(pth, "/", ncName) 
	  
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
	
	
	
# SpatRaster to netCDF file ----------------------------------------------------
	
	mask2netCDF4 <- function(x, pth = pth, 
	                         ncName = ncName, 
	                         dname = dname, 
	                         dlname = dlname)	{
	  nc_name <- paste0(pth, "/", ncName) 
	  # Temporary files
	  nc1 <- nc_name %>% 
	    str_replace(".nc", "_tmp1.nc")
	  nc2 <- nc_name %>% 
	    str_replace(".nc", "_tmp2.nc")

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
	  
	  netCDF <- nc_open(f)
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

  crop_fun <- function(f) {
	   lon1 <- -110
	   lon2 <- -60
	   lat1 <- 180
	   lat2 <- -5
	   
	   ofile <- change_grid_code(f, "_cropregrid_")
	   ofile <- file.path(cropped_CMIP_folder, basename(ofile))
	   
	   # This one below works 
	   # pattern <- "(^(?:[^_]*_){7})([^_]+)"  # Regular expression pattern to capture the parts before and after position [8]
	   # replacement <- "\\1crop"  # Specify the replacement pattern without the underscore
	   # ofile <- sub(pattern, replacement, f)
	   
	   cdo_text <- paste0("cdo -s -L -f nc4 -z zip -sellonlatbox,", lon1, ",", lon2, ",", lat1, ",", lat2, " ", f, " ", ofile)
	   system(cdo_text) 
    }
    # e.g., walk(files, crop_fun)

	

# Change file name in "grid" position ------------------------------------------
	# Matches the file naming to the stage of munging
	
	change_grid_code <- function(f, pattern) {
	 file <- basename(f)
	 find <- paste0("_", get_CMIP6_bits_dave(file)$Grid, "_")
	 replacement <- pattern
	 return(str_replace_all(file, find, replacement))
	}
	
	
	
# Change file name in "year" position ------------------------------------------
	# Matches the file naming to the stage of munging
	
	change_year_code <- function(f, pattern) {
	  file <- basename(f) # had to add this as get_CMIP6_bits was creating bits based on the entire path and filename
	  find <- paste0("_", get_CMIP6_bits_dave(file)$Year_start, "_")
	  replacement <- pattern
	  return(str_replace_all(file, find, replacement))
	}
	
	


# Merge netCDFs from the same model and scenario -------------------------------
	
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
	
	
	## Same as above, but using the long date format (get_CMIP6_bits_long) function -----
	
	merge_HIST_files_long <- function(model, infold, outfold, v, f) {

	  files <- dir(infold, pattern = model)
	  model_files <- files[grepl(model, files)]

	  # model_files <- files %>% filter(Model == model) # Can I use the same code from above

	  y1 <- get_CMIP6_bits_long(model_files[1])$Year_start %>%
	    as.character() %>%
	    gsub("-", "", .)
	  y2 <- get_CMIP6_bits_long(model_files[length(model_files)])$Year_end %>%
	    as.character() %>%
	    gsub("-", "", .)

	  input_files <- paste0(infold, "/", model_files)
	  output_file <- paste0(outfold, "/", v, "_", f, "_", model, "_historical_r1i1p1f1_mrg_", y1, "-", y2, ".nc")
	  intermediate_file <- paste0(outfold, "/", v, "_", f, "_", model, "_historical_r1i1p1f1_mrg_temp.nc")

	  system(paste0("cdo -s -L mergetime ", paste(input_files, collapse = " "), " ", intermediate_file))
	  system(paste0("cdo -s -L select,name=", v, " ", intermediate_file, " ", output_file))
	  file.remove(intermediate_file)
	}
	
	
	
# Join historical and SSP netCDFs together into the same time series -----------

	join_hist_n_scenario <- function(f) {
	  
	  hist_file <- str_subset(map_chr(HISTfiles, basename), get_CMIP6_bits_dave(f)$Model)
	  ssp_year <- paste0(str_remove_all(substr(get_CMIP6_bits_dave(f)$Year_start, 1, 7), "-")) 
	  hist_year <- paste0(str_remove_all(substr(get_CMIP6_bits_dave(hist_file)$Year_start, 1, 7), "-"))
	  
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
	
	get_Years <- function(nc_file, yr1, yr2, infold, outfold) {
		bits <- get_CMIP6_bits(nc_file)
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

	

# Adjust the extent of a raster object loaded from CDO so that it's logical ------------
	
	fix_cdo_extent <- function(rf) {
	  rf %>% 
	    rasterToPoints() %>% 
	    data.frame() %>% 
	    mutate(x = x - .125) %>% 
	    mutate(x = ifelse(x < -180, x + 360, x)) %>% 
	    rasterFromXYZ(crs = crs(raster())) %>% 
	    extend(., extent(100, 170, -50, 0))
	}
	
# Shapefiles - get, transform projection, crop  --------------------------------

	get_shps <- function(shp_dir){
	  shp <- st_read(shp_dir) %>% 
	    sf::st_transform(4326) %>% 
	    sf::st_crop(ext(base_r))
	}

	
	
# Present day data -------------------------------------------------------------
	
	## MPAs only -----
	
	get_PDMPA <- function(r_list) { 
	  
	  stacked <- stack(r_list)
	  PD_rast <- terra::subset(stacked, 
	                            grep("recent", names(stacked)))
	  PD_df <- as.data.frame(PD_rast) 
	  
	  return(list(rast = PD_rast, df = PD_df))
	}
	
	
	## Whole EEZ -----
	
	get_PDEEZ <- function(r_list) { 
	  
	  stacked <- stack(r_list)
	  PD_rastEEZ <- terra::subset(stacked, 
	                               grep("recent", names(stacked)))
	  PD_dfEEZ <- as.data.frame(PD_rastEEZ)
	  
	  return(list(rast = PD_rastEEZ, df = PD_dfEEZ))
	}
	