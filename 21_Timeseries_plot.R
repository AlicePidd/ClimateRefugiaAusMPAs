# Plotting timeseries of each rate of change variable (tos, ph, o2)
  # Written by Alice Pidd
    # June 2024


# Source the helpers and necessary bits ----------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  

  
# Variable name ----------------------------------------------------------------
  
  #**Change for each variable*
  # var_nm <- tos
  # var_nm <- ph
  var_nm <- o2

  
  
# Folders ----------------------------------------------------------------------

  infol <- make_folder(source_disk, "timeseries1", var_nm[1], "plotdfs")
  plot_fol <- make_folder(source_disk, "timeseries1", "", "plots")



# Plot -------------------------------------------------------------------------
  ## Plotting the recent past as greyscale, and projections as their own colours
  
  pal <- c(col_ssp126, col_ssp245, col_ssp370, col_ssp585)
  
  plot_timeseries <- function(df) {
    zone <- basename(df) %>% 
      str_split_i("_", 2)
    
    dat <- readRDS(df)
    datdf <- as.data.frame(dat)
    max_dat <- dat %>%  
      filter(Year <= 2014)
    max_anom <- max(max_dat$upr_esm)
    min_anom <- min(max_dat$lwr_esm)
    clim_line <- ifelse(var_nm[1] == "tos", max_anom, min_anom) %>% # conditionally select max anom (if tos), or min anom (if o2 or pH)
      unique()
    y_limits <- case_when( 
      var_nm[1] == "tos" ~ c(-0.7, 5.2), # Dynamic range for "tos"
      var_nm[1] == "o2" ~ c(-0.02, 0.003), # Fixed range for "o2"
      var_nm[1] == "ph" ~ c(-0.2, max(dat$upr_esm)))  # Dynamic range for "ph"

    p <- ggplot(datdf, aes(x = Year, y = fit_esm, color = SSP)) +
      geom_rect(xmin = 1995, xmax = 2014, ymin = -Inf, ymax = Inf, 
                fill = "grey85", 
                colour = NA, 
                alpha = 0.02) + # Shade the recent term time period
      geom_vline(xintercept = c(2020, 2040, 2060, 2080),
                 linetype = "dotted",
                 color = "grey40",
                 alpha = 0.4) + 
      geom_ribbon(data = subset(datdf, Year >= 1995 & Year <= 2014),
                  aes(ymin = lwr_esm, ymax = upr_esm),
                  fill = "black",
                  colour = NA,
                  alpha = 0.3,
                  show.legend = FALSE) +
      geom_ribbon(data = subset(datdf, Year >= 2015 & Year <= 2100),
                  aes(ymin = lwr_esm, ymax = upr_esm, fill = SSP),
                  colour = NA,
                  alpha = 0.2,
                  show.legend = FALSE) +
      geom_line(data = subset(datdf, Year >= 1995 & Year <= 2014),
                aes(linetype = "1995-2014"),
                color = "black",
                # linetype = "dashed",
                lwd = 0.4) +
      geom_line(data = subset(datdf, Year >= 2015 & Year <= 2100),
                aes(color = SSP)) +
      geom_hline(yintercept = clim_line, 
                 linetype = "dashed", 
                 color = "black",
                 linewidth = 0.35) +
      scale_color_manual(values = pal) +
      scale_fill_manual(values = pal) +
      ylim(y_limits[1], y_limits[2]) + 
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(margin = margin(t = 5))) +
      scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2100)) +
    labs(title = paste0(var_nm[4], " - ", zone),
         x = "Year",
         y = var_nm[5],
         color = "IPCC scenario")
    
    print(p)
    ggsave(paste0(plot_fol, "/", var_nm[1], "_timeseries_greyrecent_climline_1995-2100_", zone, ".pdf"), 
           p,
           width = 10, height = 4.5, dpi = 3000, paper = "a4r")
  }
  
  dfs <- dir(infol, full.names = TRUE)
  walk(dfs, plot_timeseries)
  
  