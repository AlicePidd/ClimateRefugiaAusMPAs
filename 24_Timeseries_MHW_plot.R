# Plotting timeseries for the rate of change in marine heatwave cumulative intensity (MHW-ROC)
  # Written by Alice Pidd
    # June 2024


# Source the helpers and necessary bits ----------------------------------------

  source("Helpers.R")
  source_disk <- "/Volumes/AliceShield/clim_data" # Where files are read from
  source("Background_plotting_data.R")
  

  
# Metric -----------------------------------------------------------------------
  
  var_nm <- mhw

  
  
# Folders ----------------------------------------------------------------------

  infol <- make_folder(source_disk, "timeseries1", var_nm[1], "plotdfs")
  plot_fol <- make_folder(source_disk, "timeseries1", "", "plots")



# Plot -------------------------------------------------------------------------
  ## Plotting the recent past as greyscale, and projections as their own colours
  
  plot_timeseries <- function(d) {
    dat <- readRDS(d) 
    zone <- basename(d) %>% 
      str_split_i("_", 2)
    
    max_dat <- dat %>%  
      filter(Year <= 2014)
    max_anom <- max(max_dat$upr_esm) 
    
    pal <- c(col_ssp126, col_ssp245, col_ssp370, col_ssp585) 
    
    ggplot(dat, aes(x = Year, y = fit_esm, color = SSP)) +
      geom_rect(xmin = 1995, xmax = 2014, ymin = -Inf, ymax = Inf, 
                fill = "grey85", 
                colour = NA, 
                alpha = 0.02) +
      geom_vline(xintercept = c(2020, 2040, 2060, 2080), 
                 linetype = "dotted",
                 color = "grey40",
                 alpha = 0.40) + 
      geom_ribbon(data = subset(dat, Year >= 1995 & Year <= 2014), 
                  aes(ymin = lwr_esm, ymax = upr_esm),
                  fill = "black",
                  colour = NA,
                  alpha = 0.3,
                  show.legend = FALSE) +
      geom_ribbon(data = subset(dat, Year >= 2015 & Year <= 2100), 
                  aes(ymin = lwr_esm, ymax = upr_esm, fill = SSP),
                  colour = NA,
                  alpha = 0.2,
                  show.legend = FALSE) +
      geom_line(data = subset(dat, Year >= 1995 & Year <= 2014), 
                color = "black",
                lwd = 0.4) +
      geom_line(data = subset(dat, Year >= 2015 & Year <= 2100), 
                aes(color = SSP)) +
      geom_hline(yintercept = max_anom, # Climline (max model variation in the recent past)
                 linetype = "dashed",
                 color = "black",
                 linewidth = 0.35) +
      scale_color_manual(values = pal) + 
      scale_fill_manual(values = pal) + 
      ylim(0, 2000) + # Consistent y axis range
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(margin = margin(t = 5))) +
      scale_x_continuous(breaks = c(2000, 2020, 2040, 2060, 2080, 2100)) + # x-axis breaks
      labs(title = paste0("MHW-CI for ", zone),
           x = "Year",
           y = "Cumulative annual MHW degree days\n(± interquartile range of values from ensemble)",
           color = "IPCC scenario")
    
    ggsave(paste0(plot_fol, "/", var_nm[1], "_timeseries_greyrecent_climline_1995-2100_", zone, ".pdf"), width = 10, height = 4.5, dpi = 3000, paper = "a4r")
  }
  
  dfs <- dir(infol, full.names = TRUE) 
  dfs
  walk(dfs, plot_timeseries) 
  

