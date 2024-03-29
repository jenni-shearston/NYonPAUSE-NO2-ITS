# Plots for Manuscripts & Presentations
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 10/16/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Monitor Location Map (Fig 1)
# 2: NO2 Time Series Plot (Fig 2)
# 3: ITS Plot (Fig 3)
# 4: ITS Results (Table 2 and Figure 4)
# 5: Pre vs During Intervention Weather (Table 1 and Supp Figure 1)
# 6: Random Intercepts and Slopes Plot (Sup. Fig 2)
# 7: Histogram of NO2 (Sup. Fig 3)
# 8: Model Diagnostic Plots (Sup. Figs. 4-7)


####**************
#### N: Notes #### 
####**************

# Na Description
# In this script we make all plots and figures for the manuscript
# We also curate data (model results) to make it easier to fill in tables


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
packages <- c("tidyverse", "lubridate", "nycgeo", "tidycensus",
              "tigris", "sf", "rgeos", "scico", "nlme", "cowplot",
              "ggmap")
lapply(packages, library, character.only = TRUE)

# 0b Load Data
no2_plot <- read_csv("./data/no2_with_covariates.csv") %>% 
  filter(datetime_full < lubridate::ymd_hms("2020-06-08 05:00:00")) %>% # = to 00 local time
  filter(datetime_full > lubridate::ymd_hms("2018-01-01 04:00:00")) %>% # remove Dec 2017 (GMT) 
  filter(!monitor_id %in% c("34-027-3001", "34-023-0011")) %>% 
  mutate(intervention = as.logical(case_when(
    datetime_local < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0,
    datetime_local >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1)))

# 0c Load models
mod_main <- readRDS("outputs/mod_main.rds")
mod_hourly <- readRDS("outputs/mod_hourly.rds")
mod_weekend <- readRDS("outputs/mod_weekend.rds")
mod_mainRIS <- readRDS("outputs/mod_mainRIS.rds")
mod_roadside <- readRDS("outputs/mod_roadside.rds")


####*****************************************
#### 1: Map of Monitor Locations (Fig 1) #### 
####*****************************************

# 1a Load monitor lat/long info
monitors <- tribble(
  ~monitor_id, ~lat, ~lon, ~monitor_name,
  "36-005-0133", 40.8679, -73.87809, "Pfizer Lab Site",
  "36-005-0110", 40.816, -73.902, "IS 52",
  "36-081-0124", 40.73614, -73.82153, "Queens College 2",
  "36-081-0125", 40.739264, -73.817694, "Queens College Near Road",
  "34-003-0010", 40.85355, -73.96618, "Fort Lee Near Road",
  "34-017-1002", 40.731645, -74.066308, "Jersey City") %>% 
  dplyr::select(-monitor_id)

# 1b Convert monitor info to sf object
monitors_sf <- st_as_sf(monitors, coords = c("lon", "lat"), 
                        crs = 4326, agr = "constant")

# 1c Convert to CRS of NYC shapefile we will use
monitors_transformed <- monitors_sf %>% st_transform(crs = 2263)

# 1d Create monitor sf object with both x/y point data and a geometry column
#    Notes: geom_sf and geom_point sometimes don't play nice, and so
#           making sure the point file has both geometry and x/y points
#           helps with that
#           Source: https://gist.github.com/andrewheiss/0580d6ffec37b6bc4d0ae8e77bf30956
monitors_transformed_w_lat_lon <- cbind(monitors_transformed, st_coordinates(monitors_transformed))
ggplot() + geom_sf(data = monitors_transformed_w_lat_lon)

# 1e Get google map basemap
#    Note: enter your own key; JS key in API_Keys.R
register_google(key = "")
basemap <- get_googlemap(center = c(lon = -73.937143, lat = 40.763750), zoom = 10,
                         maptype = "roadmap")
ggmap(basemap) + theme_void()

# 1f Create function to change bounding box of raster to desired crs
#    Notes: Source: https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
#           It's challenging to plot a shapefile on top of a raster. This is
#           the first fix I've got to work using ggplot.
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Convert the bbox to an sf polygon, transform it to 2263, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 2263))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# 1g Run bbox function
basemap <- ggmap_bbox(basemap)

# 1h Load nyc boundaries shapefile
nyc_outline <- nycgeo::nyc_boundaries(geography = "borough")
ggplot(nyc_outline) + geom_sf(fill = NA) + theme_void()

# 1i Create map
monitor_map <- ggmap(basemap) +
  coord_sf(crs = st_crs(2263)) + # force the ggplot2 map to be in 2263
  geom_sf(data = nyc_outline, fill = NA, inherit.aes = FALSE) +
  coord_sf(crs = st_crs(2263)) +
  geom_point(data = monitors_transformed_w_lat_lon, aes(x = X, y = Y), 
             size = 3, shape = 21, fill = "red", inherit.aes = FALSE) +
  theme_void()
monitor_map

# 1j Save map
tiff("./figures/monitor_map.tiff",
     units = "in", width = 8, height = 7, res = 300)
monitor_map
dev.off()


####*********************************
#### 2: Time Series Plot (Fig 2) #### 
####*********************************

# 2a Create plot: hourly time series
time_series_plot_hourly <- no2_plot %>% filter(monitor_name != "Chester") %>% 
  filter(monitor_name != "Rutgers") %>% filter(!is.na(monitor_id)) %>% 
  ggplot(aes(x = datetime_local, y = sample_measurement)) +
  geom_line(aes(color = monitor_name), alpha = 0.25) +
  geom_vline(aes(xintercept = as.integer(as.POSIXct("2020-03-22"))), color = "black",
             linetype = "dashed") +
  geom_smooth(aes(color = monitor_name)) +
  scale_color_scico_d(palette = "lajolla") + 
  xlab("Date") +
  ylab(expression('NO'[2]*' (ppb)')) +
  labs(color = "Monitor Name") + 
  annotate(geom = "text", x = lubridate::as_datetime("2020-03-15"),
           y = 150, label = "NY on PAUSE Begins", hjust = 1, size = 14 / .pt) +
  theme_bw() +
  theme(text = element_text(size = 16), legend.position="bottom") +
  guides(color = guide_legend(title.position="top")) 

# 2b Create plot: daily time series
time_series_plot_daily <- no2_plot %>% filter(monitor_name != "Chester") %>% 
  filter(monitor_name != "Rutgers") %>% filter(!is.na(monitor_id)) %>% 
  group_by(date_local, monitor_name) %>% mutate(no2_daily = mean(sample_measurement)) %>% 
  ungroup() %>% 
  ggplot(aes(x = datetime_local, y = no2_daily)) +
  geom_line(aes(color = monitor_name), alpha = 0.25) +
  geom_vline(aes(xintercept = as.integer(as.POSIXct("2020-03-22"))), color = "black",
             linetype = "dashed") +
  geom_smooth(aes(color = monitor_name)) +
  scale_color_scico_d(palette = "lajolla") + 
  xlab("Date") +
  ylab(expression('NO'[2]*' (ppb)')) +
  labs(color = "Monitor Name") + 
  annotate(geom = "text", x = lubridate::as_datetime("2020-03-15"),
           y = 55, label = "NY on PAUSE Begins", hjust = 1, size = 14 / .pt) +
  theme_bw() +
  theme(text = element_text(size = 16), legend.position="bottom") +
  guides(color = guide_legend(title.position="top")) 

# 2c Save plot
tiff("./figures/time_series_plot.tiff", 
     units = "in", width = 12, height = 8, res = 300)
time_series_plot_daily
dev.off()


####*************************
#### 3: ITS Plot (Fig 3) #### 
####*************************

# 3a Make data for plot
#    Notes: Set intervention to FALSE to predict the counterfactual NO2
data_for_its_plot <- mod_main$data %>% mutate(intervention = FALSE)

# 3b Predict NO2 for actual intervention scenario
data_for_its_plot$preds_actual = predict(mod_main)
#pred_test <- predict(mod_main, terms = "fixed")

# 3c Predict NO2 for counterfactual
data_for_its_plot$preds_counterf = predict(mod_main, data_for_its_plot)

# 3d Average predictions for all monitors
data_for_its_plot <- data_for_its_plot %>% group_by(time_elapsed) %>% 
  summarise(preds_actual = mean(preds_actual),
            preds_counterf = mean(preds_counterf))

# 3e Add datetime_local
dt <- no2_plot %>% group_by(monitor_id) %>% 
  mutate(time_elapsed = row_number()) %>% ungroup() %>% 
  dplyr::select(datetime_local, time_elapsed) %>% distinct()

# 3f Average by day and create rolling daily averages
data_for_its_plot2 <- data_for_its_plot %>% 
  left_join(dt, by = "time_elapsed", copy = TRUE) %>% 
  mutate(date = lubridate::date(datetime_local)) %>% group_by(date) %>% 
  summarise(preds_actual = mean(preds_actual),
            preds_counterf = mean(preds_counterf)) %>% ungroup() %>% 
  mutate(rollavg_actual = zoo::rollmean(preds_actual, k = 14, fill = NA, align = c("right")),
         rollavg_counterf = zoo::rollmean(preds_counterf, k = 14, fill = NA, align = c("right")))
            
# 3g Create colors vector
colors <- c("Actual" = "cornflowerblue", "Counterfactual" = "orchid4")

# 3h Create plot
its_results_plot <- data_for_its_plot2 %>% ggplot(aes(x = date)) +
  geom_point(aes(y = preds_counterf, color = "Counterfactual"), 
             alpha = 0.3, shape = "circle open") +
  geom_point(aes(y = preds_actual, color = "Actual"),  
             alpha = 0.3) + 
  geom_line(aes(y = rollavg_counterf, color = "Counterfactual"), size = 1) +
  geom_line(aes(y = rollavg_actual, color = "Actual"), size = 1) +
  geom_vline(aes(xintercept = lubridate::as_date("2020-03-22")), color = "black",
             linetype = "dashed") +
  xlab("Date") + ylab(expression('NO'[2]*' (ppb)')) + labs(color = "") +
  scale_color_manual(values = colors) +
  annotate(geom = "text", x = lubridate::as_date("2020-03-15"),
           y = 1.5, label = "NY on PAUSE Begins", hjust = 1, size = 14 / .pt) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(0.1, 0.1))
its_results_plot

# 3i Save plot
tiff("./figures/its_results_plot.tiff", 
     units = "in", width = 12, height = 8, res = 300)
its_results_plot
dev.off()


####*****************************************
#### 4: ITS Results (Table 3 & Figure 4) #### 
####*****************************************

# Here we load and prepare data to make it easy to make or copy/paste
# data into a MS Word table
# Figure 4 was made in MS PowerPoint, so here we curate model results
# to make them easier to transfer to PowerPoint

# 4a Pull intervention effects for main model (Table 3)
summary(mod_main)  
ranef(mod_main)    # random intercepts
beta_mod_main_int <- summary(mod_main)$tTable[2,1]
se_mod_main_int  <- summary(mod_main)$tTable[2,2]
lci_mod_main_int  <- beta_mod_main_int - 1.96*se_mod_main_int
uci_mod_main_int  <- beta_mod_main_int + 1.96*se_mod_main_int

# 4b Pull intervention effects for roadside models (Table 3)
beta_mod_nonroadside_int <- summary(mod_roadside$mods[[1]])$tTable[2,1]
se_mod_nonroadside_int <- summary(mod_roadside$mods[[1]])$tTable[2,2]
lci_mod_nonroadside_int <- beta_mod_nonroadside_int - 1.96*se_mod_nonroadside_int
uci_mod_nonroadside_int <- beta_mod_nonroadside_int + 1.96*se_mod_nonroadside_int

beta_mod_roadside_int <- summary(mod_roadside$mods[[2]])$tTable[2,1]
se_mod_roadside_int <- summary(mod_roadside$mods[[2]])$tTable[2,2]
lci_mod_roadside_int <- beta_mod_roadside_int - 1.96*se_mod_roadside_int
uci_mod_roadside_int <- beta_mod_roadside_int + 1.96*se_mod_roadside_int
  
# 4c Pull intervention effects for weekend/weekday models (Table 3)
beta_mod_weekday_int <- summary(mod_weekend$mods[[1]])$tTable[2,1]
se_mod_weekday_int  <- summary(mod_weekend$mods[[1]])$tTable[2,2]
lci_mod_weekday_int  <- beta_mod_weekday_int - 1.96*se_mod_weekday_int
uci_mod_weekday_int  <- beta_mod_weekday_int + 1.96*se_mod_weekday_int

beta_mod_weekend_int <- summary(mod_weekend$mods[[2]])$tTable[2,1]
se_mod_weekend_int  <- summary(mod_weekend$mods[[2]])$tTable[2,2]
lci_mod_weekend_int  <- beta_mod_weekend_int - 1.96*se_mod_weekend_int
uci_mod_weekend_int  <- beta_mod_weekend_int + 1.96*se_mod_weekend_int

# 4d Pull intervention effects for hourly models (Figure 4)
# 4d.i Create empty tibble to hold effect estimates
results_hourly <- tibble(hour = NA, beta = NA, se = NA)
# 4d.ii Run for loop to pull data from each hour
for(i in 1:length(mod_hourly$time_of_day)){
  results_hourly <- results_hourly %>% 
    add_row(hour = mod_hourly$time_of_day[[i]],
            beta = summary(mod_hourly$mods[[i]])$tTable[2,1],
            se = summary(mod_hourly$mods[[i]])$tTable[2,2])
}
# 4d.iii Calculate 95% CIs (Figure 4)
results_hourly <- results_hourly %>% 
  mutate(lci = beta - 1.96*se, uci = beta + 1.96*se) %>% 
  mutate_at(c("beta", "se", "lci","uci"), ~round(., digits = 1))


####*************************************************************************
#### 5: Pre vs During Intervention Descriptives (Table 2 and Supp Fig 1) #### 
####*************************************************************************

# 5a Pre-during supplemental table (Table 2)
pre_during <- no2_plot %>%
  mutate(spf_humidity_gkg = spf_humidity*1000) %>% # convert to g/kg
  filter(!is.na(intervention)) %>% 
  group_by(intervention) %>% 
  summarise(no2_mean = mean(sample_measurement, na.rm = T),
            no2_sd = sd(sample_measurement, na.rm = T),
            precip_mean = mean(precip, na.rm = T),
            precip_sd = sd(precip, na.rm = T),
            radiation_mean = mean(radiation, na.rm = T),
            radiation_sd = sd(radiation, na.rm = T),
            humidity_mean = mean(spf_humidity_gkg, na.rm = T),
            humidity_sd = sd(spf_humidity_gkg, na.rm = T),
            pressure_mean = mean(surf_pressure, na.rm = T),
            pressure_sd = sd(surf_pressure, na.rm = T),
            temp_mean = mean(temp, na.rm = T),
            temp_sd = sd(temp, na.rm = T),
            windspeed_mean = mean(wind_speed, na.rm = T),
            windspeed_sd = sd(wind_speed, na.rm = T)) %>% 
  pivot_longer(no2_mean:windspeed_sd,
               names_to = c("variable", "statistic"), 
               names_sep = "_",
               values_to = "value") %>% 
  mutate(value = round(value, digits = 2)) %>% 
  pivot_wider(names_from = statistic, values_from = value) 

# 5b Pre-post wind rose (Supplemental Figure 1)
# 5b.i Create dataframes for pre and during intervention
windrose_pre <- no2_plot %>% filter(intervention == 0)
windrose_int <- no2_plot %>% filter(intervention == 1)
# 5b.ii Create windroses
windrose_pre_plot <- with(windrose_pre, 
     clifro::windrose(speed = wind_speed,
                      direction = wind_dir_met,
                      speed_cuts = c(3, 6, 9, 12),
                      legend_title = "Wind Speed\n(m/s)",
                      legend.title.align = .5,
                      ggtheme = "bw",
                      col_pal = "Greys") +
                      ggtitle("A. Pre-intervention"))
windrose_int_plot <- with(windrose_int, 
     clifro::windrose(speed = wind_speed,
                      direction = wind_dir_met,
                      speed_cuts = c(3, 6, 9, 12),
                      legend_title = "Wind Speed\n(m/s)",
                      legend.title.align = .5,
                      ggtheme = "bw",
                      col_pal = "Greys") + 
                      ggtitle("B. Intervention"))
# 5b.iii Merge two plots
windrose <- 
  cowplot::plot_grid(windrose_pre_plot, windrose_int_plot)
# 5b.iv Save plot
tiff("./figures/wind_rose_plot.tiff", 
     units = "in", width = 12, height = 8, res = 300)
windrose
dev.off()


####*******************************************************
#### 6: Random Intercepts and Slopes Plot (Sup. Fig 2) #### 
####*******************************************************

# 6a Create empty tibble to hold effect estimates
results_RIvsRIS <- tibble(model = NA, beta_type = NA,
                          beta = NA, se = NA)

# 6b Add RI results
results_RIvsRIS <- results_RIvsRIS %>% 
  add_row(model = "Random Intercepts",
          beta_type = "Fixed Effect",
          beta = summary(mod_main)$tTable[2,1],
            se = summary(mod_main)$tTable[2,2])

# 6c Add RIS results
results_RIvsRIS <- results_RIvsRIS %>% 
  add_row(model = "Random Intercepts & Slopes",
          beta_type = "Fixed Effect",
          beta = summary(mod_mainRIS)$tTable[2,1],
          se = summary(mod_mainRIS)$tTable[2,2])

# 6d Add random slopes from RIS
#    Note: We add teh random slopes to the fixed effect for the intervention
#          because they are centered around 0
for(i in 1:length(ranef(mod_mainRIS)$interventionTRUE)){
  results_RIvsRIS <- results_RIvsRIS %>% 
    add_row(model = "Random Intercepts & Slopes",
            beta_type = "Random Slope",
            beta = (ranef(mod_mainRIS)[i,2]) + (summary(mod_mainRIS)$tTable[2,1]),
            se = NA)
}

# 6e Calculate 95% CIs
results_RIvsRIS <- results_RIvsRIS %>% 
  mutate(lci = beta - 1.96*se, uci = beta + 1.96*se) %>% 
  filter(!is.na(beta))

# 6f Create plot
results_RIvsRIS_plot <- results_RIvsRIS %>% 
  ggplot(aes(x = model, y = beta)) + 
  geom_point(aes(shape = beta_type, color = beta_type), 
             size = 4, alpha = 0.75) +
  geom_errorbar(aes(ymin = lci, ymax = uci)) +
  ylab("Effect Estimate with 95% CI") + xlab("Model Type") +
  labs(shape = "Beta Type", color = "Beta Type") +
  theme_bw(base_size = 16)
results_RIvsRIS_plot

# 6g Save plot
tiff("./figures/RIvsRIS_plot.tiff", 
     units = "in", width = 10, height = 8, res = 300)
results_RIvsRIS_plot
dev.off()


####**************************************
#### 7: Histogram of NO2 (Sup. Fig 3) #### 
####**************************************

# 7a Create histogram with full dataset
hist_full <- no2_plot %>% 
  ggplot(aes(x = sample_measurement)) +
  geom_histogram() + 
  ylab('Count') + 
  xlab(expression('NO'[2]*' (ppb)')) +
  theme_bw(base_size = 16)
hist_full

# # 7b Identify and remove obs with residuals > 3SD + mean
# # 7b.i Calculate mean and sd of no2
# mean_no2 = mean(no2_plot$sample_measurement, na.rm = T)   # 16.24254
# sd_no2 = sd(no2_plot$sample_measurement, na.rm = T)       # 10.39887
# # 7b.ii Remove residuals greater than 3 SD + mean
# no2_plot_nooutliers <- data.frame(residuals = residuals(mod_main),
#                                   sample_measurement = mod_main$data$sample_measurement) %>% 
#   filter(residuals < ((3*sd_no2) + mean_no2)) %>% 
#   na.omit() # n = 122,289 (n = 19 outliers removed)
# 
# hist_nooutliers <- no2_plot_nooutliers %>% 
#   ggplot(aes(x = sample_measurement)) +
#   geom_histogram() + 
#   ylab('Count') + 
#   xlab(expression('NO'[2]*' (ppb)')) +
#   theme_bw(base_size = 16)
# hist_nooutliers

# 7b Save plot
tiff("./figures/no2_histogram.tiff", 
     units = "in", width = 10, height = 8, res = 300)
hist
dev.off()


####************************************************
#### 8: Model Diagnostic Plots (Sup. Figs. 4-7) #### 
####************************************************

# 8a Create dataframe for diagnostic plots
no2_fordiag <- data.frame(fitted = fitted(mod_main),
                          resids = residuals(mod_main)) %>% 
  mutate(resids_scaled = scale(resids))
                          
# 8b Heterskedasticity
md_hetero <- no2_fordiag %>% 
  ggplot(aes(x = fitted, y = resids_scaled)) +
  geom_point() + 
  geom_hline(yintercept = 0) + 
  xlab("Fitted values") + 
  ylab("Standardized residuals") + 
  theme_bw()
md_hetero

tiff("./figures/md_hetero.tiff", 
     units = "in", width = 10, height = 8, res = 300)
md_hetero
dev.off()

# 8c QQ Plot (normally distributed resids)
qqnorm(no2_fordiag$resids, pch = 20, col = "black")
qqline(no2_fordiag$resids)

# 8d Check for influential data points
md_influential <- 
  plot(mod_main, monitor_id ~ resid(., scaled=TRUE), abline=0, pch=16,
     xlab = "Standardised residuals", ylab = "Monitor ID")
md_influential

tiff("./figures/md_influential.tiff", 
     units = "in", width = 8, height = 8, res = 300)
md_influential
dev.off()

# 8e Autocorrelation
tiff("./figures/autocor.tiff", 
     units = "in", width = 10, height = 8, res = 300)
DescTools::PlotACF(no2_fordiag$resids)
dev.off()


