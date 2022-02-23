# Plots for Manuscripts & Presentations
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 12/15/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Monitor Location Map (Fig 1)
# 2: NO2 Time Series Plot (Fig 2)
# 3: ITS Plot (Fig 3)
# 4: ITS Results Table (Table 1)
# 5: Pre vs During Intervention Weather (Supplemental Table and Figure)


####**************
#### N: Notes #### 
####**************



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
mod_RIS <- readRDS("outputs/mod_mainRIS.rds")

# 0d Load census API key
#    Note: copy and paste API key stored in .gitignore
census_api_key("")


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
#    Note: Copy and paste API key from .gitignore
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

##****************Old Version

# # 1b Load census geometry and commuting data
# # 1b.i Load variable table to id commuting var
# #v19 <- load_variables(2019, "acs5", cache = TRUE)
# # 1b.ii Load NY data
# options(tigris_use_cache = TRUE)
# nydata <- get_acs(state = c("NY"), 
#                    county = c("Bronx", "Kings", "New York", "Queens", "Richmond"),
#                    geography = "tract", 
#                    variables = c(vehicle_to_work = "B08006_002", 
#                                  population = "B01003_001"), 
#                    geometry = TRUE) %>% dplyr::select(-moe) 
# # 1b.iii Create proportion commuting to work in a vehicle variable
# pop <- as.data.frame(nydata) %>% filter(variable == "population") 
# nydata <- nydata %>% filter(variable == "vehicle_to_work") %>% 
#   mutate(prop_veh_to_work = round((estimate/pop$estimate)*100, digits = 1))
# 
# # 1c Make NYC shoreline clear
# # 1c.i Make function to remove water from census tracts
# st_erase <- function(x, y) {
#   st_difference(x, st_union(y))
# }
# # 1c.ii Pull NY water area
# ny_water <- area_water("NY", 
#                        c("Bronx", "Kings", "New York", "Queens", "Richmond"), 
#                        class = "sf") 
# # 1c.iii Create shorelines
# nyc_shorelines <- st_erase(nydata, ny_water)
# 
# # 1d Make map
# # 1d.i Map basemap
# basemap <- get_acs(state = c("NY", "NJ"), geography = "state",
#                     variable = "B01003_001", geometry = TRUE)
# ggplot() + geom_sf(data = basemap)
# # # 1d.ii Crop basemap
# basemap_crop <-
#   st_crop(basemap, xmin = -74.3, xmax = -73.68,
#               ymin = 40.48, ymax = 40.94)
# ggplot() + geom_sf(data = basemap_crop)
# # 1d.ii Add chloropleth map and monitor locations
# monitor_map <- ggplot() +
#   geom_sf(data = basemap_crop) + 
#   geom_sf(data = nyc_shorelines, color = NA, aes(fill = prop_veh_to_work), 
#           inherit.aes = FALSE) + 
#   geom_point(data = monitors, aes(x = long, y = lat), size = 3, 
#              shape = 21, fill = "white", inherit.aes = FALSE) +
#   #annotate(geom = "text", x = -73.76, y = 41.06,
#   #         label = "New York", hjust = 1, size = 14 / .pt) +
#   annotate(geom = "text", x = -74.05, y = 40.8, 
#            label = "New Jersey", hjust = 1, size = 14 / .pt) +
#   scale_fill_scico(palette = "hawaii") +
#   theme_void() + labs(fill="% Commute to work \n in Vehicle") +
#   theme(legend.position = "bottom", text = element_text(size = 14))
# 
# # 1d Save map
# tiff("./figures/monitor_map.tiff", 
#      units = "in", width = 8, height = 7, res = 300)
# monitor_map
# dev.off()


####*********************************
#### 2: Time Series Plot (Fig 2) #### 
####*********************************

# 2a Create plot: hourly time series
time_series_plot_hourly <- no2_plot %>% filter(monitor_name != "Chester") %>% 
  filter(monitor_name != "Rutgers") %>% filter(!is.na(monitor_id)) %>% 
  ggplot(aes(x = datetime_full, y = sample_measurement)) +
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
  group_by(date, monitor_name) %>% mutate(no2_daily = mean(sample_measurement)) %>% 
  ungroup() %>% 
  ggplot(aes(x = datetime_full, y = no2_daily)) +
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


####************************************
#### 4: ITS Results Table (Table 2) #### 
####************************************

# Here we load and prepare data to make it easy to make or copy/paste
# data into a MS Word table

# 4a Pull intervention effects for main model
summary(mod_main)  
ranef(mod_main)    # random intercepts
beta_mod_main_int <- summary(mod_main)$tTable[2,1]
se_mod_main_int  <- summary(mod_main)$tTable[2,2]
lci_mod_main_int  <- beta_mod_main_int - 1.96*se_mod_main_int
uci_mod_main_int  <- beta_mod_main_int + 1.96*se_mod_main_int

# 4b Pull intervention effects for roadside interaction
beta_mod_nonroadside_int <- summary(mod_roadside$mods[[1]])$tTable[2,1]
se_mod_nonroadside_int <- summary(mod_roadside$mods[[1]])$tTable[2,2]
lci_mod_nonroadside_int <- beta_mod_nonroadside_int - 1.96*se_mod_nonroadside_int
uci_mod_nonroadside_int <- beta_mod_nonroadside_int + 1.96*se_mod_nonroadside_int

beta_mod_roadside_int <- summary(mod_roadside$mods[[2]])$tTable[2,1]
se_mod_roadside_int <- summary(mod_roadside$mods[[2]])$tTable[2,2]
lci_mod_roadside_int <- beta_mod_roadside_int - 1.96*se_mod_roadside_int
uci_mod_roadside_int <- beta_mod_roadside_int + 1.96*se_mod_roadside_int
  
# 4c Pull intervention effects for weekend/weekday models
beta_mod_weekday_int <- summary(mod_weekend$mods[[1]])$tTable[2,1]
se_mod_weekday_int  <- summary(mod_weekend$mods[[1]])$tTable[2,2]
lci_mod_weekday_int  <- beta_mod_weekday_int - 1.96*se_mod_weekday_int
uci_mod_weekday_int  <- beta_mod_weekday_int + 1.96*se_mod_weekday_int

beta_mod_weekend_int <- summary(mod_weekend$mods[[2]])$tTable[2,1]
se_mod_weekend_int  <- summary(mod_weekend$mods[[2]])$tTable[2,2]
lci_mod_weekend_int  <- beta_mod_weekend_int - 1.96*se_mod_weekend_int
uci_mod_weekend_int  <- beta_mod_weekend_int + 1.96*se_mod_weekend_int

# 4d Pull intervention effects for hourly models
# 4d.i Create empty tibble to hold effect estimates
results_hourly <- tibble(hour = NA, beta = NA, se = NA)
# 4d.ii Run for loop to pull data from each hour
for(i in 1:length(mod_hourly$time_of_day)){
  results_hourly <- results_hourly %>% 
    add_row(hour = mod_hourly$time_of_day[[i]],
            beta = summary(mod_hourly$mods[[i]])$tTable[2,1],
            se = summary(mod_hourly$mods[[i]])$tTable[2,2])
}
# 4d.iii Calculate 95% CIs
results_hourly <- results_hourly %>% 
  mutate(lci = beta - 1.96*se, uci = beta + 1.96*se) %>% 
  mutate_at(c("beta", "se", "lci","uci"), ~round(., digits = 1))


####************************************************
#### 5: Pre vs During Intervention Descriptives #### 
####************************************************

# 5a Pre-during supplemental table (Table 1)
pre_during <- no2_plot %>%
  group_by(intervention) %>% 
  summarise(no2_mean = mean(sample_measurement, na.rm = T),
            no2_sd = sd(sample_measurement, na.rm = T),
            precip_mean = mean(precip, na.rm = T),
            precip_sd = sd(precip, na.rm = T),
            radiation_mean = mean(radiation, na.rm = T),
            radiation_sd = sd(radiation, na.rm = T),
            humidity_mean = mean(spf_humidity, na.rm = T),
            humidity_sd = sd(spf_humidity, na.rm = T),
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


####***************************************
#### 6: Forest Plot for hourly effects #### 
####***************************************

# 6a Alter times for plotting
results_hourly_forplot <- results_hourly %>% filter(!is.na(hour)) %>% 
  mutate(am_pm = c("am", "am", "am", "am", "am", "am", "am", "am", "am",
                   "am", "am", "am", "pm", "pm", "pm", "pm", "pm", "pm", 
                   "pm", "pm", "pm", "pm", "pm", "pm"),
         hour2 = paste0(hour, ":00"),
         hour2 = factor(hour2, levels = c("23:00", "22:00", "21:00", "20:00",
                                          "19:00", "18:00", "17:00", "16:00",
                                          "15:00", "14:00", "13:00", "12:00",
                                          "11:00", "10:00", "9:00", "8:00",
                                          "7:00", "6:00", "5:00", "4:00",
                                          "3:00", "2:00", "1:00", "0:00")))

# 6b Create forest plot
hourly_forest_plot <- results_hourly_forplot %>% 
  ggplot(aes(y = hour2, x = beta, color = am_pm)) + 
  geom_point() +
  geom_errorbarh(aes(xmin = uci, xmax = lci)) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  xlab("Effect Estimate with 95% CI") + ylab("Hour of Day") +
  labs(color = "") +
  scale_color_manual(values = c("darkgoldenrod1", "deepskyblue2")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom", legend.direction = "horizontal")
 
# 6c Save plot
tiff("./figures/hourly_forest_plot.tiff", 
     units = "in", width = 12, height = 8, res = 300)
hourly_forest_plot
dev.off()


####*******************************************************
#### 7: Random Intercepts and Slopes Plot (Sup. Fig 1) #### 
####*******************************************************

# 7a  Create empty tibble to hold effect estimates
results_RIvsRIS <- tibble(model = NA, beta_type = NA,
                          beta = NA, se = NA)

# 7b Add RI results
results_RIvsRIS <- results_RIvsRIS %>% 
  add_row(model = "Random Intercepts",
          beta_type = "Fixed Effect",
          beta = summary(mod_main)$tTable[2,1],
            se = summary(mod_main)$tTable[2,2])

# 7c Add RIS results
results_RIvsRIS <- results_RIvsRIS %>% 
  add_row(model = "Random Intercepts & Slopes",
          beta_type = "Fixed Effect",
          beta = summary(mod_mainRIS)$tTable[2,1],
          se = summary(mod_mainRIS)$tTable[2,2])

# 7d Add random slopes from RIS
#    Note: We add teh random slopes to the fixed effect for the intervention
#          because they are centered around 0
for(i in 1:length(ranef(mod_mainRIS)$interventionTRUE)){
  results_RIvsRIS <- results_RIvsRIS %>% 
    add_row(model = "Random Intercepts & Slopes",
            beta_type = "Random Slope",
            beta = (ranef(mod_mainRIS)[i,2]) + (summary(mod_mainRIS)$tTable[2,1]),
            se = NA)
}

# 7e Calculate 95% CIs
results_RIvsRIS <- results_RIvsRIS %>% 
  mutate(lci = beta - 1.96*se, uci = beta + 1.96*se) %>% 
  filter(!is.na(beta))

# 7f Create plot
results_RIvsRIS_plot <- results_RIvsRIS %>% 
  ggplot(aes(x = model, y = beta)) + 
  geom_point(aes(shape = beta_type, color = beta_type), 
             size = 4, alpha = 0.75) +
  geom_errorbar(aes(ymin = lci, ymax = uci)) +
  ylab("Effect Estimate with 95% CI") + xlab("Model Type") +
  labs(shape = "Beta Type", color = "Beta Type") +
  theme_bw(base_size = 16)
results_RIvsRIS_plot

# 7g Save plot
tiff("./figures/RIvsRIS_plot.tiff", 
     units = "in", width = 10, height = 8, res = 300)
results_RIvsRIS_plot
dev.off()









