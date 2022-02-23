# Add Covariates and Clean Analytic Dataset
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 01/19/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Descriptives for NO2
# 2: Weather data
# 3: PM2.5 data
# 4: Descriptives for weather and PM2.5
# 5: Save dataset with full observations (all possible) and covariates


####**************
#### N: Notes #### 
####**************

# In this script, we browse and clean the NO2 data and review time series of different monitors,
# including correlations between all monitors. We add weather and PM2.5 data to be used as
# potential confounding variables in future analysis. We ensure the dataset is full, in other
# words that it has an observation for all possible monitor/datetime combinations, even if the
# NO2 data is missing for that monitor/datetime combination. We are aiming for a total n of 210432 -->
# 8 monitors multiplied by 26304 hours (365*24*2+366*24)


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(tidyverse); library(data.table); library(lubridate); library(splines) 
library(rnoaa); library(naniar); library(weathermetrics); library(rvest); library(httr)

# 0b Load NO2 Data
no2 <- fread("./Data/no2_nyc_area.csv")

# 0c Get a Feel for the Data
glimpse(no2)
names(no2)
summary(no2)

# 0d Review Some Variables
# If we can drop vars that are the same for all obs
# this will decrease dataset size and increase speed
table(no2$poc, useNA = c("always")) 
table(no2$datum, useNA = c("always")) 
table(no2$parameter, useNA = c("always")) 
table(no2$parameter_code, useNA = c("always")) 
table(no2$units_of_measure, useNA = c("always")) 
table(no2$units_of_measure_code, useNA = c("always")) 
table(no2$sample_duration, useNA = c("always")) 
table(no2$sample_duration_code, useNA = c("always")) 
table(no2$sample_frequency, useNA = c("always")) 
table(no2$detection_limit, useNA = c("always")) # varied detection limits
table(no2$uncertainty, useNA = c("always")) 
table(no2$qualifier, useNA = c("always")) # super useful, with details like "high winds"
table(no2$method_type, useNA = c("always")) 
table(no2$method, useNA = c("always")) # different methods used
table(no2$method_code, useNA = c("always")) 
table(no2$cbsa_code, useNA = c("always")) 
table(no2$sample_measurement, useNA = c("always")) 
table(no2$county_code, useNA = c("always")) 
table(no2$county_name, useNA = c("always")) 
table(no2$state_name, useNA = c("always")) 
table(no2$state_code, useNA = c("always")) 
table(no2$status, useNA = c("always")) 
table(no2$request_time, useNA = c("always")) # leave as it might be useful is something is wrong w data
table(no2$url, useNA = c("always")) # leave as it might be useful is something is wrong w data

# 0e Drop vars that are not needed
# Notes: poc, units_of_measure, units_of_measure_code, sample_duration, sample_frequency, parameter,
# parameter_code, status, sample_duration_code are the same for all obs; 
# uncertainty is NA for all obs
no2_clean <- no2 %>% 
  dplyr::select(-units_of_measure, -sample_duration, -sample_frequency, -parameter,
                -parameter_code, -status, -uncertainty, -poc,
                -units_of_measure_code, -sample_duration_code)
rm(no2)

# 0f Create datetime_local variable
# Notes: _local is standard local time (not adjusted for daylight savings) while
#        _gmt is also not adjusted for daylight savings
#        keep both -- nldas weather data uses gmt 
no2_clean <- no2_clean %>% 
  mutate(datetime_local1 = paste0(date_local, " ", time_local),
         datetime_gmt1 = paste0(date_gmt, " ", time_gmt)) %>% 
  mutate(datetime_local = ymd_hm(datetime_local1),
         datetime_gmt = ymd_hm(datetime_gmt1, tz = "GMT")) %>% 
  dplyr::select(-datetime_local1, -datetime_gmt1)

# 0g Create unique id for all datetime-monitor combinations
no2_clean <- no2_clean %>% 
  mutate(uniq_mon_dt = paste0(monitor_name, "_", datetime_gmt))
no2_clean$uniq_mon_dt[duplicated(no2_clean$uniq_mon_dt)] # Check for duplicates --> should be 0

# 0h Create unique datetime-monitor id for all *possible* combinations 
#    (including those that are missing): n = 210432
monitors <- rep(c("Pfizer Lab Site", "IS 52", "Queens College 2",
              "Queens College Near Road", "Fort Lee Near Road", "Jersey City",
              "Chester", "Rutgers"), each = 26304)
datetime_full <- rep(seq(ymd_hm("2018-01-01 0:00", tz = "GMT"), ymd_hm("2020-12-31 23:00", tz = "GMT"), by = "hour"), 8)
df_uniq_mon_dt <- tibble(monitors, datetime_full) %>% mutate(uniq_mon_dt = paste0(monitors, "_", datetime_full))
df_uniq_mon_dt$uniq_mon_dt[duplicated(df_uniq_mon_dt$uniq_mon_dt)] # check for duplicates --> should be 0
df_uniq_mon_dt <- df_uniq_mon_dt %>% dplyr::select(-monitors) %>% 
  mutate(monitor_id = rep(c("36-005-0133", "36-005-0110", "36-081-0124",
                            "36-081-0125", "34-003-0010", "34-017-1002",
                            "34-027-3001", "34-023-0011"), each = 26304))

# 0i Fill in missing datetime-monitor obs with NA
no2_full <- df_uniq_mon_dt %>% left_join(no2_clean, by = "uniq_mon_dt") %>% 
  dplyr::select(-monitor_id.y) %>% rename(monitor_id = monitor_id.x)


####*************************
#### 1: Descriptives: NO2 #### 
####**************************

# 1a Time series plot with all data
no2_full %>% 
  filter(!is.na(sample_measurement)) %>% 
  ggplot(aes(x = datetime_local, y = sample_measurement)) +
  geom_line(aes(color = county_name)) +
  facet_wrap(~monitor_name)

# 1b Smoothed time plot of all NO2 monitors
no2_full %>% 
  ggplot(aes(x = datetime_local)) +
  geom_smooth(aes(y = sample_measurement, color = monitor_name))

# 1c Central tendency (by monitor-year)
cent_tend <- no2_full %>% group_by(monitor_name, pull_year) %>% 
  summarize(mean = mean(sample_measurement, na.rm = T),
            sd = sd(sample_measurement, na.rm = T),
            median = median(sample_measurement, na.rm = T),
            iqr = IQR(sample_measurement, na.rm = T)) %>% 
  arrange(mean)

# 1d Correlations between monitors
corr <- no2_full %>% dplyr::select(sample_measurement, monitor_name) %>%
  group_by(monitor_name) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = monitor_name, values_from = sample_measurement) %>% 
  dplyr::select(-row, -'NA') %>% 
  cor(use = "pairwise.complete.obs")

corrplot::corrplot(corr, type = "upper", tl.col = "black", tl.srt = 45)

# Notes: 
#        TIME PLOT:
#        Chester regularly has the lowest concentrations, followed by Rutgers
#        Pfizer Lab and Queens College 2 have similar patterns/concentrations        
#        IS52, Fort Lee near road, and Queens near road have similar patterns/concentrations
#        Jersey City seems to have a different pattern than other locations and higher concentrations
#        CORR PLOT:
#        IS52, Queens College 2, Queens College Near Road, Jersey City highly correlated


####**************************************
#### 2: Load and Prepare Weather Data #### 
####**************************************

# Notes: Analyses were first run using ISD data, but because there was substantial
#        missing weather data, we later switched to NLDAS data from NASA. Only
#        NLDAS data has been included in the script below (older ISD script can
#        be found in Extra_Code_Snippets (not available publicly on github)).
#        Here are links to the NLDAS weather documentation and EPA AQS documentation
#        specifying time zones for each dataset (GMT!):
#        https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/README.NLDAS2.pdf
#        https://aqs.epa.gov/aqsweb/documents/data_mart_welcome.html

# 2a Load NLDAS weather data
nldas_2018 <- fst::read_fst('data/nldas_data/2018_NLDAS_NYC.fst')
nldas_2019 <- fst::read_fst('data/nldas_data/2019_NLDAS_NYC.fst')
nldas_2020 <- fst::read_fst('data/nldas_data/2020_NLDAS_NYC.fst')

# 2b Bind 2018-2020 data together
nldas <- nldas_2018 %>% 
  bind_rows(nldas_2019, nldas_2020) 

# 2c Convert TimeHour (in GMT) to datetime format in GMT
nldas <- nldas %>% 
  mutate(TimeHour2 = str_remove(TimeHour, "\\."),
         datetime_gmt = ymd_h(TimeHour2, tz = "GMT"))

# 2d Average all centroids
# Notes: Could also do a nearest neighbor approach and assign each monitor
#        the weather from the centroid nearest it. Because weather is a covariate
#        and not an exposure or outcome we have averaged for simplicity.
#        Should end with n=26304
nldas <- nldas %>% group_by(datetime_gmt) %>% 
  summarise(temp = mean(temp),
            spf_humidity = mean(spf_humidity, na.rm = T),
            surf_pressure = mean(surf_pressure, na.rm = T),
            zonal_wind = mean(zonal_wind, na.rm = T),
            merid_wind = mean(merid_wind, na.rm = T),
            precip = mean(precip, na.rm = T),
            radiation = mean(radiation, na.rm = T))

# 2e Create wind speed and wind direction variables
# Notes: Directions for converting between zonal and meridonial velocity and
#        wind speed and direction: 
#        http://tornado.sfsu.edu/geosciences/classes/m430/Wind/WindDirection.html
#        https://www.ncl.ucar.edu/Document/Functions/Contributed/wind_direction.shtml
#        https://www.ncl.ucar.edu/Document/Functions/Built-in/atan2.shtml
radian2degree = 45/atan(1)
nldas <- nldas %>% 
  mutate(wind_speed = sqrt((zonal_wind^2)+(merid_wind^2)),
         wind_dir_met = atan2(zonal_wind, merid_wind)*radian2degree + 180)

# 2f Create wind direction 8-level categorical variable
nldas <- nldas %>% 
  mutate(wind_dir_met_cat = as.factor(case_when(
    wind_dir_met >= 337.5 | wind_dir_met < 22.5 ~ "North",
    wind_dir_met >= 22.5 & wind_dir_met < 67.5 ~ "North-east",
    wind_dir_met >= 67.5 & wind_dir_met < 112.5 ~ "East",
    wind_dir_met >= 112.5 & wind_dir_met < 157.5 ~ "South-east",
    wind_dir_met >= 157.5 & wind_dir_met < 202.5 ~ "South",
    wind_dir_met >= 202.5 & wind_dir_met < 247.5 ~ "South-west",
    wind_dir_met >= 247.5 & wind_dir_met < 292.5 ~ "West",
    wind_dir_met >= 292.5 & wind_dir_met < 337.5 ~ "North-west"
  )))

# 2g Merge weather and NO2 data
no2_full <- no2_full %>% 
  left_join(nldas, by = c("datetime_full" = "datetime_gmt"))


####************************************
#### 3: Load and Prepare PM2.5 Data #### 
####************************************

# 3a Set email and key vars
#    Note: enter your own email and key; JS key in API_Keys.R
email = c("")
key = c("")

# 3b Create tibble of monitor data
# Data entered from EPA's Interactive Map of Air Quality Monitors
# IS52 was selected because it has hourly measures (no other NYC monitors did)
pm2.5 <- tibble(monitor_id = c("36-005-0110", "36-005-0110", "36-005-0110"),
                b_date = c("20180101", "20190101", "20200101"),
                e_date = c("20181231", "20191231", "20201231"),
                data = list(NA))

# 3c Create function to pull PM2.5 for each monitor-year combination 
pull_pm2.5 = function(x) {
  #x=1
  #pm2.5$data[[x]]<-  
  GET(paste0("https://aqs.epa.gov/data/api/sampleData/bySite?email=",email,"&key=",key,"&param=88101&bdate=",pm2.5$b_date[x],"&edate=",pm2.5$e_date[x],"&state=36&county=005&site=0110")) %>% 
    content("text") %>%
    jsonlite::fromJSON() %>% 
    as_tibble() %>% 
    purrr::map_if(., is.data.frame, list) %>% 
    as_tibble() %>% 
    unnest(cols = c(Header, Data))
}

# 3d Pull from API
# Note: Takes about 15 seconds
for (x in 1:3){
  pm2.5$data[[x]] = pull_pm2.5(x)
}

# 3e Unnest, create datetime var and rename pm2.5 var
pm2.5 <- pm2.5 %>% unnest(cols = c(data), names_repair = "unique") %>% 
  mutate(datetime_local = paste0(date_local, time_local),
         datetime_local = lubridate::ymd_hm(datetime_local)) %>% 
  rename(pm2.5 = sample_measurement) 

# 3f Make sure only one pm2.5 observation per datetime
# Note: There is more than one observation per datetime, but we only want one obs
#       per datetime. The dataframe contains two POC values (4 and 1). Since most 
#       observations are POC == 4, we will keep all of these. However, there are n=76
#       observations that are only in POC == 1. We want to keep as many observations as 
#       possible without duplicating datetimes (and without averaging).

poc4 <- pm2.5 %>% filter(poc==4)
poc1 <- pm2.5 %>% filter(poc==1)
sum(poc4$datetime_local %in% poc1$datetime_local)
poc1_only <- subset(poc1, !(datetime_local %in% poc4$datetime_local))
pm2.5 <- pm2.5 %>% filter(poc == 4) %>% 
  bind_rows(poc1_only)
pm2.5$datetime_local[duplicated(pm2.5$datetime_local)] # Check for duplicates --> should be 0

# 3g Select only needed vars
pm2.5 <- pm2.5 %>% 
  dplyr::select(datetime_local, pm2.5)

# 3h Join with hourly no2 data
no2_full <- no2_full %>% 
  left_join(pm2.5, by = c("datetime_local" = "datetime_local"))


####*********************************
#### 4: Descriptives: Covariates #### 
####*********************************

# 4a Time plots of weather vars
no2_full %>% 
  ggplot(aes(x = datetime_local, y = wind_dir_met)) +
  geom_line() +
  geom_smooth()

no2_full %>% 
  ggplot(aes(x = datetime_local, y = wind_speed)) +
  geom_line() +
  geom_smooth()

no2_full %>% 
  ggplot(aes(x = datetime_local, y = temp)) +
  geom_line() +
  geom_smooth()

no2_full %>% 
  ggplot(aes(x = datetime_local, y = spf_humidity)) +
  geom_line() +
  geom_smooth()

no2_full %>% 
  ggplot(aes(x = datetime_local, y = surf_pressure)) +
  geom_line() +
  geom_smooth()

no2_full %>% 
  ggplot(aes(x = datetime_local, y = precip)) +
  geom_line() +
  geom_smooth()

no2_full %>% 
  ggplot(aes(x = datetime_local, y = radiation)) +
  geom_line() +
  geom_smooth()

# 4b Count missing obs for weather vars
# Notes: no missing weather observations
no2_full %>% summarise(count = sum(is.na(wind_dir_met_cat)))
no2_full %>% summarise(count = sum(is.na(wind_speed)))
no2_full %>% summarise(count = sum(is.na(temp)))
no2_full %>% summarise(count = sum(is.na(spf_humidity)))
no2_full %>% summarise(count = sum(is.na(surf_pressure)))
no2_full %>% summarise(count = sum(is.na(precip)))
no2_full %>% summarise(count = sum(is.na(radiation)))

# 4c Time plot of PM2.5
no2_full %>% 
  ggplot(aes(x = datetime_local, y = pm2.5)) +
  geom_line() +
  geom_smooth()

# 4d Count missing obs for PM2.5
# Notes: n=56,052 missing, mostly in 2018
no2_full %>% summarise(count = sum(is.na(pm2.5)))
  

####*************************
#### 5: Save out dataset #### 
####*************************

# 5a Save NO2 dataset with covariates
fwrite(no2_full, "./data/no2_with_covariates.csv")








