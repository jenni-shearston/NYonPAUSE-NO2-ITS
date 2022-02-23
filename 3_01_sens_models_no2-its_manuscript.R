# Sensitivity Analyses - ITS Analyses for Manuscript
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 02/22/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Adjust for PM2.5
# 2: Impute missing NO2 observations 
# 3: Remove outliers with residuals >3SD+mean
# 4: Change start date of intervention to March 20
# 5: Remove NJ monitors


####**************
#### N: Notes ####
####**************

# Na Description
# In this script, we complete several sensitivity analyses for the main model only.


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(tidyverse); library(lubridate); library(nlme)
library(forecast); 

# 0b Load data
no2_full <- read_csv("./data/no2_with_covariates.csv")
mod_main <- readRDS("outputs/mod_main.rds")

# 0c Filter dataset, create ITS variables, scale as needed
#    Notes: Do not include Rutgers and Chester, as these are too far away from
#           NYC to really be considered the metro area, and only include dates
#           through the end of NY on PAUSE for NYC (June 8, 2020)
#           Should have a final n = 128,016 
#           6 monitors * 8760 hours in 2018 + 8760 hours in 2019 + 3816 hours in 2020
no2_formods <- no2_full %>% dplyr::select(datetime_full, uniq_mon_dt, monitor_id,
                                          datetime_local, datetime_gmt,
                                          sample_measurement, temp:pm2.5) %>% 
  filter(!monitor_id %in% c("34-027-3001", "34-023-0011")) %>% 
  filter(datetime_full < lubridate::ymd_hms("2020-06-08 05:00:00")) %>% # = to 00:00:00 in local time
  filter(datetime_full > lubridate::ymd_hms("2018-01-01 04:00:00")) %>% # remove Dec 2017 (GMT) obs
  mutate(intervention = as.logical(case_when(
    datetime_local < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0,
    datetime_local >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1)),
    time_of_day = factor(lubridate::hour(datetime_local)),
    day_of_week = factor(lubridate::wday(datetime_local, label = FALSE)),
    weekend = factor(ifelse(day_of_week == 6 | day_of_week == 7, "weekend", "weekday")),
    month = factor(lubridate::month(datetime_local)),
    year = factor(lubridate::year(datetime_local)),
    sample_measurement_scale = scale(sample_measurement, center = T, scale = T),
    wind_speed_scale = scale(wind_speed, center = T, scale = T),
    temp_scale = scale(temp, center = T, scale = T),
    spf_humidity_scale = scale(spf_humidity, center = T, scale = T),
    surf_pressure_scale = scale(surf_pressure, center = T, scale = T),
    precip_scale = scale(precip, center = T, scale = T),
    radiation_scale = scale(radiation, center = T, scale = T),
    pm2.5_scale = scale(pm2.5, center = T, scale = T)) %>% 
  group_by(monitor_id) %>% 
  mutate(time_elapsed = row_number(), 
         time_elapsed_scale = scale(time_elapsed, center = T, scale = T)) %>% 
  ungroup() %>% 
  mutate(time_of_day_cat = as.factor(case_when(
    time_of_day == 0 | time_of_day == 1 ~ "0-1",
    time_of_day == 2 | time_of_day == 3 ~ "2-3",
    time_of_day == 4 | time_of_day == 5 ~ "4-5",
    time_of_day == 6 | time_of_day == 7 ~ "6-7",
    time_of_day == 8 | time_of_day == 9 ~ "8-9",
    time_of_day == 10 | time_of_day == 11 ~ "10-11",
    time_of_day == 12 | time_of_day == 13 ~ "12-13",
    time_of_day == 14 | time_of_day == 15 ~ "14-15",
    time_of_day == 16 | time_of_day == 17 ~ "16-17",
    time_of_day == 18 | time_of_day == 19 ~ "18-19",
    time_of_day == 20 | time_of_day == 21 ~ "20-21",
    time_of_day == 22 | time_of_day == 23 ~ "22-23")))

# 0d Create complete case dataset
#    Note: this is the dataset used for modeling
no2_formods_cc <- no2_formods %>% 
  dplyr::select(sample_measurement_scale, intervention, time_elapsed, day_of_week, 
                time_of_day, month, year, wind_dir_met_cat, wind_speed_scale,
                temp_scale, spf_humidity_scale, surf_pressure_scale, precip_scale,
                radiation_scale, monitor_id, time_of_day_cat, weekend,
                sample_measurement, wind_speed, temp, spf_humidity, surf_pressure,
                precip, radiation) %>% 
  na.omit() # n = 122,308 (4.5% missing)


####*************************
#### 1: Adjust for PM2.5 #### 
####*************************

# 1a Include PM2.5 in dataset
no2_formods_cc_wpm <- no2_formods %>% 
  dplyr::select(sample_measurement_scale, intervention, time_elapsed, day_of_week, 
                time_of_day, month, year, wind_dir_met_cat, wind_speed_scale,
                temp_scale, spf_humidity_scale, surf_pressure_scale, precip_scale,
                radiation_scale, monitor_id, time_of_day_cat, weekend,
                sample_measurement, wind_speed, temp, spf_humidity, surf_pressure,
                precip, radiation, pm2.5) %>% 
  na.omit() # n = 86,093 (32.7% missing)

# 1b Regress PM2.5 on NO2
pm2.5_no2_regress <- lme(sample_measurement ~ pm2.5,
                         random = ~1|monitor_id, data = no2_formods_cc_wpm,
                         method = "ML")

# 1c Save residuals to dataset
no2_formods_cc_wpm <- no2_formods_cc_wpm %>% 
  mutate(pm2.5_resids = residuals(pm2.5_no2_regress))

# 1d Re-run model adjusting for resids of NO2 ~ PM2.5
mod_main_wpm <- lme(sample_measurement ~ intervention + time_elapsed
                           + day_of_week + time_of_day + month + year + wind_dir_met_cat
                           + wind_speed + temp + precip + radiation
                           + spf_humidity + surf_pressure + pm2.5_resids, 
                           random = ~1|monitor_id, data = no2_formods_cc_wpm, 
                           method = "ML")

# 1e Compare effect estimates in models
#    Note: Main results are different -- adjusting for PM2.5 decreased the
#          effect estimate for NY on PAUSE by 2.12 ppb. However, this model also had
#          a dramatically smaller sample size (33% missing)
summary(mod_main)              # main effect = -3.23356; se = 0.121445
summary(mod_main_wpm)          # main effect = -1.1138; se = 0.069096

# 1f Re-run main model (not adjusting for PM2.5), but using the
#    same observations as included in 1d
#    Note: We find a much smaller attenuation when comparing the same observations
mod_main_wpm2 <- lme(sample_measurement ~ intervention + time_elapsed
                    + day_of_week + time_of_day + month + year + wind_dir_met_cat
                    + wind_speed + temp + precip + radiation
                    + spf_humidity + surf_pressure, 
                    random = ~1|monitor_id, data = no2_formods_cc_wpm, 
                    method = "ML")
summary(mod_main_wpm2)        # main effect = -2.98457; se = 0.133073


####****************************************
#### 2: Impute missing NO2 observations #### 
####****************************************

# 2a Review missingness
#    Note: once imputed, should have n=127,916
#          will not be able to use the 100 values with no datetime
missingness <- naniar::miss_var_summary(no2_formods, order = FALSE)

# 2a Create season variable
no2_formods_cc_imputed <- no2_formods %>% 
  mutate(season = case_when(
    month == 1 | month == 2 | month == 12 ~ 'Winter',
    month == 3 | month == 4 | month == 5 ~ 'Spring',
    month == 6 | month == 7 | month == 8 ~ 'Summer',
    month == 9 | month == 10 | month == 11 ~ 'Fall'
  ))

# 2b Calculate values to impute with
#    Note: We will impute missing NO2 observations with the mean
#          NO2 concentration for that observation's year, season (3 month period), 
#          day of week, and hour 
#          This is a total of 2,016 possible mean values: 24 hours * 4 seasons 
#          * 3 years * 7 days of the week
no2_impute_values <- no2_formods_cc_imputed %>% 
  group_by(year, season, day_of_week, time_of_day) %>% 
  summarise(no2_impute_value = mean(sample_measurement, na.rm = T))

# 2c Join imputed values to full dataset
no2_formods_cc_imputed <- no2_formods_cc_imputed %>% 
  left_join(no2_impute_values, by = c('year', 'season', 'day_of_week', 'time_of_day')) %>% 
  dplyr::select(sample_measurement, no2_impute_value, year, season, day_of_week, 
                time_of_day, everything())

# 2d Fill missing observations
no2_formods_cc_imputed <- no2_formods_cc_imputed %>% 
  mutate(no2_imputed = ifelse(is.na(sample_measurement), 
                              no2_impute_value, sample_measurement)) %>% 
  dplyr::select(no2_imputed, everything())

# 2e Restrict to complete case
no2_formods_cc_imputed <- no2_formods_cc_imputed %>%   
  dplyr::select(intervention, time_elapsed, day_of_week, 
              time_of_day, month, year, wind_dir_met_cat, wind_speed_scale,
              temp_scale, spf_humidity_scale, surf_pressure_scale, precip_scale,
              radiation_scale, monitor_id, time_of_day_cat, weekend,
              wind_speed, temp, spf_humidity, surf_pressure,
              precip, radiation, no2_imputed) %>% 
  na.omit() # n = 127,916 (0.08% missing)

# 2e Re-run model with imputed NO2
mod_main_imputed <- lme(no2_imputed ~ intervention + time_elapsed
                        + day_of_week + time_of_day + month + year + wind_dir_met_cat
                        + wind_speed + temp + precip + radiation
                        + spf_humidity + surf_pressure, 
                        random = ~1|monitor_id, data = no2_formods_cc_imputed, 
                        method = "ML")

# 2f Compare effect estimates in models
#    Note: Main results are very similar -- imputing NO2 
#          had basically no effect
summary(mod_main)              # main effect = -3.23356; se = 0.121445
summary(mod_main_imputed)      # main effect = -3.23949; se = 0.118152


####**********************************************
#### 3: Remove outliers with resids >3SD+mean #### 
####**********************************************

# 3a Identify and remove obs with residuals > 3SD + mean
# 3a.i Calculate mean and sd of no2
mean_no2 = mean(no2_formods_cc$sample_measurement)   # 16.24254
sd_no2 = sd(no2_formods_cc$sample_measurement)       # 10.39887
# 3a.ii Pull residuals from main model
no2_formods_cc$residuals = residuals(mod_main)
# 3a.iii Remove residuals greater than 3 SD + mean
no2_formods_cc_nooutliers <- no2_formods_cc %>% 
  filter(residuals < ((3*sd_no2) + mean_no2)) %>% 
  na.omit() # n = 122,289 (n = 19 outliers removed)

# 3b Re-run model without outliers
mod_main_nooutliers <- lme(sample_measurement ~ intervention + time_elapsed
                           + day_of_week + time_of_day + month + year + wind_dir_met_cat
                           + wind_speed + temp + precip + radiation
                           + spf_humidity + surf_pressure, 
                           random = ~1|monitor_id, data = no2_formods_cc_nooutliers, 
                           method = "ML")

# 3c Compare effect estimates in models
#    Note: Main results are very similar -- removing outliers increased
#          effect estimate for NY on PAUSE slightly, with slightly decreased se
summary(mod_main)              # main effect = -3.23356; se = 0.121445
summary(mod_main_nooutliers)   # main effect = -3.25015; se = 0.120839


####******************************************
#### 4: Change start date of intervention #### 
####******************************************

# 4a Create new intervention variable
no2_formods_cc_newintervention <- no2_formods %>% 
  mutate(intervention2 = as.logical(case_when(
    datetime_local < lubridate::ymd_hms("2020-03-20 0:00:00") ~ 0,
    datetime_local >= lubridate::ymd_hms("2020-03-20 0:00:00") ~ 1))) %>% 
  dplyr::select(sample_measurement_scale, intervention, time_elapsed, day_of_week, 
              time_of_day, month, year, wind_dir_met_cat, wind_speed_scale,
              temp_scale, spf_humidity_scale, surf_pressure_scale, precip_scale,
              radiation_scale, monitor_id, time_of_day_cat, weekend,
              sample_measurement, wind_speed, temp, spf_humidity, surf_pressure,
              precip, radiation, intervention2) %>% 
  na.omit() # n = 122,308 (4.5% missing)

# 4b Re-run model with new intervention variable
mod_main_march20 <- lme(sample_measurement ~ intervention2 + time_elapsed
                           + day_of_week + time_of_day + month + year + wind_dir_met_cat
                           + wind_speed + temp + precip + radiation
                           + spf_humidity + surf_pressure, 
                           random = ~1|monitor_id, data = no2_formods_cc_newintervention, 
                           method = "ML")

# 4c Compare effect estimates in models
#    Note: Main results are very similar -- changing the start date increased
#          effect estimate for NY on PAUSE slightly, with slightly decreased se
summary(mod_main)         # main effect = -3.23356; se = 0.121445
summary(mod_main_march20) # main effect = -3.27666; se = 0.120639


####***************************
#### 5: Remove NJ monitors #### 
####***************************

# 5a Filter dataset to only NY monitors
no2_formods_cc_nyonly <- no2_formods_cc %>% 
  filter(!monitor_id %in% c("34-003-0010", "34-017-1002"))

# 5b Re-run model with only NY monitors
#    Note: get the below error with lme; able to run with lme4
#          'nlminb problem, convergence error code = 1
#           message = false convergence (8)'
mod_main_nyonly <- lme(sample_measurement ~ intervention + time_elapsed
                        + day_of_week + time_of_day + month + year + wind_dir_met_cat
                        + wind_speed + temp + precip + radiation
                        + spf_humidity + surf_pressure, 
                        random = ~1|monitor_id, data = no2_formods_cc_nyonly, 
                        method = "ML")
mod_main_nyonly2 <- lme4::lmer(sample_measurement ~ intervention + time_elapsed
           + day_of_week + month + year + wind_dir_met_cat + time_of_day
           + wind_speed + temp_scale + precip + radiation
           + spf_humidity + surf_pressure +
             (1|monitor_id), data = no2_formods_cc_nyonly)

# 5c Compare effect estimates in models
#    Note: Main results are somewhat similar -- removing NJ monitors decreased the
#          effect estimate for NY on PAUSE by 0.05
summary(mod_main)         # main effect = -3.23356; se = 0.121445
summary(mod_main_nyonly2) # main effect from lme4 model = -3.18; se = 0.1437
                          # lci = -3.18 - 0.1437*1.96; uci = -3.18 + 0.1437*1.96




