# Models - ITS Analyses for Play
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 05/18/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: 
# 2: 
# 3: 
# 4: 
# 5: 
# 6: 
# 7: 

####**************
#### N: Notes ####
####**************

# Na Description
# xxxx

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(tidyverse, lubridate)

# 0b Load data
no2_full <- read_csv("./data/no2_with_covariates.csv")


####***********************************************
#### 1: Prepare DF for Unadjusted ITS Analyses #### 
####***********************************************

# 1a Restrict data to NY monitors and 1/1/18 through 6/7/20
# Notes: New York on PAUSE ended on 6/8/20 when Phase 1 reopening began
no2_ITS_1 <- no2_full %>% filter(datetime_full < lubridate::ymd_hms("2020-06-08 00:00:00")) %>% 
  filter(state_code == 36)

# 1b Average monitors per hour
# Notes: Should end with n=21336 (365*24*2 + 159*24)
no2_ITS_1 <- no2_ITS_1 %>% 
  group_by(datetime_full) %>% 
  summarise(mean_no2 = mean(sample_measurement, na.rm = T))

# 1c Create Vars for ITS
no2_ITS_1 <- no2_ITS_1 %>% 
  mutate(time_elapsed = row_number(),
         intervention = case_when(
           datetime_full < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0,
           datetime_full >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1),
         time_of_day = lubridate::hour(datetime_full),
         day_of_week = lubridate::wday(datetime_full, label = TRUE),
         month = lubridate::month(datetime_full),
         year = lubridate::year(datetime_full)) 
no2_ITS_1 <- no2_ITS_1 %>% 
  mutate(time_cat = case_when(
    time_of_day >= 0 & time_of_day < 6   ~ "Midnight to 5 AM",
    time_of_day >= 6 & time_of_day < 12  ~ "6 to 11 AM",
    time_of_day >= 12 & time_of_day < 18 ~ "Noon to 5 PM",
    time_of_day >= 18 & time_of_day < 24 ~ "6 to 11 PM"
  ))

# 1d Review averages (total and by time of day category) 
no2_ITS_1 %>% summarize(mean(mean_no2, na.rm = T))
no2_ITS_1 %>% summarize(sd(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "6 to 11 AM") %>% summarize(mean(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "6 to 11 AM") %>% summarize(sd(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "Noon to 5 PM") %>% summarize(mean(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "Noon to 5 PM") %>% summarize(sd(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "6 to 11 PM") %>% summarize(mean(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "6 to 11 PM") %>% summarize(sd(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "Midnight to 5 AM") %>% summarize(mean(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(time_cat == "Midnight to 5 AM") %>% summarize(sd(mean_no2, na.rm = T))
no2_ITS_1 %>% filter(datetime_full < lubridate::ymd_hms("2020-03-22 00:00:00")) %>% summarize(mean(mean_no2, na.rm = T)) # pre-pandemic mean
no2_ITS_1 %>% filter(datetime_full < lubridate::ymd_hms("2020-03-22 00:00:00")) %>% summarize(sd(mean_no2, na.rm = T))   # pre-pandemic sd


####************************************************************************************
#### 2: Interrupted Time Series - NY only, No Weather, PM, or NJ Station Adjustment #### 
####************************************************************************************

# 2a Run and Evaluate Overall ITS Model (NY monitors only, no weather, PM, or NJ adjustment)
# Notes: Tried using a natural spline to capture long term trends, with df of 10, 13, 
#        and 15 but all left an NA and autocorrelation plots showed a seasonal pattern.
#        Instead added month and year factors and dropped the spline. This helped a little. 
#        Tried an integer time of day variable, a 24-level factor, and a 4-level factor, chose
#        the 24-level factor for the overall model and also stratified by the 4-level factor
#        to look at modification
mod1 <- glm(mean_no2 ~ intervention 
            + time_elapsed 
            + day_of_week 
            + as.factor(time_of_day)
            + as.factor(month)
            + as.factor(year), 
            family = "gaussian", 
            data = no2_ITS_1)
summary(mod1)
mod1_tidy <- broom::tidy(mod1) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))
resids_mod1 <- residuals(mod1,type="deviance") # check residuals 
plot(no2_ITS_1$datetime_local, resids_mod1, ylim = c(-20, 30), pch = 19, cex = 0.7, col = grey(0.6),
     main = "Residuals over time mod1", ylab = "Deviance residuals", xlab = "Datetime")
abline(h = 0, lty = 2, lwd = 2)
acf(resids_mod1) # autocorrelation function
pacf(resids_mod1) # partial autocorrelation function

# 2b Stratify ITS by Time of Day Category (4-levels)
mod1.1 <- glm(mean_no2 ~ intervention 
              + time_elapsed 
              + day_of_week 
              + as.factor(time_of_day)
              + as.factor(month)
              + as.factor(year), 
              family = "gaussian", 
              data = no2_ITS_1[which(no2_ITS_1$time_cat == "6 to 11 AM"),])
summary(mod1.1)
mod1.1_tidy <- broom::tidy(mod1.1) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))

mod1.2 <- glm(mean_no2 ~ intervention 
              + time_elapsed 
              + day_of_week 
              + as.factor(time_of_day)
              + as.factor(month)
              + as.factor(year), 
              family = "gaussian", 
              data = no2_ITS_1[which(no2_ITS_1$time_cat == "Noon to 5 PM"),])
summary(mod1.2)
mod1.2_tidy <- broom::tidy(mod1.2) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))

mod1.3 <- glm(mean_no2 ~ intervention 
              + time_elapsed 
              + day_of_week 
              + as.factor(time_of_day)
              + as.factor(month)
              + as.factor(year), 
              family = "gaussian", 
              data = no2_ITS_1[which(no2_ITS_1$time_cat == "6 to 11 PM"),])
summary(mod1.3)
mod1.3_tidy <- broom::tidy(mod1.3) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))

mod1.4 <- glm(mean_no2 ~ intervention 
              + time_elapsed 
              + day_of_week 
              + as.factor(time_of_day)
              + as.factor(month)
              + as.factor(year), 
              family = "gaussian", 
              data = no2_ITS_1[which(no2_ITS_1$time_cat == "Midnight to 5 AM"),])
summary(mod1.4)
mod1.4_tidy <- broom::tidy(mod1.4) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))


####**************************************************************
#### 3: Interrupted Time Series - NY only, add Weather and PM #### 
####**************************************************************

# 3a Set up dataframe - avg NO2 from all NYC monitors
# Notes: Should end with n=21336 (365*24*2 + 159*24)
no2_ITS_2 <- no2_full %>% filter(datetime_full < lubridate::ymd_hms("2020-06-08 00:00:00")) %>% 
  filter(state_code == 36) %>% dplyr::select(datetime_full, sample_measurement,
                                             wind_direction, wind_speed,
                                             ceiling_height, temperature,
                                             temperature_dewpoint, air_pressure,
                                             AA1_depth, rel_hum, 
                                             pm2.5) %>%
  group_by(datetime_full) %>% 
  summarize(no2 = mean(sample_measurement),
            wind_direction = first(wind_direction),
            wind_speed = first(wind_speed),
            ceiling_height = first(ceiling_height),
            temp = first(temperature),
            temp_dewpt = first(temperature_dewpoint),
            air_pressure = first(air_pressure),
            AA1_depth = first(AA1_depth),
            rel_hum = first(rel_hum),
            pm2.5 = first(pm2.5)) %>% 
  mutate(time_elapsed = row_number(),
         intervention = case_when(
           datetime_full < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0,
           datetime_full >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1),
         time_of_day = lubridate::hour(datetime_full),
         day_of_week = lubridate::wday(datetime_full, label = TRUE),
         month = lubridate::month(datetime_full),
         year = lubridate::year(datetime_full))

# 3b ITS with weather and PM2.5
mod2 <- glm(no2 ~ intervention 
            + time_elapsed 
            + day_of_week 
            + as.factor(time_of_day)
            + as.factor(month)
            + as.factor(year)
            + wind_direction
            + wind_speed
            + ceiling_height
            + temp
            + rel_hum
            + air_pressure
            + AA1_depth
            + pm2.5, 
            family = "gaussian", 
            data = no2_ITS_2)
summary(mod2)
mod2_tidy <- broom::tidy(mod2) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))

# 3c Evaluate model
resids_mod2 <- residuals(mod2,type="deviance") # check residuals 
fits_mod2 <- fitted(mod2)
plot(fits_mod2, resids_mod2, pch = 19, cex = 0.7, col = grey(0.6), ylim = c(-40, 40),
     main = "Fits vs Resids Plot")
abline(h = 0, lty = 2, lwd = 2)
acf(resids_mod2) # autocorrelation function
pacf(resids_mod2) # partial autocorrelation function


####***************************************************************************************
#### 4: Interrupted Time Series - Roadside only (Fort Lee and Queens) w weather and PM #### 
####***************************************************************************************

# 4a Set up dataframe - avg NO2 from all (2) roadside monitors
# Notes: Should end with n=21336 (365*24*2 + 159*24)
no2_ITS_3 <- no2_full %>% filter(datetime_full < lubridate::ymd_hms("2020-06-08 00:00:00")) %>% 
  filter(monitor_name == "Fort Lee Near Road" | monitor_name == "Queens College Near Road" ) %>% 
  dplyr::select(datetime_full, sample_measurement,
                wind_direction, wind_speed, ceiling_height, temperature,
                temperature_dewpoint, air_pressure, AA1_depth, rel_hum, pm2.5) %>%
  group_by(datetime_full) %>% 
  summarize(no2 = mean(sample_measurement),
            wind_direction = first(wind_direction),
            wind_speed = first(wind_speed),
            ceiling_height = first(ceiling_height),
            temp = first(temperature),
            temp_dewpt = first(temperature_dewpoint),
            air_pressure = first(air_pressure),
            AA1_depth = first(AA1_depth),
            rel_hum = first(rel_hum),
            pm2.5 = first(pm2.5)) %>% 
  mutate(time_elapsed = row_number(),
         intervention = case_when(
           datetime_full < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0,
           datetime_full >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1),
         time_of_day = lubridate::hour(datetime_full),
         day_of_week = lubridate::wday(datetime_full, label = TRUE),
         month = lubridate::month(datetime_full),
         year = lubridate::year(datetime_full))

# 4b ITS with weather and PM2.5
mod3 <- glm(no2 ~ intervention 
            + time_elapsed 
            + day_of_week 
            + as.factor(time_of_day)
            + as.factor(month)
            + as.factor(year)
            + wind_direction
            + wind_speed
            + ceiling_height
            + temp
            + rel_hum
            + air_pressure
            + AA1_depth
            + pm2.5, 
            family = "gaussian", 
            data = no2_ITS_3)
summary(mod3)
mod3_tidy <- broom::tidy(mod3) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))

# 4c Evaluate model
resids_mod3 <- residuals(mod3,type="deviance") # check residuals 
fits_mod3 <- fitted(mod3)
plot(fits_mod3, resids_mod3, pch = 19, cex = 0.7, col = grey(0.6), ylim = c(-40, 40),
     main = "Fits vs Resids Plot")
abline(h = 0, lty = 2, lwd = 2)
acf(resids_mod3) # autocorrelation function
pacf(resids_mod3) # partial autocorrelation function


####************************************************************
#### 5: Interrupted Time Series - Add NJ Station Adjustment #### 
####************************************************************
# Notes: Will try subtracting NJ NO2 values from the NYC NO2 time series when the wind is blowing
#        from the SW quadrant (168.75-258.75)
#        Wind direction to degrees chart: http://snowfence.umn.edu/Components/winddirectionanddegrees.htm
#        In scientific and worldwide usage, wind direction is always stated as the direction from which the wind blows. 
#        For example, a south wind blows from the south to the north and a southwest wind blows from southwest to northeast. 
#        The National Weather Service and all U.S. media adhere to the international convention
#        The Fort Lee roadside monitor will not be subtracted out, but may be used in analysis (as exposure)

# 5a Review time plot and correlations of all stations to choose NJ background station
# Notes: Selecting Chester, although Rutgers could also be a good option
no2_full %>% 
  ggplot(aes(x = datetime_full)) +
  geom_smooth(aes(y = sample_measurement, color = monitor_name))

no2_full %>% dplyr::select(sample_measurement, monitor_name) %>%
  group_by(monitor_name) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = monitor_name, values_from = sample_measurement) %>% 
  dplyr::select(-row, -'NA') %>% 
  cor(use = "pairwise.complete.obs") %>% 
  corrplot::corrplot(type = "upper", tl.col = "black", tl.srt = 45)

# 5b Avg NO2 from all NYC monitors & add ITS variables
# Notes: Should end with n=21336 (365*24*2 + 159*24)
no2_ITS_4 <- no2_full %>% filter(datetime_full < lubridate::ymd_hms("2020-06-08 00:00:00")) %>% 
  filter(state_code == 36) %>% 
  dplyr::select(datetime_full, sample_measurement,
                wind_direction, wind_speed, ceiling_height, temperature,
                temperature_dewpoint, air_pressure, AA1_depth, rel_hum, pm2.5) %>%
  group_by(datetime_full) %>% 
  summarize(no2 = mean(sample_measurement),
            wind_direction = first(wind_direction),
            wind_speed = first(wind_speed),
            ceiling_height = first(ceiling_height),
            temp = first(temperature),
            temp_dewpt = first(temperature_dewpoint),
            air_pressure = first(air_pressure),
            AA1_depth = first(AA1_depth),
            rel_hum = first(rel_hum),
            pm2.5 = first(pm2.5)) %>% 
  mutate(time_elapsed = row_number(),
         intervention = case_when(
           datetime_full < lubridate::ymd_hms("2020-03-22 0:00:00") ~ 0,
           datetime_full >= lubridate::ymd_hms("2020-03-22 0:00:00") ~ 1),
         time_of_day = lubridate::hour(datetime_full),
         day_of_week = lubridate::wday(datetime_full, label = TRUE),
         month = lubridate::month(datetime_full),
         year = lubridate::year(datetime_full))

# 5c Add NJ background station as variable
chester <- no2_full %>% filter(monitor_name == "Chester") %>% 
  dplyr::select(datetime_full, sample_measurement) %>% 
  rename(chester_no2 = sample_measurement)

chester$datetime_full[duplicated(chester$datetime_full)] # Check for duplicates --> should be 0

no2_ITS_4 <- no2_ITS_4 %>% 
  left_join(chester, by = "datetime_full")

# 5d Create new no2 variable w Chester value subtracted when winds from SW
no2_ITS_4 <- no2_ITS_4 %>% 
  mutate(adjusted_no2 = ifelse(wind_direction > 168.74 & wind_direction < 258.76, no2 - chester_no2, no2)) %>% 
  relocate(chester_no2, adjusted_no2, .before = wind_direction)         

# 5e ITS with NJ background station subtracted
mod4 <- glm(adjusted_no2 ~ intervention 
            + time_elapsed 
            + day_of_week 
            + as.factor(time_of_day)
            + as.factor(month)
            + as.factor(year)
            + wind_direction
            + wind_speed
            + ceiling_height
            + temp
            + rel_hum
            + air_pressure
            + AA1_depth
            + pm2.5, 
            family = "gaussian", 
            data = no2_ITS_4)
summary(mod4)
mod4_tidy <- broom::tidy(mod4) %>% 
  mutate(ci_l = estimate - (1.96*std.error),
         ci_u = estimate + (1.96*std.error))

# 5f Evaluate model
resids_mod4 <- residuals(mod4,type="deviance") # check residuals 
fits_mod4 <- fitted(mod4)
plot(fits_mod4, resids_mod4, pch = 19, cex = 0.7, col = grey(0.6), ylim = c(-40, 40),
     main = "Fits vs Resids Plot")
abline(h = 0, lty = 2, lwd = 2)
acf(resids_mod4) # autocorrelation function
pacf(resids_mod4) # partial autocorrelation function

