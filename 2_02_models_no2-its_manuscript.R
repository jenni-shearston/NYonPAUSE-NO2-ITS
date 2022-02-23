# Models - ITS Analyses for Manuscript
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 01/19/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Determine choice of main model 
#    (random intercepts OR random intercepts and random slopes)
# 2: Run Main Model and evaluate
# 3: Hourly stratification
# 4: Weekday/weekend stratification
# 5: Roadside monitors interaction


####**************
#### N: Notes ####
####**************

# Na Description
# xxxx

####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(tidyverse); library(lubridate); library(nlme)
library(forecast); 

# 0b Load data
no2_full <- read_csv("./data/no2_with_covariates.csv")

# 0c Filter dataset, create ITS variables, scale as needed
#    Notes: Do not include Rutgers and Chester, as these are too far away from
#           NYC to really be considered the metro area, and only include dates
#           through the end of NY on PAUSE for NYC (June 8, 2020)
#           Should have a final n = 128,016 
#           6 monitors * 8760 hours in 2018 + 8760 hours in 2019 + 3816 hours in 2020
#           Reminder: datetime_full is the only time var that is not missing for all
#                     obs, and it is in GMT. We want to filter the dataset using  
#                     datetime_full to keep all cases, but we need to adjust it to 
#                     local time
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
         #ceiling_height_scale = scale(ceiling_height, center = T, scale = T),
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

# 0d Check distribution of outcome
#    Notes: A little bit right skewed
no2_formods %>% ggplot(aes(x = sample_measurement)) + geom_histogram()
mean(no2_formods$sample_measurement, na.rm = T)   # 16.24254
median(no2_formods$sample_measurement, na.rm = T) # 13.5
sd(no2_formods$sample_measurement, na.rm = T)     # 10.39887
IQR(no2_formods$sample_measurement, na.rm = T)    # 13.3
no2_formods %>% ggplot(aes(x = sample_measurement_scale)) + geom_histogram()

# 0e Review time series plot
#    Notes: daily (2 daily peaks), weekly, and yearly patterns
# 0e.i Full time series
no2_formods %>% ggplot(aes(x = datetime_full, y = sample_measurement)) +
  geom_line()
# 0e.ii Zoom in by month (2018)
no2_formods %>% filter(month == "3" & year == "2018" | month == "4" & year == "2018") %>% 
  ggplot(aes(x = datetime_full, y = sample_measurement)) + geom_line() + 
  facet_wrap(~month, scales = "free")
# 0e.iii Zoom in on a couple of days (Jan 2 2018 & Jan 3 2018)
no2_formods %>% filter(time_elapsed > 24 & time_elapsed < 49) %>% 
  ggplot(aes(x = datetime_local, y = sample_measurement)) + geom_line()
no2_formods %>% filter(time_elapsed > 48 & time_elapsed < 73) %>% 
  ggplot(aes(x = datetime_local, y = sample_measurement)) + geom_line()

# 0f Look at autocorrelation
acf(no2_formods$sample_measurement, na.action = na.pass, lag = 100)
pacf(no2_formods$sample_measurement, na.action = na.pass, lag = 100)


####***************************************
#### 1: Determine Choice of Main Model #### 
####***************************************

# Notes: -Here we run a fixed effects model, a mixed effects model with only random
#         intercepts, and a mixed effects model with both random intercepts and slopes.
#        -We used AIC to determine which model had better fit.
#        -We concluded that XXX was the way to go, because ... 
#        -I used these guides as a reference when creating and testing the models:
#         https://slcladal.github.io/regression.html#Random_Effects
#         https://dfzljdn9uc3pi.cloudfront.net/2020/9522/1/MixedModelDiagnostics.html
#        -There was an error when running the random effect and slope model in the lme4 
#         package without scaling, so I coded models with centered & scaled numeric vars
#         and models without. In nlme I could run all three models without the 
#         centering and scaling, so we use those models because of their interpretability
#        -Also in the lme4 package, the random intercept and slope model would not 
#         converge when time_of_day had 24 levels. Ended up using the nlme package.
#        -Because of high missingness, I do not include PM2.5 in the main model

# 1a Filter dataset to complete observations
#    Notes: This is done to make it easier to run the diagnostic plots and so 
#           that we can compare AIC values between models
no2_formods_cc <- no2_formods %>% 
  dplyr::select(sample_measurement_scale, intervention, time_elapsed, day_of_week, 
                time_of_day, month, year, wind_dir_met_cat, wind_speed_scale,
                temp_scale, spf_humidity_scale, surf_pressure_scale, precip_scale,
                radiation_scale, monitor_id, time_of_day_cat, weekend,
                sample_measurement, wind_speed, temp, spf_humidity, surf_pressure,
                precip, radiation) %>% 
  na.omit() # n = 122,308 (4.5% missing)

# 1b Create fixed effects model (FE)
mod_mainFE_s = glm(sample_measurement_scale ~ intervention + time_elapsed
               + day_of_week + time_of_day + month + year + wind_dir_met_cat
               + wind_speed_scale + temp_scale + precip_scale + radiation_scale
               + spf_humidity_scale + surf_pressure_scale,
               data = no2_formods_cc, family = "gaussian")
mod_mainFE = glm(sample_measurement ~ intervention + time_elapsed
                 + day_of_week + time_of_day + month + year + wind_dir_met_cat
                 + wind_speed + temp + precip + radiation
                 + spf_humidity + surf_pressure,
                 data = no2_formods_cc, family = "gaussian")

# 1c Create mixed effects model with random intercepts (RI)
mod_mainRI_s <- lme(sample_measurement_scale ~ intervention + time_elapsed
                  + day_of_week + time_of_day + month + year + wind_dir_met_cat
                  + wind_speed_scale + temp_scale + precip_scale + radiation_scale
                  + spf_humidity_scale + surf_pressure_scale, 
                  random = ~1|monitor_id, data = no2_formods_cc, method = "ML")
mod_mainRI <- lme(sample_measurement ~ intervention + time_elapsed
                  + day_of_week + time_of_day + month + year + wind_dir_met_cat
                  + wind_speed + temp + precip + radiation
                  + spf_humidity + surf_pressure, 
                  random = ~1|monitor_id, data = no2_formods_cc, method = "ML")
ranef(mod_mainRI)

# 1d Create mixed effects model with random intercepts and slopes (RIS)
#    Notes: Received a warning that this model failed to converge when using
#           the 24 category time of day variable and the 7 level weekday variable
#           when using the lme4 package, but it is fine with nlme
mod_mainRIS_s <- lme(sample_measurement_scale ~ intervention + time_elapsed 
                   + day_of_week + time_of_day + month + year+ wind_dir_met_cat
                   + wind_speed_scale + temp_scale + precip_scale + radiation_scale
                   + spf_humidity_scale + surf_pressure_scale,
                   random = ~1+intervention|monitor_id, method = "ML", 
                   data = no2_formods_cc)
mod_mainRIS <- lme(sample_measurement ~ intervention + time_elapsed 
                   + day_of_week + time_of_day + month + year+ wind_dir_met_cat
                   + wind_speed + temp + precip + radiation
                   + spf_humidity + surf_pressure,
                   random = ~1+intervention|monitor_id, method = "ML", 
                   data = no2_formods_cc)
ranef(mod_mainRIS)
saveRDS(mod_mainRIS, "outputs/mod_mainRIS.rds")

# 1e Compare fixed vs mixed effects models
#    Notes: While adding random intercepts substantially reduces the AIC value,
#           adding random slopes improves the AIC by a smaller value
#           Will choose to use XXXX
# 1e.i Centered and scaled models
AIC(logLik(mod_mainFE_s))                                   # AIC = 278182.9
AIC(logLik(mod_mainRI_s))                                   # AIC = 269243.7 
AIC(logLik(mod_mainRIS_s))                                  # AIC = 269235.8
anova(mod_mainRI_s, mod_mainRIS_s)
# 1e.ii Not centered and scaled
AIC(logLik(mod_mainFE))                                   # AIC = 850999.5
AIC(logLik(mod_mainRI))                                   # AIC = 842060.3 
AIC(logLik(mod_mainRIS))                                  # AIC = 842052.4
anova(mod_mainRI, mod_mainRIS)

####***************************************
#### 2: Run and Evaluate Main Model #### 
####***************************************

# Notes: 

# 2a Run main model
mod_main = lme(sample_measurement ~ intervention + time_elapsed
                  + day_of_week + time_of_day + month + year + wind_dir_met_cat
                  + wind_speed + temp + precip + radiation
                  + spf_humidity + surf_pressure, 
                  random = ~1|monitor_id, data = no2_formods_cc, method = "ML")

# 2b Create dataframe for model diagnostics
no2_fordiag <- no2_formods_cc %>% 
  mutate(fitted = fitted(mod_main),
         resids = residuals(mod_main),
         resids_scaled = scale(resids))

# 2c Check heteroscedasticity using resids vs fitted values plot
#    Notes: A bit of a funneling shape and strange linear line
#           in the bottom left quadrant
#           I did some exploring below, but still no idea why linear shape
#           is present for low NO2 concentrations
#           Since this is for quite low NO2 concentrations, and is
#           slightly different by monitor, could it be detection limits?
#           Also tried removing obs with resids above 5 (code not included),
#           but this did not help
# 2c.i All monitors
no2_fordiag %>% ggplot(aes(x = fitted, y = resids_scaled)) +
  geom_point() + geom_hline(yintercept = 0) + xlab("Fitted values") + 
  ylab("Standardized residuals") + theme_bw()
# 2c.ii Colored by monitor_id
no2_fordiag %>% ggplot(aes(x = fitted, y = resids_scaled, color = monitor_id)) +
  geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Fitted values") + 
  ylab("Standardized residuals") + theme_bw() + geom_abline(slope = -.12, intercept = .8)
# 2c.iii Create dataframe zoomed in on linear area
no2_funnel <- no2_fordiag %>% filter(resids_scaled < 2 & fitted < 2) %>% 
  mutate(zone = (-.12*fitted)+.8, # estimate linear equation
         inzone = ifelse(resids_scaled > zone, F, T)) %>% 
  filter(inzone == T) # filter to obs near linear equation
# 2c.iv Repeat resids vs fit plot
no2_funnel %>% ggplot(aes(x = fitted, y = resids_scaled, color = monitor_id)) +
  geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Fitted values") + 
  ylab("Standardized residuals") + theme_bw() + 
  geom_abline(slope = -.12, intercept = .8) + facet_wrap(~monitor_id)
# 2c.v Review obs near linear area
no2_funnel %>% filter(monitor_id == "34-003-0010") %>% summary() # Fort Lee
no2_funnel %>% filter(monitor_id == "34-017-1002") %>% summary() # Jersey City
no2_funnel %>% filter(monitor_id == "36-005-0110") %>% summary() # IS52
no2_funnel %>% filter(monitor_id == "36-005-0133") %>% summary() # Pfizer
no2_funnel %>% filter(monitor_id == "36-081-0124") %>% summary() # Queens
no2_funnel %>% filter(monitor_id == "36-081-0125") %>% summary() # Queens Near Road
# 2c.vi Look at detection limits
table(no2_full$monitor_id, no2_full$detection_limit)

# 2d Check for non-linearity using resids vs explanatory vars plots
#    Notes: time_elapsed, temp, spf_humidity, surf_pressure, radiation looked good
#           wind_speed and precip are very funnelled
#           ASK MAK: add splines? do we care?
# 2d.i time_elapsed & other vars
no2_fordiag %>% ggplot(aes(x = wind_speed, y = resids_scaled, color = monitor_id)) +
  geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Explanatory Var") + 
  ylab("Standardized residuals") + theme_bw()
# 2d.ii Run non-linear version to see if resids vs explanatory vars plots
#       improve
#       Note: the resids vs explanatory plots dont really improve
library(splines)
mod_main_ns = lme(sample_measurement ~ intervention + time_elapsed
                  + day_of_week + time_of_day + month + year + wind_dir_met_cat
                  + ns(wind_speed, df = 3) + temp + precip + I(precip^2) + radiation
                  + spf_humidity + surf_pressure, 
                  random = ~1|monitor_id, data = no2_formods_cc, method = "ML")
no2_fordiag_ns <- no2_formods_cc %>% 
  mutate(resids = residuals(mod_main_ns),
         resids_scaled = scale(resids))
no2_fordiag_ns %>% ggplot(aes(x = precip, y = resids_scaled, color = monitor_id)) +
  geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Explanatory Var") + 
  ylab("Standardized residuals") + theme_bw()
# 2e Check for heteroscedasticity by level of random effects
#    Notes: Looks good
no2_fordiag %>% ggplot(aes(x = fitted, y = resids_scaled, color = monitor_id)) +
  geom_point(alpha = 0.25) + geom_hline(yintercept = 0) + xlab("Fitted values") + 
  ylab("Standardized residuals") + theme_bw() + facet_wrap(~ monitor_id)

# 2f Check that residuals are normally distributed (QQ Plot)
#    Notes: Looks fairly good, some deviation away from the line at the upper
#           quantiles
qqnorm(no2_fordiag$resids, pch = 20, col = "black")
qqline(no2_fordiag$resids)

# 2g Check for influential datapoints
#    Notes: monitor 34-003-0010 (Fort Lee) has a large number of residual 
#           outliers and monitor 34-017-1002 also has some outliers, but
#           I have not figured out how to calculate leverage or Cooks D
#           to determine if they are influential. MAK and I have 
#           reviewed outliers, and they all occur before the intervention;
#           we think they are probably okay
# 2g.i Look at boxplots of residuals by monitor_id
plot(mod_main, monitor_id ~ resid(., scaled=TRUE), abline=0, pch=16,
     xlab = "Standardised residuals", ylab = "Monitor ID")
# 2g.ii Review resids > 3 SD from the mean
mean_no2 <- mean(no2_formods_cc$sample_measurement)
sd_no2 <- sd(no2_formods_cc$sample_measurement)
no2_fordiag %>% filter(resids > (mean_no2+(sd_no2*3))) %>% View()
# 2g.iii tried and failed packages to calculate leverage and cooks d
# predictmeans::CookD(mod_main)
# infl <- HLMdiag::hlm_influence(mod_main, level = 1)
# car::influence.lme(mod_main)
# car::cooks.distance(influence(mod_main))
# car::infIndexPlot(influence(mod_main, obs = TRUE))

# 2h Check that the random effect distribution is normal
#    Notes: Looks fairly good
hist(ranef(mod_main)[,1])

# 2i Check for autocorrelation
#    Notes: still quite a bit of autocorrelation, through ~ lag 30; 
#           partial acf suggests the autocorrelation at lower lags
#           is mostly explained by the autocorrelation at higher lags
# 2i.i Run ACF/PACF plots
acf(no2_fordiag$resids)
pacf(no2_fordiag$resids)
# 2i.ii Try adding correlation structure to model
#       Note; get an error stating it is too large 
#             "Error: 'sumLenSq := sum(table(groups)^2)' = 2.49449e+09 is too large."
mod_main_arma <- update(mod_main, correlation = corAR1(form = ~ 1|monitor_id))

# 2j Save model
saveRDS(mod_main, "outputs/mod_main.rds")


####******************************
#### 3: Hourly Stratification #### 
####******************************

# 3a Create function for model
#    Notes: radiation is dropped due to rank deficiency 
#           To determine which var was causing the full model not to run,
#           used lme4::lmer (code below) because it gives a warning and allows the 
#           model to run, so I could pick out the problematic variable, as per
#           https://stackoverflow.com/questions/68028979/error-in-meemobject-conlin-controlniterem-singularity-in-backsolve-at-lev
hourly_lme = function(df) {
  lme(sample_measurement ~ intervention + time_elapsed
      + day_of_week + month + year + wind_dir_met_cat
      + wind_speed + temp + precip
      + spf_humidity + surf_pressure,
      random = ~1|monitor_id, data = df, method = "ML")
}

# 3b Run models for all hours
mod_hourly = no2_formods_cc %>% dplyr::select(-time_elapsed) %>% 
  group_by(time_of_day, monitor_id) %>% 
  mutate(time_elapsed = row_number()) %>% 
  ungroup() %>% group_by(time_of_day) %>% 
  nest() %>% 
  mutate(mods = map(data, hourly_lme))

# 3Extra: Run lme4::lmer model to determine which var was causing model to fail 
# lme4::lmer(sample_measurement_scale ~ intervention + time_elapsed
#            + day_of_week + month + year + wind_dir_met_cat
#            + wind_speed_scale + temp_scale + precip_scale + radiation_scale
#            + spf_humidity_scale + surf_pressure_scale +
#            (1|monitor_id), data = mod_hourly$data[[1]])  

# 3c Save models
saveRDS(mod_hourly, "outputs/mod_hourly.rds")


####***************************************
#### 4: Weekday/weekend Stratification #### 
####***************************************

# 4a Create function for model
weekend_lme = function(df) {
  lme(sample_measurement ~ intervention + time_elapsed
      + month + year + wind_dir_met_cat + time_of_day
      + wind_speed + temp + precip
      + spf_humidity + surf_pressure + radiation,
      random = ~1|monitor_id, data = df, method = "ML")
}

# 4b Run models for weekend and weekdays
mod_weekend = no2_formods_cc %>% dplyr::select(-time_elapsed) %>% 
  group_by(weekend, monitor_id) %>% 
  mutate(time_elapsed = row_number()) %>% 
  ungroup() %>% group_by(weekend) %>% 
  nest() %>% 
  mutate(mods = map(data, weekend_lme))

# 4c Save models
saveRDS(mod_weekend, "outputs/mod_weekend.rds")

####*****************************************
#### 5: Road-side Monitor Stratification #### 
####*****************************************

# 5a Create function for model
roadside_lme = function(df) {
  lme(sample_measurement ~ intervention + time_elapsed
      + month + year + wind_dir_met_cat + time_of_day
      + wind_speed + temp + precip + day_of_week
      + spf_humidity + surf_pressure + radiation,
      random = ~1|monitor_id, data = df, method = "ML")
}

# 5b Run models for roadside and non-roadside
mod_roadside = no2_formods_cc %>% dplyr::select(-time_elapsed) %>% 
  mutate(roadside = ifelse(monitor_id == "36-081-0125" | monitor_id == "34-003-0010",
                           "Roadside", "Non-roadside")) %>% 
  group_by(monitor_id) %>% 
  mutate(time_elapsed = row_number()) %>% 
  ungroup() %>% group_by(roadside) %>% 
  nest() %>% 
  mutate(mods = map(data, roadside_lme))

# 5c Save models
saveRDS(mod_roadside, "outputs/mod_roadside.rds")







