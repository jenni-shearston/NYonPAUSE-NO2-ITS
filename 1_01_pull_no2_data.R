# Pull NO2 Data from AQS
# F31 NO2 COVID ITS Analysis
# Jenni A. Shearston 
# Updated 05/14/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Enter monitor information
# 2: Replicate rows to create monitor-year specific pulls
# 3: Define variables needed for API pull
# 4: Create function to pull data from API
# 5: Pull data
# 6: Clean data before saving
# 7: Save out data


####**************
#### N: Notes ####
####**************

# Na Description
# We use this script to pull data from the EPA AQS API, AirData 
# for 2018-2020
# Instructions for API use: https://aqs.epa.gov/aqsweb/documents/data_api.html#email


####********************
#### 0: Preparation #### 
####********************

# 0a Load Packages
library(tidyverse)
library(rvest)
library(httr)
library(data.table)

# 0b Register email and get key
#    Note: enter email after 'email=' in GET command below
#GET("https://aqs.epa.gov/data/api/signup?email=")

# 0c Set email and key vars
#    Note: enter your own email and key; JS key in API_Keys.R
email = c("")
key = c("")


####***************************
#### 1: Enter Monitor Info #### 
####***************************

# 1a Create tibble of monitor data
#    Note: Data entered from EPA's Interactive Map of Air Quality Monitors
#          All NO2 monitors with data any years from 2018-2020 are included
#          The Rutgers monitor has only been in operation since 2019

monitors <- tribble(
  ~monitor_id, ~b_year, ~e_year, ~monitor_name,
  "36-005-0133", 2018, 2020, "Pfizer Lab Site",
  "36-005-0110", 2018, 2020, "IS 52",
  "36-081-0124", 2018, 2020, "Queens College 2",
  "36-081-0125", 2018, 2020, "Queens College Near Road",
  "34-003-0010", 2018, 2020, "Fort Lee Near Road",
  "34-017-1002", 2018, 2020, "Jersey City",
  "34-027-3001", 2018, 2020, "Chester",
  "34-023-0011", 2019, 2020, "Rutgers"
)

monitors <- monitors %>% 
  mutate(
    monitor_id = as.character(monitor_id),
    b_year = as.numeric(b_year), e_year = as.numeric(e_year),
    monitor_name = as.character(monitor_name)
  )


####***********************
#### 2: Replicate Rows #### 
####***********************

# 2a Create variable equal to the number of times to replicate 
# each row (monitor), based on how many years of data are available. 
# We want each row to correspond to one year of data from one monitor
# because the API can only pull one year of data at a time 
# from a given monitor.
monitors <- monitors %>% 
  mutate(numb_years = e_year-b_year+1)

# 2b Replicate rows number of times in numb_years variable
monitors <- monitors[rep(seq_len(nrow(monitors)), monitors$numb_years), 1:5]


####**************************************
#### 3: Define Variables for API Pull #### 
####**************************************

# 3a Create sequence variable of number of years of data
monitors <- monitors %>% 
  group_by(monitor_id) %>% 
  mutate(yr_count = seq(1:numb_years)-1) %>% 
  ungroup()

# 3b Create data pull year variable
monitors <- monitors %>% 
  mutate(pull_year = b_year+yr_count)

# 3c Create b_date and e_date variables for API pull
#    Note: Format should be "yearmonthday" 
monitors <- monitors %>% 
  mutate(b_date = paste0(pull_year, "0101"),
         e_date = paste0(pull_year, "1231"))

# 3d Define county and site variables
monitors <- monitors %>% 
  mutate(county = substr(monitor_id, 4, 6),
         site = substr(monitor_id, 8, 11),
         state = substr(monitor_id, 1, 2))

# 3e Add email and key variables to "monitors" tibble
monitors <- monitors %>% 
  mutate(email = email,
         key = key)

# 3f Add data variable to hold lists of data
monitors <- monitors %>% 
  mutate(data = list(NA))


####*****************************************
#### 4: Create Function to Pull from API #### 
####*****************************************

# Create function to pull NO2 for each monitor-year combination 
pull_NO2 = function(x) {
  #x=1
  #monitors$data[[x]]<-  
  GET(paste0("https://aqs.epa.gov/data/api/sampleData/bySite?email=",monitors$email[x],"&key=",monitors$key[x],"&param=42602&bdate=",monitors$b_date[x],"&edate=",monitors$e_date[x],"&state=",monitors$state[x],"&county=",monitors$county[x],"&site=",monitors$site[x])) %>% 
    content("text") %>%
    jsonlite::fromJSON() %>% 
    as_tibble() %>% 
    purrr::map_if(., is.data.frame, list) %>% 
    as_tibble() %>% 
    unnest(cols = c(Header, Data))
}


####**********************
#### 5: Pull from API #### 
####**********************

# 5a Pull from API
#    Note: Pull ten at a time, and wait 10 sec between each pull,
#          so as not to overwhelm the API.
for (x in 1:10){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 11:20){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 21:23){
  monitors$data[[x]] = pull_NO2(x)
}


####*********************************
#### 6: Clean Data Before Saving #### 
####*********************************

# 6a Unnest
monitors2 <- monitors %>% unnest(cols = c(data), names_repair = "unique")

# 6b Check vars
glimpse(monitors2)

# 6c Manage 2 "county" vars
monitors2 <- monitors2 %>% 
  rename(county_name = county...45,
         state_name = state...44) %>% 
  dplyr::select(-county...10, -state...12)

# 6d Drop unneeded vars  
monitors2 <- monitors2 %>% 
  dplyr::select(-yr_count, -b_date, -e_date, -site, -email, -key) 


####**********************
#### 7: Save Out Data #### 
####**********************

# 7a Save out data as csv
#    Note: Large df, may take some time to save
#          do not use write.csv or write_csv - not fast enough
fwrite(monitors2, "./data/no2_nyc_area.csv")

# 7b Clean environment
rm(monitors, monitors2, email, key, x, pull_NO2)


