# Convert NLDAS from Grib to CSV format
# F31 NO2 COVID ITS Analysis 
# Sebastian T. Rowland (edited by Jenni A. Shearston)
# Updated 01/14/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation
# 1: Readin All of the Grib Data 
# 2: Split Long Data into Temperature and Humidity


####***************
#### N: Notes  ####
####***************

# Na Description
# From the NLDAS website you can downloand the hourly weather data. 
# However, NLDS will report values via less R-friendly formats 
# (i.e., not csv)
# I downloaded the data in grib format.
# This script converts the data to long format, and saves as a csv

# Nb Directories 
# Since the data was installed from NLDAS site on my personal computer 
# and I have not transferred the individual gribs to this computer 
# The directories are for my personal computer. 

# Nc Downloading process
# I assume that you downloaded each grib file as a individual file - 
# You did not concatenate the files during the terminal command. 
# I also assume you have one folder for each year.

# Nd Notes for this project
# Some weather variable definitions for this project:
#   wind speed: sqrt of meridinial^2+zonal wind speed^2
#   wind direction: create 8 level categorical variable based on pos/neg
#     ratio of two speeds, then use the area tangent (`atan`??) make sure i get
#     the function that goes from 0 to 360 (and not some shorter num of deg)
#   radiation: will only include short wave (solar) radiation 
# Will not pull: CAPE, CONVfac, DLWRF, PEVAP
# To identify which band represents which variable:
#  I pulled each weather variable individually for Jan 1 2018 for NYC 
#  (-74.268,40.470,-73.694,40.928) and recorded the first few observations 
#  for each "band" because the "bands" are not labeled in the data pull. We will 
#  use this to identify which bands represent which variables later in the script
#    APCP:  precipitation hourly total (kg/m^2)
#           first 100 obs are 0
#    DSWRF: shortwave radiation flux downwards (surface)(W/m^2)
#           0 until row 157, then 74.68799, 75.25610, 81.73608, 84.73193, 75.18408
#    PRES:  surface pressure (Pa)
#           first ten obs: 101932.5, 102528.3, 102440.0, 102556.5, 102489.9, 
#           102556.5, 102521.9, 102528.3, 102497.0, 102319.1
#    SPFH:  2-m above ground Specific humidity (kg/kg)
#           first ten obs: 0.0007301972, 0.0007369996, 0.0007310018, 0.0007294000,
#           0.0007742003, 0.0008005977, 0.0007642016, 0.0007410005, 0.0009077967
#           0.0009937987
#    TMP:   2-m above ground Temperature (K)
#           first ten obs: -9.529883, -9.200049, -9.319922, -9.299902, -8.889990,
#           -8.809912, -8.960059, -9.069922, -8.430029, -8.270117
#    UGRD:  10-m above ground Zonal wind speed (m/s)
#           first ten obs: 3.410000, 3.689785, 4.019863, 4.189785, 3.680019, 4.099941,
#           4.619961, 5.010098, 4.059902, 4.369961
#    VGRD:  10-m above ground Meridional wind speed (m/s)
#           first ten obs: -6.100430, -6.400235, -6.730313, -6.890470, -6.060391,
#           -6.770352, -7.480313, -7.910001, -6.200040, -6.919766


####*********************
#### 0: Preparation  ####
####*********************

# 0.a Declare Directories 
raw.data.folder <- '/Users/jennishearston/Desktop/nldas_data/'
NLDAS.data.folder <- '~/Dropbox/Columbia/Research/F31_GoogleTraffic_COVID_CVD/NO2_ITS/NYonPAUSE-NO2-ITS/data/nldas_data/'

# 0.b Load packages
packages <- c("tidyverse", "rgdal")
lapply(packages, library, character.only = TRUE)


####*************************************
#### 1: Readin All of the Grib  Data #### 
####*************************************

# 1a Make a list of all of the grib files in the year's folder 
for(YYYY in 2018:2020){
  raw.grib.folder <- paste0(raw.data.folder,'nldas_', YYYY)
  setwd(raw.grib.folder)
  flist <- list.files(pattern = 'SUB.grb')
  #flist <- flist[1:288]
  
  # 1b Define the function to read in the grib data, convert to dataframe, and define timeHour 
  # This strategy is the most robust because it makes the least assumptions about the structure of your data or 
  # how R works. 
  make.grib.hour <- function( i ){
    readGDAL(flist[i]) %>% 
      data.frame(.) %>% 
      mutate(TimeHour := flist[i]) %>%
      mutate(TimeHour = str_sub(TimeHour, start = 19L, end = -19L))
  }
  
  # 1c Initialize the empty, properly named dataframe that you will fill with the grib data
  L <- make.grib.hour(1) %>% sample_frac(0, replace = TRUE)
  
  # 1d Fill in all of the nldas data
  for (i in 1:length(flist)){ 
    L <- make.grib.hour(i) %>% 
      bind_rows(L,.)
  }
  
  # 1d Rename the variables
  L1 <- L %>% 
    rename(temp = band1, spf_humidity = band2, surf_pressure = band3,
           zonal_wind = band4, merid_wind = band5, precip = band6,
           radiation = band7) %>%
    mutate(nldas_uid = paste0(x, '_', y)) 
  # here we make up an nldas uid to keep track internally

  # 1e Save data
  L1 %>% fst::write_fst(paste0(NLDAS.data.folder, YYYY, '_NLDAS_NYC.fst'))
  
} 













