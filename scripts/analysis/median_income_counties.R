# script to process county level income data
# created: april 22, 2021
# author: @measrainsey

# inputs ---------------

  census_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/census/ACSST5Y2019.S1901_2021-04-23T001437_median-income-county'
  census_file   = 'ACSST5Y2019.S1901_data_with_overlays_2021-04-16T185007.csv'

# outputs --------------
  
  save_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/demographics'
  save_file = 'census_median_income_by_county.csv'
  
# load packages -------
  
  library(data.table)
  library(openxlsx)
  
# load census bureau data ------
  
  dt_income = fread(file.path(census_path, census_file), select = c(2, 25), skip = 1)
  setnames(dt_income, 
           c('Geographic Area Name', 'Estimate!!Households!!Median income (dollars)'), 
           c('county', 'median_household_income_usd'))
  
# edit county names ------
  
  dt_income[, county := gsub(' County, California', '', county)]
  
# convert income to numeric -----
  
  dt_income[, median_household_income_usd := as.numeric(median_household_income_usd)]

# export to csv ------
  
  fwrite(dt_income, file.path(save_path, save_file), row.names = F)
  