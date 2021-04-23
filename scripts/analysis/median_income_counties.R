# script to aggregate census tract level income data to county level
# created: april 22, 2021
# author: @measrainsey

# inputs ---------------

  ces_path      = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/calenviroscreen'
  ces_file      = 'ces3results.xlsx'
  census_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/census/ACSST5Y2019.S1903_2021-03-19T132959_median-income-census-tract'
  census_file   = 'ACSST5Y2019.S1903_data_with_overlays_2021-03-19T132947.csv'

# outputs --------------
  
  save_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/demographics'
  save_file = 'census_median_income_by_county.csv'
  
# load packages -------
  
  library(data.table)
  library(openxlsx)
  
# load census bureau data ------
  
  dt_income = fread(file.path(census_path, census_file), select = c(1, 163), skip = 1)
  setnames(dt_income, 
           c('id', 'Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households'), 
           c('census_tract', 'median_household_income_usd'))
  
# load ces data ----------
  
  dt_ces = as.data.table(read.xlsx(file.path(ces_path, ces_file), sheet = 'CES 3.0 (2018 Update)'))
  dt_ces = dt_ces[, c('Census.Tract', 'California.County')]
  setnames(dt_ces, c('Census.Tract', 'California.County'), c('census_tract', 'county'))
  
  dt_ces[, census_tract := as.character(census_tract)]

# edit census bureau tract codes ------
  
  dt_income[, census_tract := gsub('1400000US0', '', census_tract)]
  
# convert income to numeric -----
  
  dt_income[median_household_income_usd == '250000+', median_household_income_usd := '250000']
  dt_income[, median_household_income_usd := gsub(',', '', median_household_income_usd)]
  
  dt_income[, median_household_income_usd := as.numeric(median_household_income_usd)]

# merge income data with ces cities -----
  
  income_county = dt_income[dt_ces, on = .(census_tract)]
  
  # check missing census tracts without a county:
  # dt_income[!dt_ces, on = .(census_tract)]
  
# get median income at the county level -----
  
  median_income = income_county[, .(median_household_income_usd = median(median_household_income_usd, na.rm = T)), by = .(county)]
  setorder(median_income, county)
  
# export to csv ------
  
  fwrite(median_income, file.path(save_path, save_file), row.names = F)
  