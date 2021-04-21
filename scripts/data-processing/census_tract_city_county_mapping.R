# create mapping of census tracts to cities to counties using calenviroscreen
# created: april 21, 2021
# author: @measrainsey

# inputs ---------------

  ces_path      = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/calenviroscreen'
  ces_file      = 'ces3results.xlsx'

# outputs --------------
  
  save_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/calenviroscreen'
  save_file = 'census_tract_city_county_mapping.csv'
  
# load packages -------
  
  library(data.table)
  library(openxlsx)
  
# load ces data ----------
  
  dt_ces = as.data.table(read.xlsx(file.path(ces_path, ces_file), sheet = 'CES 3.0 (2018 Update)'))
  dt_ces = dt_ces[, c('Census.Tract','ZIP', 'Nearby.City.(to.help.approximate.location.only)', 'California.County')]
  setnames(dt_ces, 
           c('Census.Tract','ZIP', 'Nearby.City.(to.help.approximate.location.only)', 'California.County'), 
           c('census_tract', 'zip_code', 'city', 'county'))
  
  dt_ces[, census_tract := as.character(census_tract)]

# get unique combos ------
  
  dt_ces = unique(dt_ces)

# export to csv ------
  
  fwrite(dt_ces, file.path(save_path, save_file), row.names = F)
  