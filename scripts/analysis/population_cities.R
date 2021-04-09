# script to aggregate census tract level population data to city level
# created: april 9, 2021
# author: @measrainsey

# inputs ---------------

  ces_path      = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/calenviroscreen'
  ces_file      = 'ces3results.xlsx'
  census_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/census/ACSDT5Y2019.B01003_2021-04-09T135130_population-census-tract'
  census_file   = 'ACSDT5Y2019.B01003_data_with_overlays_2021-04-03T163500.csv'

# outputs --------------
  
  save_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/demographics'
  save_file = 'total_population_by_city_2019.csv'
  
# load packages -------
  
  library(data.table)
  library(openxlsx)
  
# load census bureau data ------
  
  dt_pop = fread(file.path(census_path, census_file), select = c(1, 3), skip = 1)
  setnames(dt_pop, 
           c('id', 'Estimate!!Total'), 
           c('census_tract', 'total_population'))
  
# load ces data ----------
  
  dt_ces = as.data.table(read.xlsx(file.path(ces_path, ces_file), sheet = 'CES 3.0 (2018 Update)'))
  dt_ces = dt_ces[, c('Census.Tract', 'Nearby.City.(to.help.approximate.location.only)')]
  setnames(dt_ces, c('Census.Tract', 'Nearby.City.(to.help.approximate.location.only)'), c('census_tract', 'city'))
  
  dt_ces[, census_tract := as.character(census_tract)]

# edit census bureau tract codes ------
  
  dt_pop[, census_tract := gsub('1400000US0', '', census_tract)]
  
# convert population to numeric -----
  
  dt_pop[, total_population := as.numeric(total_population)]

# merge income data with ces cities -----
  
  population_city = dt_pop[dt_ces, on = .(census_tract)]
  
  # check missing census tracts without a city:
  # dt_pop[!dt_ces, on = .(census_tract)]
  # census tract Census Tract 1370 in LA county is the only missing tract with a population > 0 (5067 people)
  
# get total population at the city level -----
  
  sum_population = population_city[, .(total_population = sum(total_population, na.rm = T)), by = .(city)]
  
# export to csv ------
  
  fwrite(sum_population, file.path(save_path, save_file), row.names = F)
  