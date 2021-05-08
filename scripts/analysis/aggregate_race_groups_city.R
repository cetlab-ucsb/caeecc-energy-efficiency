# script to aggregate census tract level race data to city level
# created: may 8, 2021
# author: @measrainsey

# inputs ---------------

  ces_path      = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/calenviroscreen'
  ces_file      = 'ces3results.xlsx'
  census_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/census/ACSDT5Y2019.B02001_2021-05-08T135602_race-census-tract'
  census_file   = 'ACSDT5Y2019.B02001_data_with_overlays_2021-04-30T160724.csv'

# outputs --------------

  save_path           = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/demographics'
  save_group_file     = 'census_race_group_by_city.csv'
  save_combined_file  = 'census_race_combined_by_city.csv'
  
# load packages -------

  library(data.table)
  library(openxlsx)

# load census bureau data ------

  dt_race = fread(file.path(census_path, census_file), select = c(1, 3, 5, 7, 9, 11, 13, 15, 17), skip = 1)

# rename columns ------

  setnames(dt_race, c('id', 'Estimate!!Total:'), c('census_tract', 'Total'))
  colnames(dt_race) = gsub('Estimate!!Total:!!', '', colnames(dt_race))
  colnames(dt_race) = gsub(':', '', colnames(dt_race))

# edit census bureau tract codes ------
  
  dt_race[, census_tract := gsub('1400000US0', '', census_tract)]
  
# melt from wide to long ------
  
  dt_long = melt(dt_race, id.vars = 'census_tract', variable.name = 'group', value.name = 'estimate')
  dt_long[, estimate := as.numeric(estimate)]
  
# load ces data ----------

  dt_ces = as.data.table(read.xlsx(file.path(ces_path, ces_file), sheet = 'CES 3.0 (2018 Update)'))
  dt_ces = dt_ces[, c('Census.Tract', 'Nearby.City.(to.help.approximate.location.only)')]
  setnames(dt_ces, c('Census.Tract', 'Nearby.City.(to.help.approximate.location.only)'), c('census_tract', 'city'))
  
  dt_ces[, census_tract := as.character(census_tract)]
  
# merge income data with ces cities -----
  
  race_city =  merge(dt_long, dt_ces, by = 'census_tract', all.x = T, all.y = F)

# get total counts at the city level -----
  
  agg_race = race_city[, .(estimate = sum(estimate, na.rm = T)), by = .(city, group)]
  
# calculate proportion (alone) -----
  
  agg_race[, proportion := estimate/estimate[group == 'Total'], by = .(city)]
  
# aggregate non-white estimates ------
  
  race_city[, combined_group := fcase(group == 'Total', 'Total',
                                      group == 'White alone', 'White')]
  race_city[is.na(combined_group), combined_group := 'Non-white']
  
  agg_combined = race_city[, .(estimate = sum(estimate, na.rm = T)), by = .(city, combined_group)]
  
# calculate proportion (alone) -----
  
  agg_combined[, proportion := estimate/estimate[combined_group == 'Total'], by = .(city)]
  
# export to csv ------
  
  fwrite(agg_race, file.path(save_path, save_group_file), row.names = F)
  fwrite(agg_combined, file.path(save_path, save_combined_file), row.names = F)
  