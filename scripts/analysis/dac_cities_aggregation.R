# script to get median calenviroscreen scores at the city level
# created: march 19, 2021
# author: @measrainsey

# inputs ---------------
  
  ces_path  = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/calenviroscreen'
  ces_file  = 'ces3results.xlsx'
  
# outputs --------------
  
  save_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/calenviroscreen'
  save_file = 'ces_dac_city_proportion_median.csv'
  
# load packages -------

  library(data.table)
  library(openxlsx)

# load ces data ----------
  
  dt_ces = as.data.table(read.xlsx(file.path(ces_path, ces_file), sheet = 'CES 3.0 (2018 Update)'))
  
# keep select columns ------
  
  dt_ces = dt_ces[, c('Census.Tract', 'Nearby.City.(to.help.approximate.location.only)', 
                      'CES.3.0.Score', 'CES.3.0.Percentile', 'SB.535.Disadvantaged.Community')]
  
# rename columns -----
  
  setnames(dt_ces, 
           c('Census.Tract', 'Nearby.City.(to.help.approximate.location.only)', 
             'CES.3.0.Score', 'CES.3.0.Percentile', 'SB.535.Disadvantaged.Community'),
           c('census_tract', 'city', 'ces_score', 'ces_percentile', 'dac_flag'))
  
# count number of dac flagged census tracts within each city ------
  
  count_dac = dt_ces[, .(no_tracts = uniqueN(census_tract)), by = .(city, dac_flag)]

# count total number of tracts in each city -----
  
  count_tracts = dt_ces[, .(total = uniqueN(census_tract)), by = .(city)]

# merge counts to calculate dac proportion -----
  
  prop_dac = count_dac[count_tracts, on = .(city)]
  prop_dac[, dac_proportion := no_tracts/total]
  
# keep (a) dac flag = yes and (b) dac flag = no and there are no yes dac flag
  
  prop_dac = prop_dac[(dac_flag == 'Yes') | (dac_flag == 'No' & no_tracts == total)]
  
# for cities with no dac tracts, change dac proportion to 0 -----
  
  prop_dac[(dac_flag == 'No' & no_tracts == total), dac_proportion := 0]
  
# calculate median ces score by city -------
  
  median_ces = dt_ces[, lapply(.SD, median, na.rm = T), .SDcols = c('ces_score', 'ces_percentile'), by = .(city)] 
  setnames(median_ces, 'ces_score', 'ces_score_median')
  setnames(median_ces, 'ces_percentile', 'ces_percentile_median')
  
# combine all city-level variables ------
  
  agg_ces = prop_dac[median_ces, on = .(city)]
  
# select columns ------
  
  agg_ces = agg_ces[, .(city, dac_proportion, ces_score_median, ces_percentile_median)]

# export to csv -----
  
  fwrite(agg_ces, file.path(save_path, save_file), row.names = F)