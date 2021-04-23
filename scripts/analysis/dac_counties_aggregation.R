# script to get median calenviroscreen scores at the county level
# created: april 22, 2021
# author: @measrainsey
# (i think there's a python notebook that does this already)

# inputs ---------------
  
  ces_path  = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/calenviroscreen'
  ces_file  = 'ces3results.xlsx'
  
# outputs --------------
  
  save_path = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/demographics'
  save_file = 'ces_dac_county_proportion_median.csv'
  
# load packages -------

  library(data.table)
  library(openxlsx)

# load ces data ----------
  
  dt_ces = as.data.table(read.xlsx(file.path(ces_path, ces_file), sheet = 'CES 3.0 (2018 Update)'))
  
# keep select columns ------
  
  dt_ces = dt_ces[, c('Census.Tract', 'California.County', 
                      'CES.3.0.Score', 'CES.3.0.Percentile', 'SB.535.Disadvantaged.Community')]
  
# rename columns -----
  
  setnames(dt_ces, 
           c('Census.Tract', 'California.County', 
             'CES.3.0.Score', 'CES.3.0.Percentile', 'SB.535.Disadvantaged.Community'),
           c('census_tract', 'county', 'ces_score', 'ces_percentile', 'dac_flag'))
  
# count number of dac flagged census tracts within each county ------
  
  count_dac = dt_ces[, .(no_tracts = uniqueN(census_tract)), by = .(county, dac_flag)]

# count total number of tracts in each county -----
  
  count_tracts = dt_ces[, .(total = uniqueN(census_tract)), by = .(county)]

# merge counts to calculate dac proportion -----
  
  prop_dac = count_dac[count_tracts, on = .(county)]
  prop_dac[, dac_proportion := no_tracts/total]
  
# keep (a) dac flag = yes and (b) dac flag = no and there are no yes dac flag
  
  prop_dac = prop_dac[(dac_flag == 'Yes') | (dac_flag == 'No' & no_tracts == total)]
  
# for cities with no dac tracts, change dac proportion to 0 -----
  
  prop_dac[(dac_flag == 'No' & no_tracts == total), dac_proportion := 0]
  
# calculate median ces score by county -------
  
  median_ces = dt_ces[, lapply(.SD, median, na.rm = T), .SDcols = c('ces_score', 'ces_percentile'), by = .(county)] 
  setnames(median_ces, 'ces_score', 'ces_score_median')
  setnames(median_ces, 'ces_percentile', 'ces_percentile_median')
  
# combine all county-level variables ------
  
  agg_ces = prop_dac[median_ces, on = .(county)]
  
# select columns ------
  
  agg_ces = agg_ces[, .(county, dac_proportion, ces_score_median, ces_percentile_median)]

# export to csv -----
  
  fwrite(agg_ces, file.path(save_path, save_file), row.names = F)