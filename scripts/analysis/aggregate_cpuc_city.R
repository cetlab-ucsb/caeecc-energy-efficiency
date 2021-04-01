# aggregate cpuc claims data to city level
# created: march 15, 2021
# author: @measrainsey

# ------------------------------ inputs ------------------------------ 

  data_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/cpuc'
  data_file   = 'cpuc_claims_selected_programs_2017_2019.csv'

# outputs -------

  save_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/cpuc'
  save_file     = 'cpuc_claims_city_2017_2019.csv'

# ------------------------------ script ------------------------------ 

# load packages -------

  library(data.table)
  library(stringr)

# import claims data ------
  
  dt_claims = fread(file.path(data_path, data_file), header = T)

# keep only local governments ------
  
  dt_claims = dt_claims[Grouping %like% 'Local Government']
  
# normalize city names -----
  
  dt_claims[, SiteCity := str_to_title(SiteCity)]
  
# aggregate by city -----
  
  agg_city = dt_claims[, lapply(.SD, sum, na.rm = TRUE), by = .(SiteCity), .SDcols = c('TotalFirstYearGrosskW',
                                                                                    'TotalFirstYearGrosskWh',
                                                                                    'TotalFirstYearGrossTherm',
                                                                                    'TotalGrossIncentive',
                                                                                    'TotalGrossMeasureCost',
                                                                                    'TotalGrossMeasureCost_ER',
                                                                                    'TotalLifecycleGrosskW',
                                                                                    'TotalLifecycleGrosskWh',
                                                                                    'TotalLifecycleGrossTherm') ] 
  setorder(agg_city, SiteCity)
  
# export to csv -------
  
  # fwrite(agg_city, file.path(save_path, save_file), row.names = F)
  