# aggregate cpuc claims data to city level
# created: march 15, 2021
# author: @measrainsey

# ------------------------------ inputs ------------------------------ 

  data_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/cpuc'
  sel_data    = 'cpuc_claims_selected_programs_2017_2019.csv'
  k12_data    = 'cpuc_claims_k-12_programs_2017_2019.csv'
  zip_path    = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/census'

# outputs -------

  save_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/cpuc'
  save_file     = 'cpuc_claims_city_2017_2019.csv'

# ------------------------------ script ------------------------------ 

# load packages -------

  library(data.table)
  library(stringr)

# load selected programs data ----
  
  dt_sel = fread(file.path(data_path, sel_data), header = T)
  dt_sel[, c('Selected', 'Grouping') := NULL]
  
# load k-12 data ------
  
  dt_k12 = fread(file.path(data_path, k12_data), header = T)
  
# combine selected programs data and k-12 data -----
  
  dt_all = rbind(dt_sel, dt_k12, use.names = T)
  
# normalize city names -----
  
  dt_all[, SiteCity := str_to_title(SiteCity)]
  
# aggregate by city -----
  
  agg_city = dt_all[, lapply(.SD, sum, na.rm = TRUE), by = .(SiteCity), .SDcols = c('TotalFirstYearGrosskW',
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
  
  fwrite(agg_city, file.path(save_path, save_file), row.names = F)
  