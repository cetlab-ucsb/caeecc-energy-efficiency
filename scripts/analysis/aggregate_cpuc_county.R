# aggregate cpuc claims data to county level
# created: february 5, 2021
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  data_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed'
  data_file   = 'cpuc_claims_selected_programs_2017_2019.csv'
  zip_path    = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/census'
  zip_file    = 'zcta_county_rel_10.txt'
  county_file = 'all-geocodes-v2018.xlsx'
  
# outputs -------
  
  save_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed'
  save_file     = 'cpuc_claims_county_2017_2019.csv'
  
# ------------------------------ script ------------------------------ 
  
# load packages -------
  
  library(data.table)
  library(openxlsx)
  
# import claims data ------
  
  dt_claims = fread(file.path(data_path, data_file), header = T)
  dt_claims[, ZCTA5 := substr(SiteZipCode, 1, 5)]
  
# keep only local governments ------
  
  dt_claims = dt_claims[Grouping %like% 'Local Government']
  
# import zip code to county mapping ------
  
  dt_zip = fread(file.path(zip_path, zip_file), header = T, colClasses = rep("character", 24))
  dt_zip = dt_zip[STATE == '06']
  dt_zip = dt_zip[, .(ZCTA5, COUNTY)]
  dt_zip = unique(dt_zip)
  
# import county fips code to county name -----
  
  dt_county = as.data.table(read.xlsx(file.path(zip_path, county_file), startRow = 5))
  colnames(dt_county) = c('summary_level', 'state_fips', 'county_fips', 'county_subdivision_fips', 'place_fips', 'city_fips', 'area_name')
  dt_county = dt_county[state_fips == '06']
  dt_county = dt_county[, .(county_fips, area_name)]
  
# combine claims data with zip code to county mapping ------
  
  dt_claims_2 = merge(dt_claims, dt_zip, 
                      by.x = 'ZCTA5',
                      by.y = 'ZCTA5',
                      all.x = T,
                      all.y = F,
                      allow.cartesian = T)
  
  dt_claims_2 = dt_claims_2[!is.na(COUNTY)]
  
  # dt_claims[!ZCTA5 %in% dt_zip[, ZCTA5]]

# combine claims data with county name ------
  
  dt_claims_3 = merge(dt_claims_2, dt_county, 
                      by.x = 'COUNTY',
                      by.y = 'county_fips',
                      all.x = T,
                      all.y = F,
                      allow.cartesian = T)
  
# aggregate by county ------
  
  agg_claims = dt_claims_3[, lapply(.SD, sum, na.rm = TRUE), by = 'area_name', .SDcols = c('TotalFirstYearGrosskW',
                                                                                           'TotalFirstYearGrosskWh',
                                                                                           'TotalFirstYearGrossTherm',
                                                                                           'TotalGrossIncentive',
                                                                                           'TotalGrossMeasureCost',
                                                                                           'TotalGrossMeasureCost_ER',
                                                                                           'TotalLifecycleGrosskW',
                                                                                           'TotalLifecycleGrosskWh',
                                                                                           'TotalLifecycleGrossTherm') ] 
  
# rename column ----
  
  setnames(agg_claims, 'area_name', 'county')
  
# get unique claims-id + zip + county --------
  
  dt_zip_county = unique(dt_claims_3[, .(PrgYear, ClaimID, PrgID, ZCTA5, COUNTY, area_name)])
  setnames(dt_zip_county, 'ZCTA5', 'zip_code')
  setnames(dt_zip_county, 'area_name', 'county_name')
  
# export to csv -------

  fwrite(agg_claims, file.path(save_path, save_file), row.names = F)
  fwrite(dt_zip_county, file.path(save_path, 'claims_zip_county_mapping.csv'), row.names = F)
  