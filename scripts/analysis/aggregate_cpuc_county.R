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
  dt_zip[, no_counties := uniqueN(COUNTY), by = ZCTA5]
  dt_zip = dt_zip[, .(ZCTA5, COUNTY, no_counties)]
  dt_zip = unique(dt_zip)
    
  # no_counties_per_zip = dt_zip[, (no_counties = uniqueN(COUNTY)), by = ZCTA5]
  
# import county fips code to county name -----
  
  dt_county = as.data.table(read.xlsx(file.path(zip_path, county_file), startRow = 5))
  colnames(dt_county) = c('summary_level', 'state_fips', 'county_fips', 'county_subdivision_fips', 'place_fips', 'city_fips', 'area_name')
  dt_county = dt_county[state_fips == '06']
  dt_county = dt_county[, .(county_fips, area_name)]
  
# take out claims with zip codes that belong to multiple counties ------
  
  claims_multiple_counties = dt_claims[ZCTA5 %in% unique(dt_zip[no_counties > 1, ZCTA5])]
  claims_multiple_counties = claims_multiple_counties[unique(dt_zip[, .(ZCTA5, no_counties)]), on = .(ZCTA5), nomatch = 0]
  
# separate data table for claims with zip codes that only match one county ------
  
  dt_claims_single = dt_claims[!ZCTA5 %in% unique(dt_zip[no_counties > 1, ZCTA5])]
  
# combine claims data with zip code to county mapping ------

  dt_claims_single_2 = merge(dt_claims_single, dt_zip, 
                             by.x = 'ZCTA5',
                             by.y = 'ZCTA5',
                             all.x = T)
  
  claims_no_county = dt_claims_single_2[is.na(COUNTY)]
  
  dt_claims_single_2 = dt_claims_single_2[!is.na(COUNTY)]
  
  # dt_claims[!ZCTA5 %in% dt_zip[, ZCTA5]]

# combine claims data with county name ------
  
  dt_claims_single_3 = merge(dt_claims_single_2, dt_county, 
                             by.x = 'COUNTY',
                             by.y = 'county_fips',
                             all.x = T)
  
# aggregate by county ------
  
  agg_claims = dt_claims_single_3[, lapply(.SD, sum, na.rm = TRUE), by = .(COUNTY, area_name), .SDcols = c('TotalFirstYearGrosskW',
                                                                                                           'TotalFirstYearGrosskWh',
                                                                                                           'TotalFirstYearGrossTherm',
                                                                                                           'TotalGrossIncentive',
                                                                                                           'TotalGrossMeasureCost',
                                                                                                           'TotalGrossMeasureCost_ER',
                                                                                                           'TotalLifecycleGrosskW',
                                                                                                           'TotalLifecycleGrosskWh',
                                                                                                           'TotalLifecycleGrossTherm') ] 
  
# rename column ----
  
  setnames(agg_claims, 'COUNTY', 'county_fips')
  setnames(agg_claims, 'area_name', 'county')
  
# get unique claims-id + zip + county --------
  
  dt_zip_county = unique(dt_claims_single_3[, .(PrgYear, ClaimID, PrgID, ZCTA5, COUNTY, area_name)])
  setnames(dt_zip_county, 'ZCTA5', 'zip_code')
  setnames(dt_zip_county, 'area_name', 'county_name')
  
# combine list of problematic zip codes and cities without counties ---------
  
  zip_problematic = rbindlist(list(claims_multiple_counties[, .(SiteCity, SiteZipCode)],
                                   claims_no_county[, .(SiteCity, SiteZipCode)]))
  
  zip_problematic = unique(zip_problematic)
  
# export to csv -------

  fwrite(agg_claims, file.path(save_path, save_file), row.names = F)
  fwrite(dt_zip_county, file.path(save_path, 'claims_zip_county_mapping.csv'), row.names = F)
  
  fwrite(claims_no_county, file.path(save_path, "cpuc_claims_without_matched_counties.csv"), row.names = F)
  fwrite(claims_multiple_counties, file.path(save_path, "cpuc_claims_zip_codes_with_multiple_counties.csv"), row.names = F)
  
  fwrite(zip_problematic, file.path(save_path, "cities_and_zips_manual_match.csv"), row.names = F)
  
  