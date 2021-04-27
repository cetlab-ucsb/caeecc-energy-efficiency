# script to gather k-12 relevant variables
# created: april 24, 2021
# author: @measrainsey

# inputs ---------------

  data_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/ca_department_of_education'
  frpm_file     = 'frpm1920.xlsx'
  title1_file   = 'title1pa20alloc5.xlsx'
  lcff_file     = 'lcffsummary1920.xlsx'
  
# outputs --------------
  
  save_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/demographics'
  save_file     = 'k-12_variables_county.csv'
  
# load packages -------
  
  library(data.table)
  library(openxlsx)
  
# read in frpm data ----------
  
  dt_frpm = as.data.table(read.xlsx(file.path(data_path, frpm_file), sheet = 'FRPM School-Level Data ', startRow = 2))
  dt_frpm = dt_frpm[, c('County.Code', 'County.Name', 'District.Code', 'School.Code', 'School.Name', 'Enrollment.(K-12)', 'Free.Meal.Count.(K-12)', 'FRPM.Count.(K-12)')]
  setnames(dt_frpm,
           c('County.Code', 'County.Name', 'District.Code', 'School.Code', 'School.Name', 'Enrollment.(K-12)', 'Free.Meal.Count.(K-12)', 'FRPM.Count.(K-12)'),
           c('county_code', 'county_name', 'district_code', 'school_code', 'school_name', 'enrollment', 'free_meal_count', 'frpm_count'))
  
# read in title 1 data -----
  
  dt_title1 = as.data.table(read.xlsx(file.path(data_path, title1_file), sheet = ' Title 1a', startRow = 14))
  dt_title1 = dt_title1[, c('County.Code', 'County.Name', 'District.Code', 'School.Code')]
  setnames(dt_title1,
           c('County.Code', 'County.Name', 'District.Code', 'School.Code'),
           c('county_code', 'county_name', 'district_code', 'school_code'))
  
# read in local control funding formula (lcff) data ----------
  
  dt_lcff = as.data.table(read.xlsx(file.path(data_path, lcff_file), sheet = 'LCFF Summary 19-20 AN', startRow = 8))
  dt_lcff = dt_lcff[, c('County.Code', 'District.Code', 'School.Code', 'Total.LCFF.Entitlement2')]
  setnames(dt_lcff,
           c('County.Code', 'District.Code', 'School.Code', 'Total.LCFF.Entitlement2'),
           c('county_code', 'District.Code', 'school_code', 'total_lcff_entitlement'))
  
# remove school code = '0000000' --------
  
  # dt_frpm = dt_frpm[! school_code == '0000000']
  # dt_title1 = dt_title1[! school_code == '0000000']
  # dt_lcff = dt_lcff[! school_code == '0000000']
  
# add column for title 1 status -----
  
  dt_title1[, title1_status := 'yes']
  
# remove totals from title 1 dataset -----
  
  dt_title1 = dt_title1[!is.na(county_code)]
  
# remove totals from lcff dataset -----
  
  dt_lcff = dt_lcff[!is.na(school_code)]

# merge frpm data with title 1 data -----
  
  dt_school = merge(dt_frpm, dt_title1, by = c('county_code', 'county_name', 'district_code', 'school_code'), all.x = T)
  
# if title 1 status is NA, change it to 'no' -----
  
  dt_school[is.na(title1_status), title1_status := 'no']
  
# aggregate frpm data to county level -------
  
  agg_frpm = dt_school[, lapply(.SD, sum, na.rm = T), 
                         .SDcols = c("enrollment", "free_meal_count", "frpm_count"), 
                         by = .(county_code, county_name)]
  
  agg_frpm[, perc_eligible_free := free_meal_count/enrollment]
  agg_frpm[, perc_eligible_frpm := frpm_count/enrollment]
  
  agg_frpm = agg_frpm[, .(county_code, county_name, enrollment, free_meal_count, frpm_count, perc_eligible_free, perc_eligible_frpm)]
  
# aggregate title 1 status to county level ------
  
  agg_title1 = dt_school[, .(no_title1_status = .N), by = .(county_code, county_name, title1_status)]
  agg_title1[, number_of_schools := sum(no_title1_status), by = .(county_code, county_name)]
  agg_title1[, perc_title1_status := no_title1_status/number_of_schools]
  agg_title1 = agg_title1[((title1_status == 'yes' & perc_title1_status > 0) | (title1_status == 'no' & perc_title1_status == 1))]
  agg_title1[title1_status == 'no', perc_title1_status := 0]
  agg_title1[title1_status == 'no', no_title1_status := 0]
  
  agg_title1 = agg_title1[, .(county_code, county_name, no_title1_status, number_of_schools, perc_title1_status)]

# aggregate lcff data to county level -----
  
  agg_lcff = dt_lcff[, .(total_lcff_entitlement = sum(total_lcff_entitlement, na.rm = T)), by = .(county_code)]
  
# merge all county-level data -----
  
  dt_county = agg_frpm[agg_title1, on = .(county_code, county_name)]
  dt_county = dt_county[agg_lcff, on = .(county_code)]
  
# export to csv ------
  
  fwrite(dt_county, file.path(save_path, save_file), row.names = F)
  