# extract k-12 programs in cpuc data based on NAICS codes
# created: march 15, 2021
# author: @measrainey

# ------------------------------ inputs ------------------------------ 
  
  prog_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/cedars'
  prog_file     = 'cedars_programs_10nov2020_at_233654_Public Sector_grouped.xlsx - cedars_programs_10nov2020_at_23.csv'
  data_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/cedars_cpuc_confidential/Claims_P2017-2019'
  naics_sel     = c('611112', '485410', '611699', '611113')
  
# outputs -------

  save_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed'
  save_file     = 'cpuc_claims_k-12_programs_2017_2019.csv'

# load packages -------
  
    library(data.table)
  
# read in cpuc claims data ------
  
  list_files = list.files(path = data_path, pattern = '*.csv')
  list_cpuc = list()
  
  for (i in seq_along(list_files)) {
    
    temp = fread(file.path(data_path, list_files[i]), select = c('ClaimID', 
                                                                 'PrgID',
                                                                 'Sector',
                                                                 'SiteCity',
                                                                 'SiteZipCode',
                                                                 'SiteID',
                                                                 'NAICSCode',
                                                                 'BldgHVAC',
                                                                 'BldgLoc',
                                                                 'BldgType',
                                                                 'BldgVint',
                                                                 'E3ClimateZone',
                                                                 'E3GasSavProfile',
                                                                 'E3GasSector',
                                                                 'E3MeaElecEndUseShape',
                                                                 'E3TargetSector',
                                                                 'ImplementationID',
                                                                 'InstallationDate',
                                                                 'NAICSBldgType',
                                                                 'NumUnits',
                                                                 'OBF_Flag',
                                                                 'PrgElement',
                                                                 'RateScheduleElec',
                                                                 'RateScheduleGas',
                                                                 'REN_Flag',
                                                                 'Residential_Flag',
                                                                 'SchoolIdentifier',
                                                                 'TotalFirstYearGrosskW',
                                                                 'TotalFirstYearGrosskWh',
                                                                 'TotalFirstYearGrossTherm',
                                                                 'TotalGrossIncentive',
                                                                 'TotalGrossMeasureCost',
                                                                 'TotalGrossMeasureCost_ER',
                                                                 'TotalLifecycleGrosskW',
                                                                 'TotalLifecycleGrosskWh',
                                                                 'TotalLifecycleGrossTherm',
                                                                 'Upstream_Flag',
                                                                 'WaterOnly_Flag'))
    
    x = gsub("_Claims.csv", "", list_files[i])
    cur_year = substr(x, nchar(x)-3, nchar(x))
    temp[, PrgYear := as.numeric(cur_year)]
    list_cpuc[[i]] = temp
    
    rm(temp,x)
    
  }
  
  dt_cpuc = rbindlist(list_cpuc, use.names = T, fill = T)
  
# extract k-12 programs -----
  
  dt_k12 = dt_cpuc[NAICSCode %chin% naics_sel]
  
# load list of programs -------
  
  dt_prog = fread(file.path(prog_path, prog_file), header = T)
  
# select columns ------
  
  dt_prog = dt_prog[, .(PA, PrgID, ProgramName)]
  
# combine list of k-12 programs with program name -------
  
  dt_merge = dt_k12[dt_prog, on = .(PrgID), nomatch = 0]

# reorder columns -----
  
  setcolorder(dt_merge, c(colnames(dt_merge)[c(39, 1:2, 41, 40, 3:38)]))
  
# export to csv -------
  
  fwrite(dt_merge, file.path(save_path, save_file), row.names = F)
