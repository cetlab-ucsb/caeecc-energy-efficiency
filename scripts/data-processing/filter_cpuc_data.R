# filter out CPUC data for programs highlighted by chris
# created: february 5, 2021
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  prog_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/cedars'
  prog_file     = 'cedars_programs_10nov2020_at_233654_Public Sector_grouped.xlsx - cedars_programs_10nov2020_at_23.csv'
  data_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/cedars_cpuc_confidential/Claims_P2017-2019'

# outputs -------
  
  save_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed'
  save_file     = 'cpuc_claims_selected_programs_2017_2019.csv'
  
# ------------------------------ script ------------------------------ 
  
# load packages -------
  
  library(data.table)
  
# load list of programs -------
  
  dt_prog = fread(file.path(prog_path, prog_file), header = T)
  
# select columns ------
  
  dt_prog = dt_prog[, .(PA, PrgID, ProgramName, Selected, Grouping)]
  
# get selected programs only -----
  
  sel_prog = dt_prog[Selected == 'x']
  
# read in cpuc claims data ------
  
  list_files = list.files(path = data_path, pattern = '*.csv')
  
  years = 2017:2019
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
    temp[, PrgYear := years[i]]
    list_cpuc[[i]] = temp
    
    rm(temp)
    
  }
  
  dt_cpuc = rbindlist(list_cpuc, use.names = T, fill = T)
  
  # cols = data.table(vars = colnames(list_cpuc[[1]]))
  
# keep claims dataset for selected programs only ------
  
  # sel_claims = dt_cpuc[PrgID %in% sel_prog[, PrgID]]
  sel_claims = dt_cpuc[sel_prog, on = .(PrgID), nomatch = 0]
  setcolorder(sel_claims, c(colnames(sel_claims)[c(39, 1:2, 41,40,42:43,3:38)]))
  
# export to csv -------
  
  fwrite(sel_claims, file.path(save_path, save_file), row.names = F)
  