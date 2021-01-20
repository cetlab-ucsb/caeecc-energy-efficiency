# filter out claims data for programs highlighted by chris
# created: january 20, 2021
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  prog_path     = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/raw/cedars'
  prog_file     = 'cedars_programs_10nov2020_at_233654_Public Sector_grouped.xlsx - cedars_programs_10nov2020_at_23.csv'
  data_path     = '/Users/MEAS/Google Drive/data/cedars'
  claims_file   = 'claims_record_all_years.fst'

# outputs -------

  save_path     = '/Users/MEAS/Google Drive/data/cedars/processed'
  save_file     = 'claims_selected_programs_2016_2020.csv'

# ------------------------------ script ------------------------------ 

# load packages -------

  library(data.table)
  library(fst)

# load list of programs -------
  
  dt_prog = fread(file.path(prog_path, prog_file), header = T)
  
# select columns ------
  
  dt_prog = dt_prog[, .(PA, PrgID, Selected)]
  
# get selected programs only -----
  
  sel_prog = dt_prog[Selected == 'x']

# load claims data ------
  
  dt_claims = read_fst(file.path(data_path, 'raw', claims_file))
  setDT(dt_claims)
  
# get list of column names from claims data ------
  
  claims_vars = data.table(column_name = colnames(dt_claims),
                           selected = '')
  
# keep claims dataset for selected programs only ------
  
  sel_claims = dt_claims[`Program ID` %in% sel_prog[, PrgID]]
  
# export claims data on selected programs to csv ------
  
  fwrite(sel_claims, file.path(save_path, save_file), row.names = F)
  
# export list of column names -----
  
  # fwrite(claims_vars, file.path(prog_path, 'list_of_claims_columns.csv'), row.names = F)
  