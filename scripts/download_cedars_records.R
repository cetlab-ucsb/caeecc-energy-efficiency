# read in claims and filings records from CEDARS website
# created: november 12, 2020
# author: meas meng

# inputs --------

  data_path     = '/Users/MEAS/Google Drive/data/cedars/raw'
  claims_file   = 'claims_record_all_years.zip'
  filings_file  = 'filings_record_all_years.zip'

# outputs -------

  save_path     = '/Users/MEAS/Google Drive/data/cedars/processed'

# load libraries -------

  library(data.table)
  library(fst)

# read in claims data -------
  
  claims_data = fread(cmd = paste0("unzip -cq '", file.path(data_path, claims_file), "'"))
  
  # if don't want to store file locally: 
    # claims_data = fread('curl https://cedars.sound-data.com/reports/download/claims_record_all_years.zip | funzip')

# download and read in filings data ------
  
  filings_data = fread(cmd = paste0("unzip -cq '", file.path(data_path, filings_file), "'"))
  
  # if don't want to store file locally: 
    # filings_data = fread('curl https://cedars.sound-data.com/reports/download/filings_record_all_years.zip | funzip')
  
# keep public sector records only -----
  
  claims_public = claims_data[`Primary Sector` == 'Public']
  filings_public = filings_data[`Primary Sector` == 'Public']

# get separate data table of just claims records columns ------
  
  claim_cols = data.table(col_names = colnames(claims_data))

# select claims columns to keep ------
  # claims_keep = claims_public[, .(`Claim ID`, 
  #                                 `Year`, 
  #                                 `PA`, 
  #                                 `Program ID`, 
  #                                 `Primary Sector`, 
  #                                 `Building Type`,
  #                                 `Climate Zone`,
  #                                 `Gas Savings Profile`,
  #                                 `Gas Sector`,
  #                                 `Measure Electric End Use Shape`,
  #                                 `Target Sector`,
  #                                 `Number of Units`,
  #                                 `Financing`,
  #                                 `First Year Gross kWh`,
  #                                 `First Year Gross kW`,
  #                                 `First Year Gross Therm`,
  #                                 `Gross Measure Cost`,
  #                                 `Gross Measure Cost Early Retirement`,
  #                                 `Lifecycle Gross kWh`,
  #                                 `Lifecycle Gross Therm`,
  #                                 `Upstream Flag`)]

  
# export to fst files ------
  
  write_fst(claims_public, here::here('data', 'claims_record_all_years_public.fst'), compress = 90)
  write_fst(filings_public, here::here('data', 'filings_record_all_years_public.fst'), compress = 90)
  
# export to csv files ------

  fwrite(claims_public, file.path(save_path, 'claims_record_all_years_public.csv'), row.names = F)
  fwrite(filings_public, file.path(save_path, 'filings_record_all_years_public.csv'), row.names = F)
  fwrite(claim_cols, file.path(save_path, 'claims_record_columns.csv'), row.names = F)
