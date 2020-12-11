# combine zip code level data with public claims data
# created: december 9, 2020
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  data_path     = '/Users/MEAS/Google Drive/data/cedars'
  claims_file   = 'claims_record_all_years.csv'
  filings_file  = 'filings_record_all_years.csv'
  
# outputs -------
  
  save_path     = '/Users/MEAS/Google Drive/data/cedars/processed'

# ------------------------------ script ------------------------------ 
  
# load packages -------
  
  library(data.table)
  
# load zip code level data ---------  

    years_zip = 2017:2019
    list_zip = list()
    
    for (i in seq_along(years_zip)) {
      
      temp = fread(file.path(data_path, 'zip_code', paste0(years_zip[i], '_Claims_by_PrgID-Zip.csv')), header = T)
      temp[, year := years_zip[i]]
      list_zip[[i]] = temp
      
    }
    
    dt_zip = rbindlist(list_zip)
  # dt_zip = rbindlist(lapply(list.files(path = file.path(data_path, 'zip_code'), '*.csv', full.names = T), fread))

# load public claims data -------
  
  dt_claims = fread(file.path(data_path, 'raw', claims_file), header = T)
  claims_public = dt_claims[`Primary Sector` == 'Public']
  
# load public filings data -------
  
  dt_filigs = fread(file.path(data_path, 'raw', filings_file), header = T)
  filings_public = dt_filigs[`Primary Sector` == 'Public']

# get unique set of programs and primary sector --------
  
  un_programs = unique(dt_claims[, .(`Program ID`, `Year`, `PA`, `Program Name`, `Primary Sector`, `Program Sector`)])
  un_public = un_programs[`Primary Sector` == 'Public']
  
# only keep years between 2017-2019 --------
  
  un_programs = un_programs[Year %in% 2017:2019]
  un_public = un_public[Year %in% 2017:2019]
  
# get programs that exist in the claims dataset and the zip code dataset -------
  
  matched_programs = un_programs[dt_zip, on = .(`Program ID` = `PrgID`, `Year` == `year`), nomatch = 0]
  matched_public = matched_programs[`Primary Sector` == 'Public']

# see which programs exist in the claims dataset but not the zip code dataset ------ 
  
  nonmatched_programs = un_programs[!dt_zip, on = .(`Program ID` = `PrgID`, `Year` == `year`)]
  nonmatched_public = nonmatched_programs[`Primary Sector` == 'Public']
  # nonmatched = dt_claims[!`Program ID` %in% dt_zip[, PrgID], .(`Claim ID`, `Year`, `PA`, `Program ID`, `Program Name`, `Primary Sector`, `Program Sector`)]
  # nonmatched_public = nonmatched[`Primary Sector` == 'Public']
  
# get nonmatched zip code data -------
  
  nonmatched_zip = dt_zip[!un_programs, on = .(`PrgID` = `Program ID`, `year` == `Year`)]
  
# set column orders -------
  
  setorder(matched_public, `Program ID`, `Year`)
  setorder(nonmatched_public, `Program ID`, `Year`)
  
# aggregate columns by program ID + year -------
  
  budget_claim = claims_public[, .(type = 'claim',
                                   budget_usd = sum(`Budget`, na.rm = T),
                                   gross_measure_cost_usd = sum(`Gross Measure Cost`, na.rm = T),
                                   lifecycle_gross_kwh = sum(`Lifecycle Gross kWh`, na.rm = T)), 
                               by = .(`Program ID`, `PA`, `Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`, `Year`)]
  budget_filing = filings_public[, .(type = 'filing',
                                     budget_usd = sum(`Budget`, na.rm = T),
                                     gross_measure_cost_usd = sum(`Gross Measure Cost`, na.rm = T),
                                     lifecycle_gross_kwh = sum(`Lifecycle Gross kWh`, na.rm = T)), 
                                 by = .(`Program ID`, `PA`, `Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`, `Year`)]
  
# combine claims and filings data -------
  
  vars_combined = rbindlist(list(budget_claim, budget_filing), use.names = T)

# get 2019 filings data ------
  
  vars_2019 = vars_combined[Year == 2019 & type == 'filing']
  # claims_2019 = budget_claim[Year == 2019]
  # filings_2019 = budget_filing[Year == 2019]
  
# get top 10 categories for filings ------
  
  vars_2019[, budget_rank := rank(-budget_usd)]
  vars_2019[, measure_cost_rank := rank(-gross_measure_cost_usd)]
  vars_2019[, gross_kwh_rank := rank(-lifecycle_gross_kwh)]
  
# merge ranking with variable data -------
  
  vars_combined = merge(vars_combined, 
                        vars_2019[, .(`Program ID`, `PA`, `Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`,
                                      budget_rank, measure_cost_rank, gross_kwh_rank)], 
                        on = c(`Program ID`, `PA`, `Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`),
                        all.x = T)
  
  vars_combined[, budget_program_name := ifelse(budget_rank %in% 1:10, `Program Name`, 'Other')]
  vars_combined[, measure_cost_program_name := ifelse(measure_cost_rank %in% 1:10, `Program Name`, 'Other')]
  vars_combined[, gross_kwh_program_name := ifelse(gross_kwh_rank %in% 1:10, `Program Name`, 'Other')]
  
# export to csvs --------
  
  fwrite(matched_public, file.path(save_path, 'public_programs_with_zip_code_data_2017_2019.csv'), row.names = F)
  fwrite(nonmatched_public, file.path(save_path, 'public_programs_without_zip_code_data_2017_2019.csv'), row.names = F)

# count unique number of programs in each dataset -------
  
  uniqueN(un_public[, `Program ID`])
  uniqueN(matched_public[, `Program ID`])
  uniqueN(nonmatched_public[, `Program ID`])
  
  uniqueN(un_public[! `Program ID` %in% dt_zip[, PrgID], `Program ID` ])
  
  uniqueN(un_public[, `Program ID`, Year])
  uniqueN(matched_public[, `Program ID`, Year])
  uniqueN(nonmatched_public[, `Program ID`, Year])
  
  