# combine zip code level data with public claims data
# created: december 9, 2020
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  data_path     = '/Users/MEAS/Google Drive/data/cedars'
  claims_file   = 'claims_record_all_years.csv'
  
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

# count unique number of programs in each dataset -------
  
  uniqueN(un_public[, `Program ID`])
  uniqueN(matched_public[, `Program ID`])
  uniqueN(nonmatched_public[, `Program ID`])
  
  uniqueN(un_public[! `Program ID` %in% dt_zip[, PrgID], `Program ID` ])
  
  uniqueN(un_public[, `Program ID`, Year])
  uniqueN(matched_public[, `Program ID`, Year])
  uniqueN(nonmatched_public[, `Program ID`, Year])
  
# export to csvs --------
  
  fwrite(matched_public, file.path(save_path, 'public_programs_with_zip_code_data_2017_2019.csv'), row.names = F)
  fwrite(nonmatched_public, file.path(save_path, 'public_programs_without_zip_code_data_2017_2019.csv'), row.names = F)
  