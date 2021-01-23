# save raw claims csv file as fst file
# created: december 17, 2020
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  data_path     = '/Users/MEAS/Google Drive/data/cedars/raw'
  claims_file   = 'claims_record_all_years.csv'

# ------------------------------ outputs ------------------------------ 

  save_path     = '/Users/MEAS/Google Drive/data/cedars/raw'
  save_file     = 'claims_record_all_years.fst'

# ------------------------------ script ------------------------------ 

# load packages -------

  library(data.table)
  library(fst)

# load data -------

  dt_raw = fread(file.path(data_path, claims_file), header = T)

# save fst file -------

  write_fst(dt_raw, file.path(save_path, save_file), compress = 95)
  