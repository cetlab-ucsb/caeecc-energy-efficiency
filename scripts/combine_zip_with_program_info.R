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
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(stringr)
  
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
                               by = .(`Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`, `Year`)]
  budget_filing = filings_public[, .(type = 'filing',
                                     budget_usd = sum(`Budget`, na.rm = T),
                                     gross_measure_cost_usd = sum(`Gross Measure Cost`, na.rm = T),
                                     lifecycle_gross_kwh = sum(`Lifecycle Gross kWh`, na.rm = T)), 
                                 by = .(`Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`, `Year`)]
  
# combine claims and filings data -------
  
  vars_combined = rbindlist(list(budget_claim, budget_filing), use.names = T)
  
# calculate gross measure cost per kwh -------
  
  vars_combined[, cost_per_kwh := gross_measure_cost_usd/lifecycle_gross_kwh]
  
# get 2019 filings data ------
  
  vars_2019 = vars_combined[Year == 2019 & type == 'filing' & cost_per_kwh < Inf]
  # claims_2019 = budget_claim[Year == 2019]
  # filings_2019 = budget_filing[Year == 2019]
  
# get top 10 categories for filings ------
  
  vars_2019[, budget_rank := rank(-budget_usd)]
  setorder(vars_2019, budget_rank)
  top10_budget = vars_2019[1:10, `Program Name`]
  
  vars_2019[, measure_cost_rank := rank(-gross_measure_cost_usd)]
  setorder(vars_2019, measure_cost_rank)
  top10_measure_cost = vars_2019[1:10, `Program Name`]
  
  vars_2019[, gross_kwh_rank := rank(-lifecycle_gross_kwh)]
  setorder(vars_2019, gross_kwh_rank)
  top10_gross_kwh = vars_2019[1:10, `Program Name`]
  
  vars_2019[, cost_per_kwh_rank := rank(cost_per_kwh)]
  setorder(vars_2019, cost_per_kwh_rank)
  top10_cost_per_kwh = vars_2019[1:10, `Program Name`]

# merge ranking with variable data -------
  
  vars_combined = merge(vars_combined, 
                        vars_2019[, .(`Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`,
                                      budget_rank, measure_cost_rank, gross_kwh_rank, cost_per_kwh_rank)], 
                        on = c(`Program Name`, `Primary Sector`, `Program Category`, `Program Implementer`),
                        all.x = T)
  
  vars_combined[, budget_program_name := ifelse(budget_rank %in% 1:10, `Program Name`, 'Other')]
  vars_combined[, measure_cost_program_name := ifelse(measure_cost_rank %in% 1:10, `Program Name`, 'Other')]
  vars_combined[, gross_kwh_program_name := ifelse(gross_kwh_rank %in% 1:10, `Program Name`, 'Other')]
  vars_combined[, cost_per_kwh_name := ifelse(cost_per_kwh_rank %in% 1:10, `Program Name`, 'Other')]
  
# aggregate variables ------
  
  agg_measure_cost = vars_combined[, .(gross_measure_cost_usd = sum(gross_measure_cost_usd, na.rm = T)),
                                   by = .(`measure_cost_program_name`, `Primary Sector`, `Program Category`, `Program Implementer`, `Year`, type)]
  agg_cost_per_kwh = vars_combined[, .(cost_per_kwh = mean(cost_per_kwh, na.rm = T)),
                                   by = .(`cost_per_kwh_name`, `Primary Sector`, `Program Category`, `Program Implementer`, `Year`, type)]
  
# reorder factor levels ------
  
  vars_combined[, type := factor(type, levels = rev(c('claim', 'filing')))]
  vars_combined[, measure_cost_program_name := factor(measure_cost_program_name, levels = c(top10_measure_cost, 'Other'))]
  vars_combined[, gross_kwh_program_name := factor(gross_kwh_program_name, levels = c(top10_gross_kwh, 'Other'))]
  vars_combined[, cost_per_kwh_name := factor(cost_per_kwh_name, levels = c(top10_cost_per_kwh, 'Other'))]
  
  agg_cost_per_kwh[, cost_per_kwh_name := factor(cost_per_kwh_name, levels = c(top10_cost_per_kwh, 'Other'))]
  
  
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
  
  
  
# ------------------------- plots ------------------------- 
  
  # plot theme & palettes ------
  
    pal_type = c('claim' = '#005581',
                   'filing' = '#FFB511')
    
    theme_line = theme_ipsum(base_family = 'Secca Soft',
                             grid = 'Y', 
                             plot_title_size = 14, 
                             subtitle_size = 12,
                             axis_title_just = 'center',
                             axis_title_size = 12, 
                             axis_text_size = 12,
                             strip_text_size = 12)  +
      theme(plot.title = element_text(hjust = 0, face = 'bold'),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 8, color = '#5c5c5c', face = 'plain'),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.2, 'cm'),
            axis.text.x = element_text(margin = margin(t = .1, unit = 'cm')),
            axis.text.y = element_text(margin = margin(r = .1, unit = 'cm')),
            legend.title = element_text(size = 12, vjust = 0.5),
            legend.text = element_text(size = 12, vjust = 0.5),
            legend.margin = margin(t = 0, b = 0, unit = 'cm'),
            legend.position = 'bottom',
            strip.text = element_text(hjust = 0.5),
            plot.margin = unit(c(1,1,1,1), 'lines'))
  
  # plot: measure costs ---------
  
    fig_measure_cost = ggplot(vars_combined[Year == 2019], aes(x = measure_cost_program_name, y = gross_measure_cost_usd/1e6, group = type, fill = type)) +
      geom_bar(position = "dodge", stat = "identity", width = 0.5) +
      labs(title = 'Gross measure cost reported in 2019 filings',
           subtitle = 'Million USD',
           caption = 'Non-top 10 programs aggregated into single program', 
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = pal_type) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(expand = c(0,0)) +
      theme_line
    fig_measure_cost
  
    ggsave(fig_measure_cost,
           filename = here::here('figures', 'top_10_gross_measure_cost.pdf'),
           width = 11,
           height = 6,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(here::here('figures', 'top_10_gross_measure_cost.pdf'),
                outfile = here::here('figures', 'top_10_gross_measure_cost.pdf'))
    
    ggsave(fig_measure_cost,
           filename = here::here('figures', 'top_10_gross_measure_cost.png'),
           width = 11,
           height = 6,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    
    
  # plot: lifecycle gross kwh ---------
    
    fig_gross_kwh = ggplot(vars_combined[Year == 2019], aes(x = gross_kwh_program_name, y = lifecycle_gross_kwh/1e6, group = type, fill = type)) +
      geom_bar(position = "dodge", stat = "identity", width = 0.5) +
      labs(title = 'Lifecycle gross energy savings in 2019 filings',
           subtitle = 'Terawatt-hours (TWh)',
           caption = 'Non-top 10 programs aggregated into single program', 
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = pal_type) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(expand = c(0,0)) +
      theme_line
    fig_gross_kwh
    
    ggsave(fig_gross_kwh,
           filename = here::here('figures', 'top_10_lifecycle_gross_kwh.pdf'),
           width = 11,
           height = 6,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(here::here('figures', 'top_10_lifecycle_gross_kwh.pdf'),
                outfile = here::here('figures', 'top_10_lifecycle_gross_kwh.pdf'))
    
    ggsave(fig_gross_kwh,
           filename = here::here('figures', 'top_10_lifecycle_gross_kwh.png'),
           width = 11,
           height = 6,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    
    
  # plot: measure cost per lifecycle savings kwh ---------
    
    fig_cost_per_kwh = ggplot(agg_cost_per_kwh[Year == 2019], aes(x = cost_per_kwh_name, y = cost_per_kwh, group = type, fill = type)) +
      geom_bar(position = "dodge", stat = "identity", width = 0.5) +
      labs(title = 'Measure cost per lifecycle energy savings in 2019 filings',
           subtitle = 'USD per kWh',
           caption = 'Non-top 10 programs aggregated into single program', 
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = pal_type) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10), expand = c(0,0)) + 
      scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 0.5), expand = c(0,0)) +
      theme_line
    fig_cost_per_kwh
    
    ggsave(fig_cost_per_kwh,
           filename = here::here('figures', 'top_10_cost_per_kwh.pdf'),
           width = 11,
           height = 6,
           units = 'in', 
           device = 'pdf')
    
    embed_fonts(here::here('figures', 'top_10_cost_per_kwh.pdf'),
                outfile = here::here('figures', 'top_10_cost_per_kwh.pdf'))
    
    ggsave(fig_cost_per_kwh,
           filename = here::here('figures', 'top_10_cost_per_kwh.png'),
           width = 11,
           height = 6,
           dpi = 400, 
           units = 'in', 
           device = 'png')
    
    
    
  