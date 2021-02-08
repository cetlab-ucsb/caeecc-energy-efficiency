# aggregate cpuc claims data to categorical level
# created: february 5, 2021
# author: meas meng

# ------------------------------ inputs ------------------------------ 

  data_path   = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed'
  data_file   = 'cpuc_claims_selected_programs_2017_2019.csv'

# outputs -------

  save_path           = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed'
  save_prog_file      = 'cpuc_claims_aggregated_by_program_2017_2019.csv'
  save_cat_file       = 'cpuc_claims_aggregated_by_category_2017_2019.csv'
  
# ------------------------------ script ------------------------------ 

# load packages -------

  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  
# import claims data ------

  dt_claims = fread(file.path(data_path, data_file), header = T)
  dt_claims[, ZCTA5 := substr(SiteZipCode, 1, 5)]

# if no grouping, say unclassified --------
  
  dt_claims[Grouping %like% 'Higher', category := 'Higher Education']
  dt_claims[Grouping %like% 'K-12', category := 'Other Education']
  dt_claims[Grouping == 'Education ()', category := 'Other Education']
  dt_claims[Grouping %like% 'Local Government', category := 'Local Government']
  dt_claims[Grouping %like% 'Federal', category := 'Federal']
  dt_claims[Grouping %like% 'State', category := 'State']
  dt_claims[is.na(category), category := 'Uncategorized']
  
# aggregate by program --------
  
  agg_program = dt_claims[, lapply(.SD, sum, na.rm = TRUE), by = c('PrgID', 'ProgramName', 'Grouping', 'category'), 
                          .SDcols = c('TotalFirstYearGrosskW',
                                      'TotalFirstYearGrosskWh',
                                      'TotalFirstYearGrossTherm',
                                      'TotalGrossIncentive',
                                      'TotalGrossMeasureCost',
                                      'TotalGrossMeasureCost_ER',
                                      'TotalLifecycleGrosskW',
                                      'TotalLifecycleGrosskWh',
                                      'TotalLifecycleGrossTherm') ] 
  
# get top 10 programs with highest gross measure costs -----
  
  setkey(agg_program, PrgID)
  # agg_program[, tail(.SD, 10)]
  
  agg_program[, rank_cost := frank(-TotalGrossMeasureCost)]
  agg_program[, group_cost := ifelse(rank_cost %in% 1:10, ProgramName, 'Non-top 10')]
  
  agg_top10_cost = agg_program[rank_cost %in% 1:10, lapply(.SD, sum, na.rm = TRUE), by = .(ProgramName, category), 
                               .SDcols = c('TotalFirstYearGrosskW',
                                           'TotalFirstYearGrosskWh',
                                           'TotalFirstYearGrossTherm',
                                           'TotalGrossIncentive',
                                           'TotalGrossMeasureCost',
                                           'TotalGrossMeasureCost_ER',
                                           'TotalLifecycleGrosskW',
                                           'TotalLifecycleGrosskWh',
                                           'TotalLifecycleGrossTherm') ] 
  
# aggregate by category ------
  
  agg_category = dt_claims[, lapply(.SD, sum, na.rm = TRUE), by = 'category', .SDcols = c('TotalFirstYearGrosskW',
                                                                                          'TotalFirstYearGrosskWh',
                                                                                          'TotalFirstYearGrossTherm',
                                                                                          'TotalGrossIncentive',
                                                                                          'TotalGrossMeasureCost',
                                                                                          'TotalGrossMeasureCost_ER',
                                                                                          'TotalLifecycleGrosskW',
                                                                                          'TotalLifecycleGrosskWh',
                                                                                          'TotalLifecycleGrossTherm') ] 
  
  count_category = dt_claims[, lapply(.SD, uniqueN, na.rm = TRUE), by = 'category', .SDcols = c('ClaimID', 'PrgID')] 
  
  agg_category = agg_category[count_category, on = .(category)]
  
# export to csvs -------
  
  fwrite(agg_program, file.path(save_path, save_prog_file), row.names = F)
  fwrite(agg_category, file.path(save_path, save_cat_file), row.names = F)
  
# ------------------------------- plot -------------------------------
  
  
  # theme -------
  
  pal_category = c("Uncategorized" = "#576b81",
                   "Other Education" = "#50a727",
                   "Higher Education" = "#f05c0b",
                   "State" = "#e30011",
                   "Local Government" = "#7b1978",
                   "Federal" = "#1782d2")
  
  theme_line = theme_ipsum(base_family = 'Secca Soft',
                           grid = 'Y', 
                           plot_title_size = 20, 
                           subtitle_size = 18,
                           axis_title_just = 'center',
                           axis_title_size = 18, 
                           axis_text_size = 16,
                           strip_text_size = 16)  +
    theme(plot.title = element_text(hjust = 0, face = 'bold'),
          plot.title.position = 'plot',
          plot.subtitle = element_text(hjust = 0),
          plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
          axis.line.x = element_line(color = 'black'),
          axis.ticks.x = element_line(color = 'black'),
          axis.ticks.length.x = unit(0.2, "cm"),
          # axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
          plot.margin = unit(c(1,1,1,1), "lines"),
          legend.text = element_text(size = 14),
          legend.position = 'right')
  
  # bar plot: gross measure cost by category ----------
  
    bar_cost = ggplot(agg_category, aes(x = category, y = TotalGrossMeasureCost/1e6, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Gross measure costs (2017-2019)',
           subtitle = 'Million USD',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), limits = c(0,1200)) +
      scale_fill_manual(values = pal_category) + 
      guides(fill = "none") +
      theme_line 
    bar_cost
    
    bar_cost_fname = 'bar_gross-measure-cost-by-category_2017-2019.pdf'
    
    ggsave(bar_cost, 
           filename = here::here('figures', bar_cost_fname), 
           width = 12, 
           height = 6.25)
    
    embed_fonts(here::here('figures', bar_cost_fname),
                outfile = here::here('figures', bar_cost_fname))
    
  # bar plot: energy savings by category ----------
    
    bar_kwh = ggplot(agg_category, aes(x = category, y = TotalLifecycleGrosskWh/1e9, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Total life time kWh savings (2017-2019)',
           subtitle = 'Billion kWh',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
      scale_fill_manual(values = pal_category) + 
      guides(fill = "none") +
      theme_line 
    bar_kwh
    
    bar_kwh_fname = 'bar_lifecycle-kwh-savings-by-category_2017-2019.pdf'
    
    ggsave(bar_kwh, 
           filename = here::here('figures', bar_kwh_fname), 
           width = 12, 
           height = 6.25)
    
    embed_fonts(here::here('figures', bar_kwh_fname),
                outfile = here::here('figures', bar_kwh_fname))
  
  
    
  # bar plot: number of programs by category ----------
    
    bar_nprogs = ggplot(agg_category, aes(x = category, y = PrgID, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Number of programs by category (2017-2019)',
           # subtitle = 'Billion kWh',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(expand = c(0,0), limits = c(0,100)) +
      scale_fill_manual(values = pal_category) + 
      guides(fill = "none") +
      theme_line 
    bar_nprogs
    
    bar_nprogs_fname = 'bar_number-of-programs-by-category_2017-2019.pdf'
    
    ggsave(bar_nprogs, 
           filename = here::here('figures', bar_nprogs_fname), 
           width = 12, 
           height = 6.25)
    
    embed_fonts(here::here('figures', bar_nprogs_fname),
                outfile = here::here('figures', bar_nprogs_fname))
    
    
    
  # bar plot: number of claims by category ----------
    
    bar_nclaims = ggplot(agg_category, aes(x = category, y = ClaimID, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Number of claims by category (2017-2019)',
           # subtitle = 'Billion kWh',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), limits = c(0,4.5e5)) +
      scale_fill_manual(values = pal_category) + 
      guides(fill = "none") +
      theme_line 
    bar_nclaims
    
    bar_nclaims_fname = 'bar_number-of-claims-by-category_2017-2019.pdf'
    
    ggsave(bar_nclaims, 
           filename = here::here('figures', bar_nclaims_fname), 
           width = 12, 
           height = 6.25)
    
    embed_fonts(here::here('figures', bar_nclaims_fname),
                outfile = here::here('figures', bar_nclaims_fname))
    
    
    
  # bar plot: top 10 gross measure costs by program ----------
    
    bar_top10_cost = ggplot(agg_top10_cost, aes(x = reorder(ProgramName, -TotalGrossMeasureCost), y = TotalGrossMeasureCost/1e6, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Gross measure cost for top 10 programs',
           subtitle = 'Million USD',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), limits = c(0,175)) +
      scale_fill_manual(values = pal_category) + 
      # guides(fill = "none") +
      theme_line +
      theme(axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1))
    bar_top10_cost
    
    bar_top10_cost_fname = 'bar_top-10-measure-costs-by-program_2017-2019.pdf'
    
    ggsave(bar_top10_cost, 
           filename = here::here('figures', bar_top10_cost_fname), 
           width = 13.5, 
           height = 8)
    
    embed_fonts(here::here('figures', bar_top10_cost_fname),
                outfile = here::here('figures', bar_top10_cost_fname))
    
    