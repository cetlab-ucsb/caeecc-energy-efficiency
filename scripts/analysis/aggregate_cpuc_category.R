# aggregate cpuc claims data to county level
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
  
  agg_program = dt_claims[, lapply(.SD, sum, na.rm = TRUE), by = c('PrgID', 'ProgramName', 'Grouping'), 
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
  
# export to csvs -------
  
  fwrite(agg_program, file.path(save_path, save_prog_file), row.names = F)
  fwrite(agg_category, file.path(save_path, save_cat_file), row.names = F)
  
# ------------------------------- plot -------------------------------
  
  
  # theme -------
  
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
  
  # bar plot: gross measure cost ----------
  
    bar_cost = ggplot(agg_category, aes(x = category, y = TotalGrossMeasureCost/1e6, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Gross measure costs (2017-2019)',
           subtitle = 'Million USD',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(labels = scales::comma, expand = c(0,0), limits = c(0,1200)) +
      scale_fill_ipsum() + 
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
    
  # bar plot: gross measure cost ----------
    
    bar_kwh = ggplot(agg_category, aes(x = category, y = TotalLifecycleGrosskWh/1e9, fill = category)) + 
      geom_bar(stat = 'identity') + 
      labs(title = 'Total life time kWh savings (2017-2019)',
           subtitle = 'Billion kWh',
           x = NULL,
           y = NULL, 
           fill = NULL) +
      scale_y_continuous(expand = c(0,0), limits = c(0,20)) +
      scale_fill_ipsum() + 
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
    
    
    test = dt_claims[PrgYear == 2017 & PrgID == 'PGE211009']
    sum(test[, TotalGrossMeasureCost])
    
    agg_test = test[, lapply(.SD, sum, na.rm = TRUE), by = c('PrgID', 'ProgramName', 'Grouping'), 
                            .SDcols = c('TotalFirstYearGrosskW',
                                        'TotalFirstYearGrosskWh',
                                        'TotalFirstYearGrossTherm',
                                        'TotalGrossIncentive',
                                        'TotalGrossMeasureCost',
                                        'TotalGrossMeasureCost_ER',
                                        'TotalLifecycleGrosskW',
                                        'TotalLifecycleGrosskWh',
                                        'TotalLifecycleGrossTherm') ] 
    
  