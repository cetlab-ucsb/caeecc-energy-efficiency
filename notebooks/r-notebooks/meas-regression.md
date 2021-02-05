---
title: "caeecc regression"
output: 
  html_document:
    keep_md: true
---

inputs:

```r
data_file = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1-BTh8T0PMwHDs3KZ9V--KPrNWRUgRxvV/2020_CAEECC_Public_Sector_Underserved/data/processed/2_4_2021_updated_compiled_dataset.xlsx'
```

load packages:

```r
library(data.table)
library(openxlsx)
```

import data:

```r
dt = as.data.table(read.xlsx(data_file))
```

rename columns:

```r
colnames(dt) = c('county', 'budget', 'first_year_gross_kwh', 'population', 'mean_income', 'irr', 'tax_revenue', 'ces_score_median', 'first_year_gross_therm',
                 'first_year_net_kwh', 'first_year_net_therm', 'total_claims', 'total_programs')
```

# regression

## linear regression to predict budget
i'm just going to try it out with a linear regression for now

### linear regression on full dataset

```r
reg_budget_full = lm(budget ~ population + mean_income + tax_revenue + ces_score_median + irr, data = dt)
```

checking the results:

```r
summary(reg_budget_full)
```

```
## 
## Call:
## lm(formula = budget ~ population + mean_income + tax_revenue + 
##     ces_score_median + irr, data = dt)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -9057763 -1945469  -772290  1192214 14510288 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)   
## (Intercept)       5.138e+06  5.660e+06   0.908  0.36818   
## population        3.323e+00  1.286e+00   2.584  0.01261 * 
## mean_income       9.636e+01  6.821e+01   1.413  0.16367   
## tax_revenue      -9.802e-04  5.442e-04  -1.801  0.07745 . 
## ces_score_median -6.765e+04  5.719e+04  -1.183  0.24226   
## irr              -1.736e+07  6.368e+06  -2.726  0.00872 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4277000 on 52 degrees of freedom
## Multiple R-squared:  0.4657,	Adjusted R-squared:  0.4143 
## F-statistic: 9.065 on 5 and 52 DF,  p-value: 3.028e-06
```

### linear regression on only counties with programs/budgets

```r
reg_budget_partial = lm(budget ~ population + mean_income + tax_revenue + ces_score_median + irr, data = dt[budget > 0])
```

checking results:

```r
summary(reg_budget_partial)
```

```
## 
## Call:
## lm(formula = budget ~ population + mean_income + tax_revenue + 
##     ces_score_median + irr, data = dt[budget > 0])
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -8609965 -3165687  -850516  1467375 13840667 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)  
## (Intercept)      -9.931e+06  1.555e+07  -0.639   0.5314  
## population        3.638e+00  2.117e+00   1.718   0.1039  
## mean_income       3.957e+02  2.027e+02   1.952   0.0676 .
## tax_revenue      -1.280e-03  8.632e-04  -1.482   0.1565  
## ces_score_median -4.375e+04  1.263e+05  -0.346   0.7332  
## irr              -2.315e+07  1.566e+07  -1.478   0.1576  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5540000 on 17 degrees of freedom
## Multiple R-squared:  0.5088,	Adjusted R-squared:  0.3644 
## F-statistic: 3.522 on 5 and 17 DF,  p-value: 0.02286
```

