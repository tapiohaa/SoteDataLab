
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###       r-script 6_tidy_result_tables.R         ###
###                Replication file               ###        
###                    2024 by TH                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Tidy and save result tables.
rm(list=ls())

# Install and load the following packages:
library(here)		          # relative file paths.
library(data.table)       # Mutating data.
library(stargazer)        # Save as tex file.

# Inputs:
input_results_ind <- here('data', 'results_montecarlo_raw_ind.csv')
input_results_fam_IU <- here('data', 'results_montecarlo_raw_fam_IU.csv')

# Outputs:
output_precision <- here('tables', 'results_montecarlo.tex')
output_CIs_IU <- here('tables', 'results_montecarlo_CIs_IU.tex')

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Table on precision gains (individual-level randomization). ####
### ### ### ### ### ### ### ### ### ### ### ### ###


df <- data.table::fread(input_results_ind, na.strings = c(NA_character_, ''))

# Results averaged over 2000 iterations:

df <- 
  df[, .('mse' = mean((true.ate-effect)^2),
         'size' = mean(as.integer( 2*(1-pnorm(abs(t_stat))) < 0.05 )),
         'se' = mean(sqrt(variance)),
         'share_treated' = mean(share.treated),
         'control_mean' = mean(control.mean)), 
     by=c('model', 'randomization', 'estimator', 'follow.up', 'type')]

print(df)

# Order:
df[, no := rep(rep(1:nrow(df[follow.up==9 & type=='rf']), each=2), times= 1)]


# Create table:

table <- df[, .(no, randomization, estimator, follow.up, type, mse, size, se)]
table <- dcast(table, no + randomization + estimator + follow.up ~ type, 
               value.var = c('mse', 'size', 'se'))
table <- table[, .(no, randomization, estimator, follow.up, mse_rf, size_rf, 
                   se_rf, mse_2SLS, size_2SLS, se_2SLS)]

# Loop over follow-ups:
follow.ups <- c(9)


tables <- lapply(follow.ups, function(fup) {
  
  dt <- table[follow.up==fup]
  
  # Present results (MSE, SE) as a percentage change relative to the baseline:
  
  mse.base.rf <- dt[randomization=='complete' & estimator=='ols', mse_rf]
  se.base.rf <- dt[randomization=='complete' & estimator=='ols', se_rf]
  mse.base.2SLS <- dt[randomization=='complete' & estimator=='ols', mse_2SLS]
  se.base.2SLS <- dt[randomization=='complete' & estimator=='ols', se_2SLS]
  
  dt[, ':=' (mse_rf = 100 * (mse_rf - mse.base.rf) / mse.base.rf,
             se_rf = 100 * (se_rf - se.base.rf) / se.base.rf,
             size_rf = format(round(100 * size_rf, digits=2), nsmall=2),
             mse_2SLS = 100 * (mse_2SLS - mse.base.2SLS) / mse.base.2SLS,
             se_2SLS = 100 * (se_2SLS - se.base.2SLS) / se.base.2SLS,
             size_2SLS = format(round(100 * size_2SLS, digits=2), nsmall=2))]
  
  # Round:
  vars <- c('mse_rf', 'se_rf', 'mse_2SLS', 'se_2SLS')
  dt[, (vars) := lapply(.SD, round, digits=1), .SDcols=vars]
  
  # To character. Preserve the sign. Tidy.
  
  dt[, ':=' (sign.mse.rf = ifelse(mse_rf < 0, '-', '+'),
             sign.se.rf = ifelse(se_rf < 0, '-', '+'),
             sign.mse.2SLS = ifelse(mse_2SLS < 0, '-', '+'),
             sign.se.2SLS = ifelse(se_2SLS < 0, '-', '+'))]
  
  dt[, ':=' (mse_rf = paste(sign.mse.rf, format(abs(mse_rf), nsmall=1), 
                            '%', sep=''),
             se_rf = paste(sign.se.rf, format(abs(se_rf), nsmall=1), 
                           '%', sep=''),
             mse_2SLS = paste(sign.mse.2SLS, format(abs(mse_2SLS), nsmall=1), 
                              '%', sep=''),
             se_2SLS = paste(sign.se.2SLS, format(abs(se_2SLS), nsmall=1), 
                             '%', sep=''))]
  
  dt[, ':=' (mse_rf = gsub(' ', '', mse_rf),
             se_rf = gsub(' ', '', se_rf),
             mse_2SLS = gsub(' ', '', mse_2SLS),
             se_2SLS = gsub(' ', '', se_2SLS))]
  
  # Highlight the reference value:
  
  dt[randomization=='complete' & estimator=='ols',
     ':=' (mse_rf = '', se_rf = '', mse_2SLS = '', se_2SLS = '')]
  
  
  # Collect and order columns:
  
  dt <- dt[, .(no, randomization, estimator, mse_rf, size_rf, se_rf,
               mse_2SLS, size_2SLS, se_2SLS)]
  
  # Tidy specification names:
  
  dt[randomization=='complete', randomization := 'Complete']
  dt[randomization=='age_gender', randomization := 'Strata (age X gender)']
  dt[randomization=='Y_baseline', randomization := 'Strata (Y\\_baseline)']
  
  dt[estimator=='ols', estimator := 'None']
  dt[estimator=='ols_age_gender', estimator := 'Age, gender']
  dt[estimator=='ols_Ypre', estimator := 'Y\\_baseline']
  dt[estimator=='ols_Ypre_age_gender', estimator :='Y\\_baseline, age, gender']
  dt[estimator=='ols_rich', estimator := 'Rich FEs']
  
  # Tidy column names:
  
  colnames(dt) <- c('', 'Randomization', 'Fixed effects', 'MSE (RF)', 
                    'Size (%) (RF)', 'Std. error (RF)', 'MSE (2SLS)', 
                    'Size (%) (2SLS)', 'Std. error (2SLS)')
  
  print(dt)
  
})

# Save:
tables <- rbindlist(tables)
stargazer::stargazer(tables, out = output_precision,
                     type='text', summary=FALSE, rownames = F, header = F)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Table on CIs (family-level randomization). ####
### ### ### ### ### ### ### ### ### ### ### ### ###


df <- data.table::fread(input_results_fam_IU, na.strings = c(NA_character_, ''))

# Results averaged over 2000 iterations:

df <- 
  df[, .('mse' = mean((true.ate-effect)^2),
         'size' = mean(as.integer( 2*(1-pnorm(abs(t_stat))) < 0.05 )),
         'se' = mean(sqrt(variance)),
         'share_treated' = mean(share.treated),
         'control_mean' = mean(control.mean)), 
     by=c('model', 'randomization', 'estimator', 'outcome', 'type')]

print(df)


# Create table:
table <- df[, .(outcome, type, se, control_mean)]
table <- dcast(table, outcome + control_mean ~ paste0('se_', type), 
               value.var = 'se')

# "Confidence intervals around zero":
table[, ':=' (se_rf_X_1.96 = 1.96* se_rf,
              se_2SLS_X_1.96 = 1.96* se_2SLS)]

# Select variables:
table <- table[, .(outcome, control_mean, se_rf, se_rf_X_1.96, se_2SLS,
                   se_2SLS_X_1.96)]

# Round:
vars <- c('control_mean', 'se_rf', 'se_rf_X_1.96', 'se_2SLS', 'se_2SLS_X_1.96')
table[, (vars) := lapply(.SD, round, digits=3), .SDcols=vars]

# To character:
table[, ':=' (control_mean = format(control_mean, nsmall=3),
              se_rf = format(se_rf, nsmall=3),
              se_rf_X_1.96 = format(se_rf_X_1.96, nsmall=3),
              se_2SLS = format(se_2SLS, nsmall=3),
              se_2SLS_X_1.96 = format(se_2SLS_X_1.96, nsmall=3))]

# Tidy:
table[, se_rf := paste(se_rf, ' [', se_rf_X_1.96, ']', sep='')]
table[, se_2SLS := paste(se_2SLS, ' [', se_2SLS_X_1.96, ']', sep='')]
table <- table[, .(outcome, control_mean, se_rf, se_2SLS)]
colnames(table) <- c('Outcome', 'Control mean (Y)', 'SE (RF)', 'SE (2SLS)')
table[, Outcome := toupper(Outcome)]

# Save:
stargazer::stargazer(table, out = output_CIs_IU,
                     type='text', summary=FALSE, rownames = F, header = F)

# End.
