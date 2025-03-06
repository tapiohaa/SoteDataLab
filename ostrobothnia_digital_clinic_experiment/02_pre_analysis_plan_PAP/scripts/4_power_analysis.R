
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 4_power_analysis.R         ###
###                Replication file               ###        
###                    2024 by TH                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Some initial preliminary power analysis (LATE + scaled ITT).
rm(list=ls())

# Install and load the following packages:
library(here)           # A Simpler Way to Find Your Files 
library(data.table)     # Extension of 'data.frame' 
library(fixest)         # OLS regression.
library(powerLATE)      # Generalized Power Analysis for LATE 
library(ggplot2)        # Plotting data.
library(patchwork)      # Print multiple plots into same figure.
library(stargazer)      # Save tables as tex 
library(pwr)            # Basic function for power analysis (ITT) 

# Inputs:
input_data <- here('data', 'rct_montecarlo.csv')
input_stats <- here('data', 'rct_digiclinic_stats.csv')

# Outputs:
output_plot_age <- here('figures', 'power_late_by_age.pdf')
output_table_late <- here('tables', 'power_late_follow_up.tex')
output_table_itt <- here('tables', 'power_itt_follow_up.tex')
output_table_b3 <- here('tables', 'power_b3_follow_up.tex')

###
###


dt <- data.table::fread(input_data, na.strings = c(NA_character_, ''))
stats <- data.table::fread(input_stats, na.strings = c(NA_character_, ''))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) LATE: grid of parameter values. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Select parameter values:

sig.level <- 0.05 # Type 1 error rate
share.treated <- 0.5
follow.up <- c(9, 10, 11, 12)
rand.unit <- c('petu', 'petu.alt')
outcome <- c('b.1', 'b.2', 'b.3')
complience.coef <- c(0.94, 1, 1.06) # share of digital clinic users annually
substitution <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40) # "MDE (ACR)"
age.limit <- seq(10, 100, by=1) # include people younger than the age limit

# Create a grid of parameter values:
params <- CJ(
  sig.level, share.treated, follow.up, rand.unit, outcome, complience.coef, 
  substitution, age.limit)

# Merge statistics on digital clinic use in East Uusimaa:
params <- merge(params, stats, by=c('follow.up', 'age.limit'), all.x = TRUE)
params[, complience := digiusers.per.cap * complience.coef]


# Compute standard deviation of the outcome and estimate ar2 as a function of 
# follow-up and age limit:

table <- lapply(age.limit, function(i) {
  
  
  # Subset by age:
  df <- dt[ika < i & strata_dim == 'complete']
  
  
  # Y.sd:
  dt.petu <- df[, .(Y_post = mean(Y_post)), 
                by=c('petu', 'follow.up', 'outcome')]
  dt.petu <- dt.petu[, .(y.sd = sd(Y_post)), by=c('outcome', 'follow.up')]
  dt.petu[, rand.unit := 'petu']
  
  dt.petu.alt <- df[, .(Y_post = mean(Y_post)),
                    by=c('petu.alt', 'follow.up', 'outcome')]
  dt.petu.alt <- dt.petu.alt[, .(y.sd = sd(Y_post)), 
                             by=c('outcome', 'follow.up')]
  dt.petu.alt[, rand.unit := 'petu.alt']
  
  dt.petu <- rbind(dt.petu, dt.petu.alt)
  dt.petu[, age.limit := i]
  
  
  # ar2:

  ols.formula <- Y_post ~ 1 | Y_pre + ika + sukup + kunta31_12 + 
    kturaha_percentile + kieli_k
  
  ols.results <- df[, {
    ols <- fixest::feols(ols.formula, data = .SD) 
    ar2 <- r2(ols, type='ar2')
    list(r2yw = ar2)
  }, by=c('follow.up', 'outcome')]
  
  dt.petu <- merge(dt.petu, ols.results, by=c('follow.up', 'outcome'), all.x=T)
  
  
  # Units of randomization (N):
  
  dt.petu[rand.unit == 'petu', n.units := df[, length(unique(petu))]]
  dt.petu[rand.unit == 'petu.alt', n.units := df[, length(unique(petu.alt))]]
  
})

table <- rbindlist(table)


# Merge y.sd to params:
params <- merge(params, table, 
                by=c('rand.unit', 'outcome', 'follow.up', 'age.limit'), all.x=T)

# Compute the absolute effect as well as effect size:
params[, effect.y.abs := digivisits.per.user * substitution]
params[, effect.y.size := effect.y.abs / y.sd]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) LATE: calculate power. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over all combinations of parameters:

invisible(lapply(1:nrow(params), function(i) {
  
  # Calculate power:
  pwr <- powerLATE::powerLATE.cov(
    pZ = params[i, share.treated],
    pi = params[i, complience], 
    sig.level = params[i, sig.level],
    N = params[i, n.units],
    kappa = params[i, effect.y.size],
    r2dw = params[i, r2dw],
    r2yw = params[i, r2yw],
    verbose = FALSE
  )
  
  params[i, power := pwr$output.parameter]
  
}))


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 3) LATE: power as a function of the upper age limit. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #


plots <- lapply(rand.unit, function(rand.dim) {
  
  lapply(outcome, function(otc) {
    
    # Extract the data:
    df <- params[rand.unit==rand.dim & outcome==otc & age.limit <= 100 & 
                   complience.coef==1.0 & substitution==0.25 & follow.up==9, 
                 .(age.limit, power)]
    
    # Age limit that maximizes power:
    x.pwr.max <- df[power == df[, max(power)], age.limit] 
    
    # Plot:
    
    ggplot(data=df, aes(x=age.limit, y=power)) +
      geom_line() +
      geom_segment(x = df[, min(age.limit)], xend = x.pwr.max, 
                   y = df[, max(power)], yend = df[, max(power)], 
                   linetype='dashed') + 
      geom_segment(x = x.pwr.max, xend = x.pwr.max, 
                   y = 0, yend = df[, max(power)], 
                   linetype='dashed') + 
      scale_x_continuous(name = 'Upper age limit',
                         breaks = seq(10, 100, by=10)) +
      scale_y_continuous(name = 'Power',
                         breaks = seq(0, 1, by=0.1), limits=c(0,1)) + 
      ggtitle(paste('Outcome', toupper(otc))) + 
      theme(text = element_text(size=20),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(linewidth=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.grid.minor.x = element_line(linewidth=0.25, linetype ='solid',
                                              color = 'lightgrey'),
            panel.grid.minor.y = element_blank(),
            panel.border = element_rect(colour='black', fill=NA, linewidth=0.5))
  
  })

})


# Save plots:

ggsave(
  filename = output_plot_age, width = 15, height = 10,
  plot = 
    wrap_elements(panel = plots[[1]][[1]] + plots[[1]][[2]] + plots[[1]][[3]]) + 
    ggtitle('A. Randomization by family') + 
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[2]][[1]] + plots[[2]][[2]] + plots[[2]][[3]]) + 
    ggtitle('B. Randomization: alternative') + plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5)))  



### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 4) LATE: power as a function of follow-up. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #


# Randomization by family, those aged 0 to 69 are included.
# Loop over outcomes: 
outcomes <- c('b.1', 'b.2', 'b.3')

tables <- lapply(outcomes, function(otc) {
  
  table <- params[rand.unit=='petu' & outcome==otc & age.limit==70, 
                  .(follow.up, complience.coef, substitution, power)]
  
  # Tidy columns:
  table[, power := paste(format(round((power * 100), digits=0), nsmall=0), 
                         '%', sep='')]
  table[, complience.coef := format(complience.coef, nsmall=2)]
  table[, substitution := format(substitution, nsmall=1)]
  
  # Pivot wider:
  table <- dcast(table, complience.coef + substitution ~ 
                   paste0('power_', follow.up), value.var = 'power')
  
  table <- table[, .(substitution, complience.coef, 
                     power_9, power_10, power_11, power_12)]
  table <- table[order(-power_9)]
  table <- table[order(-substitution, -complience.coef)]
  
})

table <- rbindlist(tables[1:2])
table.b.3.late <- rbindlist(tables[3])

# Save as tex:
stargazer::stargazer(table, out = output_table_late, 
                     type='text', summary=F, header=F, rownames=F)


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 5) ITT: a grid of parameter values. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #


# Select parameter values:

sig.level <- 0.05 # Type 1 error rate
share.treated <- 0.5
follow.up <- c(9, 10, 11, 12)
rand.unit <- c('petu', 'petu.alt')
outcome <- c('b.1', 'b.2', 'b.3')
effect.coef <- c(0.94, 1, 1.06) # digital clinic contacts per capita per year
substitution <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40) # "MDE (ACR)"
age.limit <- seq(10, 100, by=1)

# Create a grid of parameter values:
params <- CJ(
  sig.level, share.treated, follow.up, effect.coef, rand.unit, outcome, 
  substitution, age.limit)

# Merge statistics on digital clinic use in East Uusimaa:
params <- merge(params, stats, by=c('follow.up', 'age.limit'), all.x = TRUE)
params[, effect.d := digivisits.per.cap.ann * effect.coef]


# Compute standard deviation of the outcome and estimate ar2 as a function of 
# follow-up and outcome:


# Y.sd:
dt.petu <- dt[strata_dim == 'complete', .(Y_post = mean(Y_post)), 
              by=c('petu', 'follow.up', 'outcome')]
dt.petu <- dt.petu[, .(y.sd = sd(Y_post)), by=c('outcome', 'follow.up')]
dt.petu[, rand.unit := 'petu']

dt.petu.alt <- dt[strata_dim == 'complete', .(Y_post = mean(Y_post)),
                  by=c('petu.alt', 'follow.up', 'outcome')]
dt.petu.alt <- dt.petu.alt[, .(y.sd = sd(Y_post)), 
                           by=c('outcome', 'follow.up')]
dt.petu.alt[, rand.unit := 'petu.alt']

dt.petu <- rbind(dt.petu, dt.petu.alt)


# ar2:

ols.formula <- Y_post ~ 1 | Y_pre + ika + sukup + kunta31_12 + 
  kturaha_percentile + kieli_k

ols.results <- dt[strata_dim=='complete', {
  ols <- fixest::feols(ols.formula, data = .SD) 
  ar2 <- r2(ols, type='ar2')
  list(r2yw = ar2)
}, by=c('follow.up', 'outcome')]

dt.petu <- merge(dt.petu, ols.results, by=c('follow.up', 'outcome'), all.x=T)


# Units of randomization (N):

dt.petu[rand.unit == 'petu', n.units := dt[, length(unique(petu))]]
dt.petu[rand.unit == 'petu.alt', n.units := dt[, length(unique(petu.alt))]]

# Merge y.sd to params:
params <- merge(params, dt.petu, 
                by=c('rand.unit', 'outcome', 'follow.up'), all.x=T)


# Compute the absolute effect as well as effect size:
params[, effect.y.abs := effect.d * substitution]
params[, effect.y.size := effect.y.abs / y.sd]

# Compute the number of treated and controls:
params[, n.treated := floor(n.units * share.treated)]
params[, n.control := n.units - n.treated]


### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 6) ITT: calculate power. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #


# Loop over all combinations of parameters:

invisible(lapply(1:nrow(params), function(i) {
  
  # Calculate power:
  pwr <- pwr::pwr.t2n.test(
    n1 = params[i, n.treated], 
    n2 = params[i, n.control], 
    d = params[i, effect.y.size], 
    sig.level = params[i, sig.level], 
    alternative='two.sided'
  )
  
  params[i, power := pwr$power]
  
}))



### ### ### ### ### ### ### ### ### ### ### ### ### #
#### 7) ITT: power as a function of follow-up. ####
### ### ### ### ### ### ### ### ### ### ### ### ### #


# Randomization by family, those aged 0 to 69 are included.
# Loop over outcomes: 
outcomes <- c('b.1', 'b.2', 'b.3')

tables <- lapply(outcomes, function(otc) {
  
  table <- params[rand.unit=='petu' & outcome==otc & age.limit==70, 
                  .(follow.up, effect.coef, substitution, power)]
  
  # Tidy columns:
  table[, power := paste(format(round((power * 100), digits=0), nsmall=0), 
                         '%', sep='')]
  table[, effect.coef := format(effect.coef, nsmall=2)]
  table[, substitution := format(substitution, nsmall=1)]
  
  # Pivot wider:
  table <- dcast(table, effect.coef + substitution ~ 
                   paste0('power_', follow.up), value.var = 'power')
  
  table <- table[, .(substitution, effect.coef, 
                     power_9, power_10, power_11, power_12)]
  table <- table[order(-power_9)]
  table <- table[order(-substitution, -effect.coef)]
  
})

table <- rbindlist(tables[1:2])
table.b.3.itt <- rbindlist(tables[3])

# Save as tex:

stargazer::stargazer(table, out = output_table_itt, 
                     type='text', summary=F, header=F, rownames=F)

setnames(table.b.3.itt, old='effect.coef', new='complience.coef')
table.b.3 <- rbind(table.b.3.late, table.b.3.itt)
stargazer::stargazer(table.b.3, out = output_table_b3, 
                     type='text', summary=F, header=F, rownames=F)

# End.
