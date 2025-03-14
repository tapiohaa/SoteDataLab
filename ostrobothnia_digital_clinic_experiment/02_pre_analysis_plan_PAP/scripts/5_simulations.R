
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 5_simulations.R          ###
###                Replication file               ###        
###                    2025 by TH                 ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Conduct Monte Carlo simulations.
rm(list=ls())

# Install and load the following packages:
library(here)		          # relative file paths.
library(data.table)       # Mutating and aggregating data.
library(fixest)           # OLS regression.

# Inputs:
input_data <- here('data', 'rct_montecarlo.csv')
input_data_IU <- here('data', 'rct_montecarlo_east_uusimaa.csv')

# Outputs:
output_results_ind <- here('data', 'results_montecarlo_raw_ind.csv')
output_results_fam_IU <- here('data', 'results_montecarlo_raw_fam_IU.csv')

###
###

df <- data.table::fread(input_data, na.strings = c(NA_character_, ''))
df.IU <- data.table::fread(input_data_IU, na.strings = c(NA_character_, ''))

# SAMPLING:
# - a fixed finite population
# - model: Y_i(0)=Y_i(1) (reduced form ATE=0 & no heterogeneity);
#   D_i(0)=0 & D_i(0) <= D_i(1) (first stage)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Functions for simulations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that randomizes individuals in each stratum to treatment. ###

randomize.ind <- function(data) {
  # INPUTS:
  # data: a data.table with variable 'strata' with N_s >= 2 for all strata.
  # OUTPUTS:
  # the same data.table but treatment status 'treat' added (split 1:1)
  
  
  DT <- data[, mget(colnames(data))]
  DT <- DT[order(strata)] # important for this function
  
  treatment.rand <- 
    DT[, {
      # The number of treated given the stratum size. Randomize between
      # using the floor and ceiling function so that approximately 1/2
      # end up being in the control group:
      if( .N %% 2 == 0 ) { 
        n_treat <- 1/2 * .N 
      } else {
        floor_or_ceiling <- rbinom(1, 1, 0.5)
        if(floor_or_ceiling==0) { n_treat <- floor(1/2 * .N) }
        if(floor_or_ceiling==1) { n_treat <- ceiling(1/2 * .N) }
      }
      # Initialize a vector of treatment assignments:
      treat.fill <- rep(0, .N)
      # Assing treatment to rows with selected indices. Do this by
      # selecting a random number, ordering them from smallest to largest,
      # and taking the desired number (n_treat) of indices that have the 
      # smallest random numbers.
      treat.fill[order(runif(.N))[seq_len(n_treat)]] <- 1
      list(treat = treat.fill)
    }, by=strata][, treat]
  
  DT[, treat := treatment.rand]
  
  # For those not treated, set D_1 to 0.
  DT[treat==0, D_1 := 0]
  
  return(DT)
  
}
test <- randomize.ind(df[follow.up==9 & strata_dim=='complete' & 
                           outcome=='y1.1'])


### A function that randomizes families to treatment. ###

randomize.fam <- function(data, var) {
  # INPUTS:
  # data: a data.table
  # var: variable name, either petu or petu.alt
  # OUTPUTS:
  # the same data.table but treatment status 'treat' added (split families 1:1)
  
  
  DT <- data[, mget(colnames(data))]
  
  # Order family numbers from smallest to largest:
  DT.fam <- DT[, .(cluster = unique(get(var)))][order(cluster)]
  
  # Random number for each family, order from largest to smallest:
  DT.fam[, rand.no := runif(nrow(DT.fam))]
  DT.fam <- DT.fam[order(-rand.no)]
  
  # 50% of the families to the treatment:
  DT.fam[1:floor(1/2 * nrow(DT.fam)), treat := 1]
  DT.fam[is.na(treat), treat := 0]
  
  # Merge the treatment status to DT:
  DT <- merge(DT, DT.fam, by.x=var, by.y='cluster', all.x=TRUE)
  setnames(DT, old=var, new='cluster')
  
  # For those not treated, set D_1 to 0.
  DT[treat==0, D_1 := 0]
  
  return(DT)
  
}
test <- randomize.fam(df[follow.up==9 & strata_dim=='complete' & 
                           outcome=='y1.1'], var='petu.alt')


### A function that estimates the effect, its variance, and T-statistic using 
# several different regression formulas. ###

estimate <- function(data, method, model='Y_i(0)=Y_i(1)', clustering) {
  # INPUTS:
  # data: an output (data.table) of randomize()
  # method:
  #   ols: no covariates
  #   ols_age_gender: age and gender fixed effects
  #   ols_Ypre: fixed effects for baseline outcome
  #   ols_Ypre_age_gender: fixed effects for baseline outcome, age, and gender
  #   ols_rich: fixed effects for baseline outcome, age, gender, municipality, 
  #               income percentile, and language
  # model: affects the true ATE  
  #   'Y_i(0)=Y_i(1)' (ATE=0 & no heterogeneity)
  # clustering: 0 for heteroskedasticity-robust SEs, 1 for clustering by family
  # OUTPUTS:
  
  
  DT <- data[, mget(colnames(data))]
  
  # The true ATE depends on the outcome model:
  if (model=='Y_i(0)=Y_i(1)') { true.ate <- 0 }
  
  share.treated <- DT[, mean(treat==1)]
  control.mean <- DT[treat==0, mean(Y_post)]
  
  
  if(method=='ols') {
    
    # Reduced form:
    rf <- fixest::feols(Y_post ~ treat, data=DT)
    
    # Two stage least squares:
    tsls <- fixest::feols(Y_post ~ 1 | D_1 ~ treat, data=DT)
    
    
  } else if (method=='ols_age_gender') {
    
    # Reduced form:
    rf <- fixest::feols(Y_post ~ treat | ika + sukup, data=DT)
    
    # Two stage least squares:
    tsls <- fixest::feols(Y_post ~ 0 | ika + sukup | D_1 ~ treat, data=DT)
    
    
  } else if (method=='ols_Ypre') {
    
    # Reduced form:
    rf <- fixest::feols(Y_post ~ treat | Y_pre, data=DT)
    
    # Two stage least squares:
    tsls <- fixest::feols(Y_post ~ 0 | Y_pre | D_1 ~ treat, data=DT)
    
    
  } else if (method=='ols_Ypre_age_gender') {
    
    # Reduced form:
    rf <- fixest::feols(Y_post ~ treat | Y_pre + ika + sukup, data=DT)
    
    # Two stage least squares:
    tsls <- fixest::feols(Y_post ~ 0 | Y_pre + ika + sukup | D_1 ~ treat, 
                          data=DT)
    
  } else if (method=='ols_rich') {
    
    # Reduced form:
    rf <- fixest::feols(Y_post ~ treat | Y_pre + ika + sukup + kunta31_12 + 
                          kturaha_percentile + kieli_k,
                        data=DT)
    
    # Two stage least squares:
    tsls <- fixest::feols(Y_post ~ 0 | Y_pre + ika + sukup + kunta31_12 + 
                            kturaha_percentile + kieli_k | 
                            D_1 ~ treat, data=DT)
    
  } 
  
  
  # Collect results (reduced form):
  
  if(clustering==0) {
    reg <- summary(rf, vcov = 'hetero')$coeftable
  } else if (clustering==1) {
    reg <- summary(rf, cluster = 'cluster')$coeftable }
  
  effect <- reg['treat', 'Estimate']
  se <- reg['treat', 'Std. Error']
  variance <- reg['treat', 'Std. Error']^2
  t_stat <- (effect - true.ate) / reg['treat', 'Std. Error']
  results.rf <- data.table(effect, true.ate, se, variance, t_stat, 
                           share.treated, control.mean, clustering, type='rf')
  
  # Collect results (2SLS):
  
  if(clustering==0) {
    reg <- summary(tsls, vcov = 'hetero')$coeftable
  } else if (clustering==1) {
    reg <- summary(tsls, cluster = 'cluster')$coeftable }
  
  effect <- reg['fit_D_1', 'Estimate']
  se <- reg['fit_D_1', 'Std. Error']
  variance <- reg['fit_D_1', 'Std. Error']^2
  t_stat <- (effect - true.ate) / reg['fit_D_1', 'Std. Error']
  results.tsls <- data.table(effect, true.ate, se, variance, t_stat, 
                             share.treated, control.mean, clustering, 
                             type='2SLS')
  
  results <- rbind(results.rf, results.tsls)
  return(results)
  
}

estimate(data=test, method='ols', clustering=0)
estimate(data=test, method='ols_age_gender', clustering=0)
estimate(data=test, method='ols_Ypre', clustering=0)
estimate(data=test, method='ols_Ypre_age_gender', clustering=0)
estimate(data=test, method='ols_rich', clustering=0)
estimate(data=test, method='ols_rich', clustering=1)


### A function that conducts the Monte Carlo analysis 2000 repetitions) for a 
# given specification. ###

monte.carlo <- function(data, follow_up, outcome.model='Y_i(0)=Y_i(1)', otc,
                        strata_dimension, method.est, rand.type, 
                        petu.var='NA') {
  # INPUTS:
  # outcome.model: 'Y_i(0)=Y_i(1)' (ATE=0 & no heterogeneity)
  # otc: 'y1.1', 'y1.2'
  # follow_up: either 6, 9 or 12 months
  # strata_dimension:
  #   'age_gender' for stratified randomization by age*gender
  #   'Y_baseline' for stratified randomization by baseline Y
  #   'complete' for completely randomized trial
  # method.est:
  #   ols: no covariates
  #   ols_age_gender: age and gender fixed effects
  #   ols_Ypre: fixed effects for baseline outcome
  #   ols_Ypre_age_gender: fixed effects for baseline outcome, age, and gender
  #   ols_rich: fixed effects for baseline outcome, age, gender, municipality, 
  #               income percentile, and language
  # rand.type: 'individual' or 'family'
  # petu.var: 'petu' or 'petu.alt'
  # OUTPUTS:
  # a data.table containing key statistics from the simulation

  
  # No need to sample repeatedly in the fixed finite population perspective:
  DT <- data[follow.up==follow_up & strata_dim==strata_dimension & outcome==otc]
  rep.bstrap <- 2000
  set.seed(12345)
  
  
  results <- lapply(c(1:rep.bstrap), function(i) {
    
    # Print every 50th bootstrap iteration:
    if(i %% 50 == 0) { print(paste('Iteration:', i))}
    
    if(rand.type=='individual') {
      
      df <- randomize.ind(data=DT)
      estimates <- estimate(data=df, method = method.est, model = outcome.model,
                            clustering = 0)
      
    } else if(rand.type=='family') {
      
      df <- randomize.fam(data=DT, var=petu.var)
      estimates <- estimate(data=df, method = method.est, model = outcome.model,
                            clustering = 1)
      
    }
    
  })
  results <- do.call(rbind.data.frame, results)
  results <- results[, mget(colnames(results))]
  
  results[, ':=' (follow.up=follow_up, model=outcome.model, outcome=otc,
                  randomization=strata_dimension, randomization.type=rand.type,
                  estimator=method.est, petu.type=petu.var)]
  
  return(results)
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Simulations: randomize by individual. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


follow.ups <- c(9)
randomization.type <- 'individual'

specs <- list(
  # Complete randomization; no covariates:
  list(strata_dimension='complete', method.est='ols'),
  # Complete randomization; age and gender fixed effects:
  list(strata_dimension='complete', method.est='ols_age_gender'),
  # Complete randomization; baseline Y fixed effects:
  list(strata_dimension='complete', method.est='ols_Ypre'),
  # Complete randomization; fixed effects for baseline Y, age and gender:
  list(strata_dimension='complete', method.est='ols_Ypre_age_gender'),
  # Complete randomization; 
  #   fixed effects for baseline outcome, age, gender, municipality, 
  #   income percentile, language
  list(strata_dimension='complete', method.est='ols_rich'),
  # Stratified randomization by age and gender; age and gender fixed effects:
  list(strata_dimension='age_gender', method.est='ols_age_gender'),
  # Stratified randomization by age and gender; 
  #   fixed effects for baseline Y, age, and gender:
  list(strata_dimension='age_gender', method.est='ols_Ypre_age_gender'),
  # Stratified randomization by baseline Y, baseline Y fixed effects:
  list(strata_dimension='Y_baseline', method.est='ols_Ypre'),
  # Stratified randomization by baseline Y; 
  #   fixed effects for baseline Y, age, and gender:
  list(strata_dimension='Y_baseline', method.est='ols_Ypre_age_gender')
)


# Conduct the simulations (measure running time):
t <- Sys.time()
print(t)

results <- lapply(follow.ups, function(fup) {
  
  results.specs <- lapply(specs, function(specification) {
    print(fup)
    
    mc <- monte.carlo(
      data = df, 
      otc = 'y1.1',
      follow_up = fup,
      strata_dimension = specification$strata_dimension, 
      method.est = specification$method.est,
      rand.type = randomization.type
    )
    
  })
  results.specs <- rbindlist(results.specs)
  
})
results <- rbindlist(results)

data.table::fwrite(results, file=output_results_ind)
print(Sys.time())

Sys.time() - t


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Simulations: randomize by family. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


age.limit <- 70

outcomes <- c('y1.1', 'y1.2')
follow.up <- 7
randomization.type <- 'family'
cluster.types <- 'petu'

specs <- list(
  # Complete randomization; 
  #   fixed effects for baseline outcome, age, gender, municipality, 
  #   income percentile, language
  list(strata_dimension='complete', method.est='ols_rich')
)


# Conduct the simulations (measure running time):

t <- Sys.time()
print(t)

results <- lapply(outcomes, function(outcome) {
  
  results.specs <- lapply(specs, function(specification) {
    
    results.petu <- lapply(cluster.types, function(clstr) {
      print(outcome)
      
      mc <- monte.carlo(
        data = df.IU[ika < age.limit], 
        otc = outcome,
        follow_up = follow.up,
        strata_dimension = specification$strata_dimension, 
        method.est = specification$method.est,
        rand.type = randomization.type,
        petu.var = clstr
      )
      
    })
    results.petu <- rbindlist(results.petu)
    
  })
  results.specs <- rbindlist(results.specs)
  
})
results <- rbindlist(results)

Sys.time() - t

data.table::fwrite(results, file=output_results_fam_IU)

# End.
