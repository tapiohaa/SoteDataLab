
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###   r-script 2_randomization_ostrobothnia.R     ###
###                   2025 TH, MS, AO             ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Conducts the randomization for the Ostrobothnia 
# Digital Clinic RCT.
rm(list=ls())

# Install and load the following packages:
library(thlConnect)       # Connection to database.
library(data.table)       # Mutating and aggregating data.
library(openxlsx)         # Save as excel file.


# Set seed for randomization:
seed <- 14031608          # KIITOS MARKKU JA SUVI ! 


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Extract study population from DVV. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# The population of Ostrobothnia is extracted from DVV in an SQL script.
# The table contains the following variables:
#   - hetu, etunimi, sukunimi, 
#     katunimi, katunumero, huoneistokirjain, huoneistonumero,
#     postinumero, postitoimipaikka, kotikunta, asuinpaikantunnus, 
#     ika, sukupuoli_mies, kielikoodi.


# Open connection to the database and conduct a query:
terveys <- thlDbConnect("database_name", case="database", workgroup="database_alias")
query <- paste0("select * from user.THL2024_6688_vtj")
data <- dbGetQuery(terveys, query)

# Transform to data.table and tidy column names:
dt <- as.data.table(data)
colnames(dt) <- tolower(colnames(dt))

# Tidy apartment number and gender:
dt[, huoneistonumero := as.integer(huoneistonumero)]
dt[, sukupuoli_mies := as.integer(sukupuoli == 'MIES')]

# Sample size (raw data):
n.ind.1.raw <- nrow(dt) # the number of individuals
n.hh.1.raw <- dt[, uniqueN(asuinpaikantunnus)] # the number of households


# The permanent address is missing for less than 1 % of the population:
100 * dt[, mean(is.na(katunimi))] 
# About half of this is explained by two facts:
# 1: Some have applied for address confidentiality.
# 2: Some of those who do not have a permanent address have a
#     temporary address.

# Drop those for whom permanent address is missing:
dt <- dt[!is.na(katunimi)]

# Sample size (address not missing):
n.ind.2.addr <- nrow(dt) # the number of individuals
n.hh.2.addr <- dt[, uniqueN(asuinpaikantunnus)] # the number of households


# We aim to exclude people (often old) in institutional care:
# Our definition: a residence code is an institution if it has more than
# 2 individuals aged more than 80 OR if it has more than 4 individuals aged
# more than 60.

pudotettavat.asuinpaikantunnukset <- 
  dt[, .(hlomaara = .N, iakkaita.80 = sum(ika > 80),
         iakkaita.60 = sum(ika > 60)), by=asuinpaikantunnus
     ][iakkaita.80 > 2 | iakkaita.60 > 4, asuinpaikantunnus]

dt <- dt[!(asuinpaikantunnus %in% pudotettavat.asuinpaikantunnukset)]

# Sample size (not in institutional care):
n.ind.3.inst <- nrow(dt) # the number of individuals
n.hh.3.inst <- dt[, uniqueN(asuinpaikantunnus)] # the number of households

# Print the distribution of household sizes:
dt[, .(hlomaara = .N), by=asuinpaikantunnus
   ][, .N, by=hlomaara
     ][order(hlomaara)]

# Print the share of missing values:
100 *  colMeans(is.na(dt))


# Finally, drop individuals residing in Kristiinankaupunki which is 
# excluded from the pilot because the PPC services are outsourced there:
dt <- dt[kotikunta != '287']

# Sample size (not Kristiinankaupunki):
n.ind.4.krist <- nrow(dt) # the number of individuals
n.hh.4.krist <- dt[, uniqueN(asuinpaikantunnus)] # the number of households


# Group those aged 100 or more:
dt[, uniqueN(hetu), by='ika'][ika > 90][order(ika)]
dt[ika >= 100, ika := 100]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) File paths: outputs. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Set working directory:
setwd("//user_directory/THL2024_6688")

# Outputs:

output_fiona <- "./data/TUTKPALV_U1921_pohjanmaa_rct.csv"
# This table is sent to Statistics Finland for research purposes.
# variables: 
#   - hetu, asuinpaikantunnus, kotikunta, koeryhmassa, strata

output_flowmedik <- "./data/VIRALLINEN_pohjanmaa_pilottiryhma_sisaan.csv"
# This table is sent to Flowmedik so that the digital clinic 
# platform could let in only the treatment group.
# variables: 
#   - hetu 

output_hva <- "./data/VIRALLINEN_pohjanmaan_vaesto_osoitteet.xlsx"
# This table should is sent to Pohjanmaa HVA so that the treatment group
# individuals can be informed via letters.
# variables: 
#   - etunimi, sukunimi, katunimi, katunumero, huoneistokirjain, 
#     huoneistonumero, postinumero, postitoimipaikka, 
#     kotikunta, asuinpaikantunnus, kielikoodi, strata

output_check.1 <- "./data/FINAL_randomization_check.1.csv"
output_check.2 <- "./data/FINAL_randomization_check.2.csv"
output_check.3 <- "./data/FINAL_randomization_check.3.csv"
# These tables help to assess whether everything worked as intended.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Household size strata. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Randomization will be done at household level, stratified by household size.
# This means that 1) compute household sizes and 2) within each household size,
# randomize 50 % of the households to treatment. 

# Note that we need to make sure that each household size has at least two 
# households so that the cell can have both treated and control households.

# The following function should join adjacent households so that each stratum
# (household size) has at least two households.


# Initialize strata that may be "too granular" (N_s < 2):
strata <- dt[, .(strata = .N), by='asuinpaikantunnus']
dt <- merge(dt, strata, by='asuinpaikantunnus', all.x = TRUE)

# Compute stratum sizes and sort by baseline visits (largest to smallest):
dt.help.pre <- 
  unique(dt[, .(asuinpaikantunnus, strata)])[, .N, by='strata'
  ][order(-strata)]
dt.help.pre[, ennen_vai_jalkeen_korjauksen := 'ennen']
print(dt.help.pre)
# NOTE: above, we should observe thousands of small households, but for the 
# largest households we could observe only one household per household size.


### A function that (by looping) merges too small strata to the next stratum,
# merging from above. ###

aggr.strata.to.next <- function(data, data.help) {
  # INPUTS:
  # data: a data.table containing the trial data with 'strata' covariate
  # data.help: a smaller data.table containing sample sizes ('N') for each 
  #     strata, ordered so that the merging can be done from above.
  
  
  DT <- data[, mget(colnames(data))]
  DT.help <- data.help[, mget(colnames(data.help))]
  
  # Store the original strata labels before aggregation:
  strata.orig <- DT.help[, strata]
  
  # Initialize a table that will collect how the strata labels are updated:
  i <- 0
  strata.changes <- data.table(old = integer(length = 0),
                               new = integer(length = 0))
  
  # Loop over strata (from the top to the bottom) until N_s >= 2 for all strata:
  
  while(DT.help[, min(N)] < 2) {
    i <- i + 1
    
    # Aggregate if N_s < 2 for a given stratum:
    
    if(DT.help[strata==strata.orig[i], N] < 2) {
      
      # Combine a stratum that is too small to the next stratum:
      DT.help[strata==strata.orig[i+1], 
              N := N + DT.help[strata==strata.orig[i], N]]
      DT.help <- DT.help[strata != strata.orig[i]]
      
      # Store the information on which strata were merged:
      strata.changes <- rbind(
        strata.changes, 
        data.table(old = strata.orig[i], new = strata.orig[i+1]))
      
    }
    
  }
  
  # Loop over the merges and update the "strata" variable:
  for(i in 1:nrow(strata.changes)) {
    DT[strata==strata.changes[, old][i], strata := strata.changes[, new][i]]
  }
  
  return(DT)
  
}

# Merge too small strata to the next stratum:
dt <- aggr.strata.to.next(data = dt, data.help = dt.help.pre)

# Check that the code works:
dt.help.post <- 
  unique(dt[, .(asuinpaikantunnus, strata)])[, .N, by='strata'
  ][order(-strata)]
dt.help.post[, ennen_vai_jalkeen_korjauksen := 'jalkeen']
print(dt.help.post)
# NOTE: above, smallest N should likely be 2 (1 is not allowed anymore).


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Randomize. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Important to set seed so that the randomization can be reproduced.
set.seed(seed) 


# Addresses and strata
dt.rand <- unique(dt[, .(asuinpaikantunnus, strata)])
dt[, strata := NULL]


### A function that randomizes by household id, stratified by household size. ###

randomize <- function(data.rand, data.ind) {
  # INPUTS:
  # data.rand: a data.table at the level of randomization (household) with 
  #           variable 'strata' with N_s >= 2 for all strata.
  # data.ind: a data.table at the individual level containing study population
  # OUTPUTS:
  # the same data.table as data.ind but treatment status added (split 1:1)
  
  
  DT <- data.rand[, mget(colnames(data.rand))]
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
      rand.numbers <- runif(.N)
      ord <- order(rand.numbers)
      treat.fill[ord[seq_len(n_treat)]] <- 1
      list(treat = treat.fill)
    }, by=strata][, treat]
  
  DT[, koeryhmassa := treatment.rand]
  
  # Merge treatment status to individual level data:
  DT <- merge(data.ind, DT, by='asuinpaikantunnus', all.x = TRUE)
  
  return(DT)
  
}

dt <- randomize(data.rand=dt.rand, data.ind=dt)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Randomize recipients of the letters (one per household). ####
### ### ### ### ### ### ### ### ### ### ### ### ###


set.seed(seed)

select_recipients <- function(data) {
  
  # Create a copy to avoid modifying the original data
  dt_copy <- copy(data)
  
  # First name should be observed and the person should be in
  # the treatment group:
  dt_copy <- dt_copy[!is.na(etunimi) & koeryhmassa==1]
  
  # Add a random number to each record for randomization:
  dt_copy[, random := runif(.N)]
  
  # Identify adults and eligible minors
  dt_copy[, is_adult := ika >= 18]
  dt_copy[, is_eligible_minor := ika >= 15 & ika < 18]
  
  # Flag households with adults
  dt_copy[, has_adults := any(is_adult), by = asuinpaikantunnus]
  
  # Step 1: Select from households with adults
  adults_selection <- dt_copy[
    is_adult == TRUE & has_adults == TRUE,
    .SD[which.max(random)],
    by = asuinpaikantunnus
  ]
  
  # Step 2: Select from households without adults but with eligible minors
  minors_selection <- dt_copy[
    has_adults == FALSE & is_eligible_minor == TRUE,
    .SD[which.max(random)],
    by = asuinpaikantunnus
  ]
  
  # Combine the results
  recipients <- rbind(adults_selection, minors_selection)
  
  # Return the vector of person_ids
  return(recipients$hetu)

}

# Get the vector of randomly selected recipient IDs
recipient_ids <- select_recipients(dt)


# Create an indicator on whether the person will be sent a letter:
dt[, selected.letter := as.integer(hetu %in% recipient_ids)]

# Check that there is only one letter per household:
dt[selected.letter==1, .(N=uniqueN(hetu)), by='asuinpaikantunnus'][, unique(N)]

# Check the age distribution:
dt[selected.letter==1, .N, by='ika'][order(ika)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Assess balance. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# The number of treated individuals and households:
n.treated.individuals <- dt[, sum(koeryhmassa)]
n.treated.households <- dt[koeryhmassa==1, uniqueN(asuinpaikantunnus)]
print(c(n.treated.individuals, n.treated.households))


# The share of treated individuals and households:
share.treated.individuals <- dt[, mean(koeryhmassa)]
share.treated.households <- 
  unique(dt[, .(asuinpaikantunnus, strata, koeryhmassa)])[, mean(koeryhmassa)]
print(c(share.treated.individuals, share.treated.households))


# The share of men:
share.men.treatment <- dt[koeryhmassa==1, mean(sukupuoli_mies==1, na.rm=TRUE)]
share.men.control <- dt[koeryhmassa==0, mean(sukupuoli_mies==1, na.rm=TRUE)]
print(c(share.men.treatment, share.men.control))


# The mean age:
mean.age.treatment <- dt[koeryhmassa==1, mean(ika)]
mean.age.control <- dt[koeryhmassa==0, mean(ika)]
print(c(mean.age.treatment, mean.age.control))


# The share of treated individuals and households by strata:

dt.help.1 <- merge(
  dt[, .(share.treated.individuals = mean(koeryhmassa)), by='strata'],
  unique(dt[, .(asuinpaikantunnus, strata, koeryhmassa)])[, .(share.treated.households = mean(koeryhmassa)), by='strata'],
  by='strata', all.x = TRUE
)
print(dt.help.1)


# The share of treated men and the mean age by strata:

# Men:
dt.help.2 <- dcast(dt[, .(men = mean(sukupuoli_mies==1, na.rm=TRUE)), 
                      by=c('koeryhmassa', 'strata')], 
                   strata ~ paste0('share.men.treated_', koeryhmassa), 
                   value.var = 'men')

# Age:
dt.help.3 <- dcast(dt[, .(age = mean(ika)), by=c('koeryhmassa', 'strata')], 
                   strata ~ paste0('mean.age.treated_', koeryhmassa), 
                   value.var = 'age')

dt.help.4 <- merge(dt.help.2, dt.help.3, by='strata', all.x = TRUE)
dt.help.4 <- merge(dt.help.1, dt.help.4, by='strata', all.x = TRUE)
print(dt.help.4)


# Is the treatment status same within all households?
varying.treatment.within.household <- 
  nrow(dt[, .(N = uniqueN(koeryhmassa)), by='asuinpaikantunnus'][N > 1])


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# For Fiona (research purposes):
dt.fiona <- dt[, .(hetu, asuinpaikantunnus, kotikunta, koeryhmassa, strata)]
rows.fiona <- nrow(dt.fiona)
print(rows.fiona)
fwrite(dt.fiona, file = output_fiona)

# For Flowmedik (let selected individuals in to the digital clinic):
dt.flowmedik <- dt[koeryhmassa==1, .(hetu)]
rows.flowmedik <- nrow(dt.flowmedik)
print(rows.flowmedik)
fwrite(dt.flowmedik, file = output_flowmedik)

# For Pohjanmaa hva (send letters to the treatment group, one per household):
dt.hva <- dt[koeryhmassa==1 & selected.letter==1, 
             .(etunimi, sukunimi, 
               katunimi, katunumero, huoneistokirjain, huoneistonumero, 
               postinumero, postitoimipaikka, kotikunta, asuinpaikantunnus, 
               kielikoodi, strata)]
rows.hva <- nrow(dt.hva)
hhs.hva <- dt.hva[, uniqueN(asuinpaikantunnus)]
print(rows.hva)
openxlsx::write.xlsx(dt.hva, file = output_hva, overwrite = TRUE)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 8) Tests and checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Save tables to assess whether everything works as intended:

check.1 <- data.table(
  n.ind.1.raw, 
  n.hh.1.raw, 
  n.ind.2.addr, 
  n.hh.2.addr, 
  n.ind.3.inst, 
  n.hh.3.inst, 
  n.ind.4.krist,
  n.hh.4.krist,
  n.treated.individuals, 
  share.treated.individuals,
  n.treated.households,
  share.treated.households,
  share.men.treatment, 
  share.men.control,
  mean.age.treatment,
  mean.age.control,
  varying.treatment.within.household,
  rows.fiona,
  rows.flowmedik,
  rows.hva,
  hhs.hva
)

check.2 <- rbind(dt.help.pre, dt.help.post)

check.3 <- dt.help.4

print(check.1)
print(check.2)
print(check.3)

fwrite(check.1, file = output_check.1)
fwrite(check.2, file = output_check.2)
fwrite(check.3, file = output_check.3)

# End.
