
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script 2_construct_analysis_data.R      ###
###                 Replication file.                 ###
###                    2024 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Read and tidy datasets for Monte Carlo simulations (Ostrobothnia).
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data

source("~/Desktop/work/DATAINFRA/functions.R") # some helper functions

# Input folder for Avohilmo:
input_folder <- "~/Desktop/work/DATAINFRA/THL/cleaned"

# Inputs:
input_folk <- "~/Desktop/work/DATAINFRA/TK/raw/folk_yhdistetty_2022.csv"
input_kunta_hva <- "~/Desktop/work/DATAINFRA/misc/raw/kunta_hva_2023.csv"
inputs <- 
  get_file_paths(input_folder)[(grepl('avohilmo', file_name) & year >= 2022) |
                                 grepl('avohilmo_hta', file_name), file_name]
inputs <- paste0(inputs, '.csv')
inputs_avosh <- inputs[grep('avosh_', inputs)]
inputs_hta <- inputs[grep('hta_', inputs)]
print(c(inputs_avosh, inputs_hta))

# Outputs:
output_montecarlo <- here('data', 'rct_montecarlo.csv')

###
###


# Study window:
min_date <- as.Date('2022-04-01')
max_date <- as.Date('2024-03-31')
treatment <- as.Date('2023-04-01')


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Study population and their characteristics. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read FOLK data:
vars <- c('shnro', 'petu', 'ika', 'sukup', 'kieli_k', 'maka', 'kunta31_12', 
          'posti_alue', 'kturaha_ekv')
folk <- data.table::fread(input_folk, select=vars, 
                          na.strings = c(NA_character_, ''))

# Keep only people residing in Ostabothnia wellbeing services county.
# Kristiinankaupunki (287) is excluded because of outsourced PPC services.
munies_keep <- data.table::fread(input_kunta_hva, encoding='UTF-8',
                                 na.strings = c(NA_character_, ''))
munies_keep <- munies_keep[hva=='Pohjanmaan hyvinvointialue', kuntanro]
munies_keep <- setdiff(munies_keep, 287)
folk <- folk[kunta31_12 %in% munies_keep]

# If not in family population, use person ID as family ID:
folk[, petu := as.character(petu)]
folk[is.na(petu), petu := shnro]

# Families with minors:
minors <- folk[, .(N = sum(ika < 18)), by='petu'][N > 0, petu]
folk[petu %in% minors, petu.alt := petu]
folk[is.na(petu.alt), petu.alt := shnro]

# N:
folk[, uniqueN(shnro)]
folk[, uniqueN(petu)]
folk[, uniqueN(petu.alt)]

# Income deciles and percentiles:
folk <- folk[order(kturaha_ekv)]
folk[, kturaha_decile :=
       .bincode(kturaha_ekv,
                quantile(kturaha_ekv, probs= 0:10/10, na.rm=TRUE),
                right = FALSE, include.lowest = TRUE)]
folk[, kturaha_percentile :=
       .bincode(kturaha_ekv,
                quantile(kturaha_ekv, probs= 0:100/100, na.rm=TRUE),
                right = FALSE, include.lowest = TRUE)]

# Expand (several outcomes and follow-up lengths):
dt <- CJ(shnro = folk[, unique(shnro)],
         follow.up = c(9, 10, 11, 12),
         outcome = c('b.1', 'b.2', 'b.3'))
dt <- merge(dt, folk, by='shnro', all.x = TRUE)


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Data on PPC utilization. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Outpatient visits:

avosh <- lapply(inputs_avosh, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder, input_name, sep='/')
  
  # Read the datasets and select covariates:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_alkoi_pvm_c >= min_date & 
            kaynti_alkoi_pvm_c <= max_date &
            shnro %in% folk[, shnro] & 
            sektori==1 &
            kaynti_palvelumuoto=='T11' &
            kaynti_luonne=='SH' &
            kaynti_kavijaryhma==1 &
            kaynti_ammattiluokka %in% c('SH', 'LK'),
          .(avohilmoid, shnro, kaynti_yhteystapa, kaynti_alkoi_pvm_c, 
            kaynti_ammattiluokka, kaynti_alkoi_aika)]
  
})
avosh <- rbindlist(avosh)


# Care needs assessments:

hta <- lapply(inputs_hta, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder, input_name, sep='/')
  
  # Read the datasets and select covariates:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[hta_pvm_c >= min_date & 
            hta_pvm_c <= max_date &
            shnro %in% folk[, shnro] & 
            sektori==1,
          .(avohilmoid, shnro, hta_pvm_c, hta_aika, hta_ammattiluokka)]
  dt[, kaynti_yhteystapa :='hta']
 
})
hta <- rbindlist(hta)

# We include as separate rows those care needs assessments for which:
# - we observe no timestamp for the visit (kaynti_alkoi) OR
# - we observe that the timestamp for the visit differs from the time stamp
#   for the triage.

hta <- merge(hta, avosh[, .(avohilmoid, kaynti_alkoi_aika, kaynti_alkoi_pvm_c)],
             by='avohilmoid', all.x = TRUE)

hta <- hta[is.na(kaynti_alkoi_pvm_c) |
            !(kaynti_alkoi_aika == hta_aika & kaynti_alkoi_pvm_c == hta_pvm_c)]

# Tidy columns:
hta[, ':=' (kaynti_alkoi_aika=NULL, kaynti_alkoi_pvm_c=NULL, hta_aika=NULL)]
setnames(hta, old = c('hta_pvm_c', 'hta_ammattiluokka'),
         new = c('kaynti_alkoi_pvm_c', 'kaynti_ammattiluokka'))


# Bind rows:
phc <- rbind(hta, avosh, fill=TRUE)

# Overview on the data:

t <- phc[, .(share_percent = round(100 * .N / nrow(phc), digits=4), .N),
         by='kaynti_ammattiluokka'][order(-share_percent)]
print(t)

t <- phc[, .(share_percent = round(100 * .N / nrow(phc), digits=4), .N),
         by='kaynti_yhteystapa'][order(-share_percent)]
print(t)


# Include care needs assessments and in-person visits or telemedicine or 
# professional-to-professional interactions conducted by nurses or physicians in 
# digital PPC clinics:

phc <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R10', # "in-person" visits
                                  'R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH','LK'))]

setnames(phc, old='kaynti_alkoi_pvm_c', new='date')
phc[, kaynti_alkoi_aika := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Create analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Person-date panels where included contact types differ:

phc.b.1 <- phc[kaynti_yhteystapa == 'R10' & 
                   kaynti_ammattiluokka %in% c('SH', 'LK'), 
                 .(contacts = .N), by=c('shnro', 'date')]
phc.b.1[, outcome := 'b.1']

phc.b.2 <- 
  phc[kaynti_yhteystapa=='hta' |
        (kaynti_yhteystapa %in% c('R50','R51','R52','R55','R56') & # telemed
           kaynti_ammattiluokka=='SH'), 
      .(contacts = .N), by=c('shnro', 'date')]
phc.b.2[, outcome := 'b.2']

phc.b.3 <- 
  phc[(kaynti_yhteystapa %in% c('R50','R51','R52','R55','R56', # telemedicine
                                'R60', 'R71') & # prof-to-prof interactions) 
         kaynti_ammattiluokka=='LK'), 
      .(contacts = .N), by=c('shnro', 'date')]
phc.b.3[, outcome := 'b.3']

phc <- rbind(phc.b.1, phc.b.2, phc.b.3)


# Create separate datasets for different follow-ups (9, 10, 11, 12 months):

phc.9kk <- phc[date < as.Date('2024-01-01')]
phc.9kk[, post := as.integer(date >= treatment)]

phc.10kk <- phc[date < as.Date('2024-02-01')]
phc.10kk[, post := as.integer(date >= treatment)]

phc.11kk <- phc[date < as.Date('2024-03-01')]
phc.11kk[, post := as.integer(date >= treatment)]

phc.12kk <- phc[date < as.Date('2024-04-01')]
phc.12kk[, post := as.integer(date >= treatment)]


# Count visit dates before and after treatment and pivot wider:

phc.9kk <- phc.9kk[, .(contact.days = .N), by=c('shnro', 'post', 'outcome')]
phc.9kk <- dcast(phc.9kk, shnro + outcome ~ paste0('contact.days.post', post), 
                 value.var = 'contact.days')
phc.9kk[, follow.up := 9]

phc.10kk <- phc.10kk[, .(contact.days = .N), by=c('shnro', 'post', 'outcome')]
phc.10kk <- dcast(phc.10kk, shnro + outcome ~ paste0('contact.days.post', post), 
                 value.var = 'contact.days')
phc.10kk[, follow.up := 10]

phc.11kk <- phc.11kk[, .(contact.days = .N), by=c('shnro', 'post', 'outcome')]
phc.11kk <- dcast(phc.11kk, shnro + outcome ~ paste0('contact.days.post', post), 
                 value.var = 'contact.days')
phc.11kk[, follow.up := 11]

phc.12kk <- phc.12kk[, .(contact.days = .N), by=c('shnro', 'post', 'outcome')]
phc.12kk <- dcast(phc.12kk, shnro + outcome ~ paste0('contact.days.post', post), 
                  value.var = 'contact.days')
phc.12kk[, follow.up := 12]

phc <- rbind(phc.9kk, phc.10kk, phc.11kk, phc.12kk)


# Merge and impute zeroes:
dt <- merge(dt, phc, by=c('shnro', 'follow.up', 'outcome'), all.x = TRUE)
dt[is.na(contact.days.post0), contact.days.post0 := 0]
dt[is.na(contact.days.post1), contact.days.post1 := 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Construct strata. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# First, stratify by age and gender.
# Combine those cells by gender where age >= 90:

strata.1 <- CJ(ika = dt[, unique(ika)], sukup = dt[, unique(sukup)])
strata.1[, stratum.1 := rownames(strata.1)]
dt <- merge(dt, strata.1, by=c('ika', 'sukup'), all.x = TRUE)

stratum.m <- strata.1[ika==90 & sukup==1, stratum.1]
dt[ika >= 90 & sukup==1, stratum.1 := stratum.m]
stratum.f <- strata.1[ika==90 & sukup==2, stratum.1]
dt[ika >= 90 & sukup==2, stratum.1 := stratum.f]

# The smallest stratum size:
test <- dt[follow.up==9, .N, by=c('stratum.1')]
test[, min(N)]


# Next, stratify by the number of leading PPC contacts.

# Initialize strata that may be "too granular" (N_s < 2):
strata.2 <- dt[follow.up==9 & outcome=='b.1']
strata.2[, strata := contact.days.post0]

# Compute stratum sizes and sort by baseline visits (largest to smallest):
dt.help <- strata.2[, .N, by='strata'][order(-strata)]


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
strata.2 <- aggr.strata.to.next(data = strata.2, data.help = dt.help)

# The smallest stratum size:
test <- strata.2[follow.up==9 & outcome=='b.1', .N, by=c('strata')]
test[, min(N)]

# Merge stratum information to dt:
strata.2 <- strata.2[, .(shnro, strata)]
setnames(strata.2, old='strata', new='stratum.2')
strata.2[, stratum.2 := as.character(stratum.2)]
dt <- merge(dt, strata.2, by='shnro', all.x=TRUE)

setnames(dt, old=c('contact.days.post0', 'contact.days.post1'),
         new=c('Y_pre', 'Y_post'))

# Y_post should be annualized:
dt[, Y_post := as.double(Y_post)]
dt[follow.up==9, Y_post := 12 * Y_post / 9]
dt[follow.up==10, Y_post := 12 * Y_post / 10]
dt[follow.up==11, Y_post := 12 * Y_post / 11]

# Finally, create one stratum for simple randomization:
dt[, stratum.3 := '1']

# Pivot longer:
dt <- melt(
  dt, id.vars=c('shnro', 'petu', 'petu.alt', 'ika', 'sukup', 'follow.up', 
                'kturaha_ekv', 'maka', 'kieli_k', 'kturaha_decile', 
                'kturaha_percentile', 'kunta31_12', 'posti_alue',
                'Y_pre', 'Y_post', 'outcome'),
  measure.vars=c('stratum.1', 'stratum.2', 'stratum.3'),
  variable.name = 'strata_dim', value.name = 'strata')

dt[, strata_dim := as.character(strata_dim)]
dt[strata_dim=='stratum.1', strata_dim := 'age_gender']
dt[strata_dim=='stratum.2', strata_dim := 'Y_baseline']
dt[strata_dim=='stratum.3', strata_dim := 'complete']


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Simulate D_1. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We need to simulate the number of digital clinic contacts for each individual, 
# representing potential outcome (D_1) if the person is offered access to the 
# digital clinic.

# We will later improve this simulation using real-world data from other
# wellbeing services counties. 

# For now, we want to satisfy the following constraints:
# 0.23 contacts per resident, 5.3% had at least one contact, and
# 4.4 contacts per user.
# We use the negative binomial distribution with mu=0.23 and size=0.0225.
100 * (1 - dnbinom(0, mu=0.23, size=0.0225)) # 5.29 % had at least one contact
set.seed(123)
draw <- rnbinom(nrow(dt[follow.up==9 & outcome=='b.1' & strata_dim=='complete']), 
               mu=0.23, size=0.0225)
100 - 100 * sum(draw==0) / length(draw) # 5.24 % had at least one contact
mean(draw) # 0.23 contacts per resident
mean(draw[draw > 0]) # 4.38 contacts per client

draw <- data.table(shnro = dt[, unique(shnro)],
                   D_1 = draw)

dt <- merge(dt, draw, by='shnro', all.x = TRUE)

# Save:
data.table::fwrite(dt, file=output_montecarlo)

# End.
