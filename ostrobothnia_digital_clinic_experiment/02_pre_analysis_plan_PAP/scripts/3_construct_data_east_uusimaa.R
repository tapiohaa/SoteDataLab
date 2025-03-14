
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###     r-script 3_construct_data_east_uusimaa.R      ###
###                 Replication file.                 ###
###                    2025 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Read and tidy datasets for Monte Carlo simulations (East Uusimaa).
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # mutating and aggregating data
library(fixest)           # OLS regression.

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
output_montecarlo <- here('data', 'rct_montecarlo_east_uusimaa.csv')

###
###


# Study window:
min_date <- as.Date('2022-09-01')
max_date <- as.Date('2024-03-31')
treatment <- as.Date('2023-09-01')

# Follow-up:
f.up <- 7


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Study population and their characteristics. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read FOLK data:
vars <- c('shnro', 'petu', 'ika', 'sukup', 'kieli_k', 'maka', 'kunta31_12', 
          'posti_alue', 'kturaha_ekv')
folk <- data.table::fread(input_folk, select=vars, 
                          na.strings = c(NA_character_, ''))

# Keep only people residing in East Uusimaa wellbeing services county.
munies_keep <- data.table::fread(input_kunta_hva, encoding='UTF-8',
                                 na.strings = c(NA_character_, ''))
munies_keep <- munies_keep[hva=='Itä-Uudenmaan hyvinvointialue', kuntanro]
folk <- folk[kunta31_12 %in% munies_keep]

# If not in family population, use person ID as family ID:
folk[, petu := as.character(petu)]
folk[is.na(petu), petu := shnro]

# N:
folk[, uniqueN(shnro)]
folk[, uniqueN(petu)]

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


# The population of East Uusimaa is smaller than the population of Ostrobothnia.
# Randomly sample (86,034-folk[, uniqueN(petu)]) families with replacement:
set.seed(1234)
sampled_families <- sample(folk[, unique(petu)], 
                           size=(86034-folk[, uniqueN(petu)]), replace = TRUE)

# Create a data table of sampled families with counts of how many times each 
# family was sampled:
family_counts <- data.table(petu = sampled_families)[, .N, by='petu']

# Replicate each family based on the value in N:
family_counts <- family_counts[rep(1:.N, N)]

# Duplicated families should represent distinct families; update family ID:
family_counts[, ':=' (N = NULL,
                      petu.new = paste(petu, .I, sep='_'))]

# Join the original data with the sample families with counts of how many times
# each family was sampled:
sampled <- merge(folk, family_counts, by='petu', allow.cartesian = TRUE)

# Rbind and tidy column names:
folk <- rbind(folk, sampled, fill=TRUE)
folk[is.na(petu.new), petu.new := petu]
folk[, petu := NULL]
setnames(folk, old='petu.new', new='petu')
folk <- folk[order(shnro)]


# Create linkable shnro:
folk[, shnro.new := paste(shnro, .I, sep='_')]
shnro <- folk[, .(shnro, shnro.new)]


# N (close to the N of Ostrobothnia):
folk[, uniqueN(shnro.new)]
folk[, uniqueN(petu)]


# Expand (several outcomes and follow-up lengths):

dt <- CJ(shnro.new = shnro[, shnro.new],
         follow.up = f.up,
         outcome = c('y1.1', 'y1.2'))

dt <- merge(dt, shnro, by='shnro.new', all.x=TRUE)
dt[, shnro := NULL]
dt <- merge(dt, folk, by='shnro.new', all.x = TRUE)


# Strata: complete randomization:
dt[, ':=' (strata_dim = 'complete', strata = '1')]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 2) Data on digital clinic utilization. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Outpatient visits:

avosh <- lapply(inputs_avosh, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder, input_name, sep='/')
  
  # Read the datasets and select covariates:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_alkoi_pvm_c >= treatment &
             kaynti_alkoi_pvm_c <= max_date &
             shnro %in% folk[, unique(shnro)] & 
             sektori==1 &
             kaynti_palvelumuoto=='T11' &
             kaynti_luonne=='SH' &
             kaynti_kavijaryhma==1 &
             kaynti_ammattiluokka %in% c('SH', 'LK') &
             digiklinikka_hva==1,
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
  dt <- dt[hta_pvm_c >= treatment &
             hta_pvm_c <= max_date &
             shnro %in% folk[, unique(shnro)] & 
             sektori==1 &  
             digiklinikka_hva==1, 
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
hta[, ':=' (kaynti_alkoi_aika=NULL, kaynti_alkoi_pvm_c=NULL)]
setnames(hta, old = c('hta_pvm_c', 'hta_ammattiluokka', 'hta_aika'),
         new = c('kaynti_alkoi_pvm_c', 'kaynti_ammattiluokka', 
                 'kaynti_alkoi_aika'))


# Bind rows:
phc <- rbind(hta, avosh, fill=TRUE)


# Overview on the data:

t <- phc[, .(share_percent = round(100 * .N / nrow(phc), digits=4), .N),
         by='kaynti_ammattiluokka'][order(-share_percent)]
print(t)

t <- phc[, .(share_percent = round(100 * .N / nrow(phc), digits=4), .N),
         by='kaynti_yhteystapa'][order(-share_percent)]
print(t)


# Outcome D.1:
# Include care needs assessments, telemedicine and professional-to-professional 
# interactions conducted by nurses or physicians in digital PPC clinics:

phc <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R10', # "in-person" visits
                                  'R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH','LK'))]


# Count the number of days with contact per person and merge into dt:
phc.aggr <- phc[, .(D_1 = .N), by=c('shnro', 'kaynti_alkoi_pvm_c')
                ][, .(D_1 = .N), by='shnro']
dt <- merge(dt, phc.aggr, by='shnro', all.x = TRUE)

# Impute zeroes if no digital clinic contacts are observed:
dt[is.na(D_1), D_1 := 0]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) Data on traditional PPC utilization. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Outpatient visits:

avosh <- lapply(inputs_avosh, function(input_name) {
  print(input_name)
  source.path <- paste(input_folder, input_name, sep='/')
  
  # Read the datasets and select covariates:
  dt <- data.table::fread(source.path, na.strings = c(NA_character_, ''))
  dt <- dt[kaynti_alkoi_pvm_c >= min_date & 
             kaynti_alkoi_pvm_c <= max_date &
             shnro %in% folk[, unique(shnro)] & 
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
             shnro %in% folk[, unique(shnro)] & 
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
#### 4) Create analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Person-date panels where included contact types differ:

phc.y1.1 <- phc[kaynti_yhteystapa == 'R10' & # in-person visits
                  kaynti_ammattiluokka %in% c('SH', 'LK'), 
                .(contacts = .N), by=c('shnro', 'date')]
phc.y1.1[, outcome := 'y1.1']

phc.y1.2 <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH','LK')), 
      .(contacts = .N), by=c('shnro', 'date')]
phc.y1.2[, outcome := 'y1.2']

phc <- rbind(phc.y1.1, phc.y1.2)


# Create separate datasets for different follow-ups (7 months):

phc.7kk <- phc[date < as.Date('2024-04-01')]
phc.7kk[, post := as.integer(date >= treatment)]


# Count visit dates before and after treatment and pivot wider:

phc.7kk <- phc.7kk[, .(contact.days = .N), by=c('shnro', 'post', 'outcome')]
phc.7kk <- dcast(phc.7kk, shnro + outcome ~ paste0('contact.days.post', post), 
                 value.var = 'contact.days')
phc.7kk[, follow.up := 7]

phc <- rbind(phc.7kk)


# Merge and impute zeroes:
dt <- merge(dt, phc, by=c('shnro', 'follow.up', 'outcome'), all.x = TRUE)
dt[is.na(contact.days.post0), contact.days.post0 := 0]
dt[is.na(contact.days.post1), contact.days.post1 := 0]

setnames(dt, old=c('contact.days.post0', 'contact.days.post1'),
         new=c('Y_pre', 'Y_post'))

# Y_post should be annualized:
dt[, Y_post := as.double(Y_post)]
dt[follow.up==7, Y_post := 12 * Y_post / 7]

# Save:
data.table::fwrite(dt, file=output_montecarlo)

# End.
