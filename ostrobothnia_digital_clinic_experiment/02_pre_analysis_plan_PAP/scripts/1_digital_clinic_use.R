
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 1_digital_clinic_use.R         ###
###                 Replication file.                 ###
###                    2024 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Statistics on digital clinic use in East Uusimaa.
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
  get_file_paths(input_folder)[(grepl('avohilmo', file_name) & year >= 2023) |
                                 grepl('avohilmo_hta', file_name), file_name]
inputs <- paste0(inputs, '.csv')
inputs_avosh <- inputs[grep('avosh_', inputs)]
inputs_hta <- inputs[grep('hta_', inputs)]
print(c(inputs_avosh, inputs_hta))

# Outputs:
output_stats <- here('data', 'rct_digiclinic_stats.csv')

###
###


# Study window (7 months):
min_date <- as.Date('2023-09-01')
max_date <- as.Date('2024-03-31')


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 1) Study population and their characteristics. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read FOLK data:
vars <- c('shnro', 'ika', 'sukup', 'kieli_k', 'maka', 'kunta31_12', 
          'posti_alue', 'kturaha_ekv')
folk <- data.table::fread(input_folk, select=vars, 
                          na.strings = c(NA_character_, ''))

# Keep only people residing in East Uusimaa:
munies_keep <- data.table::fread(input_kunta_hva, encoding='UTF-8',
                                 na.strings = c(NA_character_, ''))
munies_keep <- munies_keep[hva %in%c('Itä-Uudenmaan hyvinvointialue'), kuntanro]
folk <- folk[kunta31_12 %in% munies_keep]

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
  dt <- dt[hta_pvm_c >= min_date &
            hta_pvm_c <= max_date &
            shnro %in% folk[, shnro] & 
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


# Outcome A.1:
# Include care needs assessments, telemedicine and professional-to-professional 
# interactions conducted by nurses or physicians in digital PPC clinics:

phc <- 
  phc[kaynti_yhteystapa=='hta' | # care needs assesments
        (kaynti_yhteystapa %in% c('R10', # "in-person" visits
                                  'R50','R51','R52','R55','R56', # telemedicine
                                  'R60', 'R71') & # prof-to-prof interactions
           kaynti_ammattiluokka %in% c('SH','LK'))]


# Count the number of days with contact per person and merge into folk-data:
phc.aggr <- phc[, .(digi.contacts = .N), by=c('shnro', 'kaynti_alkoi_pvm_c')
                ][, .(digi.contacts = .N), by='shnro']
folk <- merge(folk, phc.aggr, by='shnro', all.x = TRUE)

# Impute zeroes if no digital clinic contacts are observed:
folk[is.na(digi.contacts), digi.contacts := 0]

# In indicator for having used the digital clinic:
folk[, used.digi := as.integer(digi.contacts > 0)]


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 3) r2dw. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Compute standard deviation of the D (digital clinic use) and estimate ar2 as 
# a function of age limit:

age.limit <- 10 : 100

r2dw <- lapply(age.limit, function(i) {

  # Subset by age:
  df <- folk[ika < i]
  
  # ar2:
  
  ols.formula <- used.digi ~ 1 | ika + sukup + kunta31_12 + kturaha_percentile + kieli_k
  # NOTE: Y_pre is not here although it should.
  
  ols <- fixest::feols(ols.formula, data = df) 
  ar2 <- r2(ols, type='ar2')
  
  data.table(age.limit = i, r2dw = ar2)
  
})

r2dw <- rbindlist(r2dw)


### ### ### ### ### ### ### ### ### ### ### ### ###
#### 4) Key summary statistics on digital clinic use. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Merge patient age to phc:
phc <- unique(phc[, .(shnro, kaynti_alkoi_pvm_c)])
phc <- merge(phc, folk[, .(shnro, ika)], by='shnro', all.x = TRUE)


# Key summary statistics on digital clinic use as a function of 
# follow-up and age limit:

stats <- lapply(age.limit, function(i) {
  
  # Age restriction:
  dt <- phc[ika < i]
  n.persons <- folk[ika < i, uniqueN(shnro)]
  
  
  # The cumulative sum of digital clinic contacts by date:
  
  phc.contacts <- dt[, .(total.contacts = .N),by = 'kaynti_alkoi_pvm_c'
                     ][order(kaynti_alkoi_pvm_c)]
  phc.contacts[, ':=' (cum.contacts = cumsum(total.contacts),
                       date = as.integer(kaynti_alkoi_pvm_c))]
  
  # Linear fit:
  ols.contacts <- fixest::feols(
    cum.contacts ~ date, 
    data = phc.contacts[date >= (as.integer(min_date) + 90)])
  
  # Predict:
  future.contacts <- data.table(date = c((as.integer(max_date) + 1) : 
                                           (as.integer(min_date) + 365)))
  future.contacts[, pred := predict(ols.contacts, newdata = future.contacts)]
  
  
  # The cumulative sum of patients by date:
  
  dates <- phc[, sort(unique(kaynti_alkoi_pvm_c))]
  
  phc.patients <- lapply(dates, function(i) {
    df <- dt[kaynti_alkoi_pvm_c <= i]
    data.table(date = i, cum.patients = df[, uniqueN(shnro)])
  })
  phc.patients <- rbindlist(phc.patients)
  phc.patients[, date := as.numeric(date)]
  
  # Linear fit:
  ols.patients <- fixest::feols(
    cum.patients ~ date, 
    data = phc.patients[date >= (as.integer(min_date) + 90)])
  
  # Predict:
  future.patients <- data.table(date = c((as.integer(max_date) + 1) : 
                                           (as.integer(min_date) + 365)))
  future.patients[, pred := predict(ols.patients, newdata = future.patients)]
  
  
  # Collect results to a table:
  
  results <- data.table(
    age.limit = i,
    follow.up = c(9, 10, 11, 12),
    digi.users = c(
      future.patients[date == (as.integer(min_date) + 9*30), pred],
      future.patients[date == (as.integer(min_date) + 10*30), pred],
      future.patients[date == (as.integer(min_date) + 11*30), pred],
      future.patients[date == max(date), pred]),
    digi.contacts = c(
     future.contacts[date==(as.integer(min_date) + 9*30), pred],
     future.contacts[date==(as.integer(min_date) + 10*30), pred],
     future.contacts[date==(as.integer(min_date) + 11*30), pred],
     future.contacts[date == max(date), pred])
  )
  
  results[, ':=' (digiusers.per.cap = digi.users / n.persons,
                  digivisits.per.user = digi.contacts / digi.users,
                  digivisits.per.cap = digi.contacts / n.persons)]
  
  # Annualized digi.clinic contacts per capita:
  results[follow.up==9, digivisits.per.cap.ann := (12/9) * digivisits.per.cap]
  results[follow.up==10, digivisits.per.cap.ann := (12/10) * digivisits.per.cap]
  results[follow.up==11, digivisits.per.cap.ann := (12/11) * digivisits.per.cap]
  results[follow.up==12, digivisits.per.cap.ann := digivisits.per.cap]
  
  # Drop variables that are no longer needed:
  results[, ':=' (digi.users=NULL, digi.contacts=NULL)]
  
})

stats <- rbindlist(stats)

# Merge r2dw to stats and save:
stats <- merge(stats, r2dw, by='age.limit', all.x = TRUE)
fwrite(stats, file=output_stats)

# End.
