
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script master_script_pap.R          ###
###                 Replication file.                 ###
###                    2025 by TH                     ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: The order in which R scripts should be run (simulations and power).
rm(list=ls())

# R version 4.2.1.
# Install and load the following packages:
library(here)		          # relative file paths.
library(data.table)       # mutating and aggregating data
library(fixest)           # OLS regression.
library(powerLATE)        # Generalized Power Analysis for LATE 
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.
library(stargazer)        # Save as tex file.
library(pwr)              # Basic function for power analysis (ITT)

writeLines(capture.output(sessionInfo()), 'sessionInfo_pap.txt')

###
###


# Data extraction and cleaning:
#source(file = "~/Desktop/work/DATAINFRA/master_script_datainfra.R")

# Construct statistics on digital clinic use in East Uusimaa:
source(file = here('scripts', '1_digital_clinic_use.R'))
# Running time: 2 mins

# Construct analysis data for Monte Carlo simulations:
source(file = here('scripts', '2_construct_analysis_data.R'))
# Running time: 3 mins 

# Construct analysis data for Monte Carlo simulations (East Uusimaa):
source(file = here('scripts', '3_construct_data_east_uusimaa.R'))
# Running time: 4 mins

# LATE and scaled ITT power analysis:
source(file = here('scripts', '4_power_analysis.R'))
# Running time: 5 mins

# Conduct the Monte Carlo simulations:
source(file = here('scripts', '5_simulations.R'))
# Running time: 2 hours 

# Tidy and save result tables:
source(file = here('scripts', '6_tidy_result_tables.R'))
# Running time: <1 min

# End.
