# =======================================================
# Project: Diabetes outpatient care - modelling follow-up patterns
# Purpose: Ouptatient appointments - cleaning and processing 
# Author: Fiona Grimm
# Date: 10/09/2019
# Amended by: Meetali Kakad - moved descriptives code to another file
# Date: 06/11/2019
# =======================================================


library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

# Source file paths
source('R/file_paths.R')


# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients.Rds'))

# HES outpatient appointments # Changed the name to raw
hesop_appts_raw <- readRDS(str_c(raw_RDS_path, 'HES_outpatients.Rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))

# Lookup table for treatment specialty names
tretspef_lookup <- read_csv('../data_dictionaries_HES/lookup_tables/tretspef_lookup.csv')


# Cleaning ----------------------------------------------------------------

## Recode missing values as NA

hesop_appts <- hesop_appts_raw %>% 
  mutate(admincat = na_if(admincat, 99),
         apptage = ifelse(apptage %in% c('7001', '7002', '7003', '7004', '7005', '7006', '7007'), 0, apptage),
         atentype = na_if(atentype, 13),
         attended = na_if(attended, 9),
         firstatt = ifelse(firstatt %in% c('X', '9'), NA, firstatt),
         firstatt = as.integer(firstatt),
         outcome = na_if(outcome, 9),
         priority = na_if(priority, 9),
         refsourc = na_if(refsourc, 99),
         servtype = na_if(servtype, 9),
         stafftyp = ifelse(stafftyp %in% c(9, 99), NA, stafftyp)) 

# Only keep appts for study pop
hesop_appts <- hesop_appts %>% 
  semi_join(patients, by = 'patid') %>% # patients file excluded those without DM read code, IMD status, died before study
  left_join(tretspef_lookup,  by = 'tretspef')

# Remove duplicates
hesop_appts <- hesop_appts %>% 
  distinct(patid,             admincat,          apptdate,         atentype,         
           attended,          dnadate,           firstatt,         outcome,           
           priority,          refsourc,          reqdate,          servtype,          
           stafftyp,          wait_ind,          waiting,          HES_yr,            
           tretspef,          mainspef, .keep_all = TRUE)

# Exclude appointment without tretspef, firstatt, attended
hesop_appts<- hesop_appts %>% 
  filter(!is.na(tretspef) & !is.na(firstatt) & !is.na(attended))


# Drop columns we don't need
hesop_appts<- hesop_appts %>% 
  select(-apptage, -atentype)

# Appointments after date of death
hesop_appts<- hesop_appts %>% 
  left_join(patients[, c('patid', 'ONS_dod')], by = 'patid') %>% 
  filter(apptdate < ONS_dod | is.na(ONS_dod))

##NEW

# Add diabetes_type
hesop_appts <- hesop_appts %>% 
  left_join(diabetes_bypat, by ='patid')


# Drop columns we don't need
hesop_appts<- hesop_appts %>% 
  select( -"other_count", -"type1_count", -"type2_count",      
          -"unspecified_count", -"other_codes", -"type1_codes", -"type2_codes",      
          -"unspecified_codes", -"mixed_type_flag")


# Saving processed files --------------------------------------------------
saveRDS(hesop_appts, str_c(processed_RDS_path, 'hesop_appts.Rds'))

