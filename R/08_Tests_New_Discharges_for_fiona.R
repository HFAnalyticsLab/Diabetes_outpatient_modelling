#========================================================================================================
# Our model appears to overestimate volume of diabetic outpatient appointments scheduled per year
# There is a question that the proportion of new patients appointment vs discharges is incorrect
# We wish to triangulate our model results with analyses of # new patients and # discharges from different cuts of the data
# =========================================================================================================
# Code to check the numbers of new patients and number of patients discharged every week.
# ==========================================================================================================
# 1) We start using the hesop_appts data set for our CPRD study patients for all years, filtering for diabetes type 1&2 and diabetes medicine (DM) appointments
# 2) We repeat filtering this data further to only include DM appointments scheduled during our study period from 1/12/2015- 26/11/2016
# 3) We repeat using data from 2) using this data having applied our algorithm for backfilling outcome and appointment type (new/follow up) data 
#    Algorithm checks if outcome or firstatt values are missing and attempts to impute
#    or where there is a long period between previous appointment and current or last appointment and current
# 4) Repeat for the clinic_cohort dataset I created whilst in London which should the similar results to 3)
# The results are stored in separate tables for release from SDE (assuming cells have more than 10 events)
# ==========================================================================================================
# Author: Meetali Kakad
# Date: 20200520
# ==========================================================================================================
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(tidylog)

# # Source file paths
source('../../diabetes_outpatients_git/R/file_paths.R')

# Source study parameters
source('../../diabetes_outpatients_git/R/study_params.R')

# Input parameters to check 
simulation_start <- ymd('2015-12-01')
simulation_end <- ymd('2016-11-26')


# Import data -------------------------------------------------------------

# # dummy data in Oslo
# hesop_appts <-  read_excel('Tables/old/tbl_dummydata_hesop_appts2.xlsx') %>% 
#   mutate(apptdate = ymd(apptdate))

# Cleaned outpatient data for our CPRD cohort all specialities from 2003-2018
# Count number of discharges and number of new patients based on firstatt variable and outcome variable only

# Import data
hesop_appts <- readRDS(str_c(processed_RDS_path, 'hesop_appts.Rds'))

hesop_appts <- hesop_appts %>% 
  group_by(patid, tretspef) %>% 
  arrange(apptdate) %>% 
  filter(diabetes_type %in% c('type1', 'type2') & tretspef == '307') %>% 
  mutate(year = year(apptdate),
         week = week(apptdate), 
         firstatt_agg =  case_when(firstatt %in% c(1,3) ~ 1, 
                                   firstatt %in% c(2,4) ~ 2, 
                                   TRUE ~ NA_real_)) %>% 
  ungroup()

# Summary stats for diabetes medicine patients (tretspef  = 307) and type 1 and type 2 diabetes 
# hesop_appts_DM <- hesop_appts %>% 
#   filter(diabetes_type %in% c('type1', 'type2') & tretspef == '307') %>% 
#   mutate(year = year(apptdate),
#          month = month (apptdate))


# Only start at 2010 
hesop_appts %>% 
  filter(apptdate >= ymd('2010-01-01')) %>% 
  group_by(year, week, diabetes_type, firstatt_agg) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, fill = list(n = 0)) %>% 
  write_csv(path = str_c(simulation_outputs_path, 'hesop_appts_weekly_firstatt.csv'))

hesop_appts %>% 
  filter(apptdate >= ymd('2010-01-01')) %>% 
  group_by(year, week, diabetes_type, outcome) %>% 
  tally() %>%   
  ungroup %>% 
  complete(year, week, diabetes_type, outcome, fill = list(n = 0)) %>% 
  write_csv(path = str_c(simulation_outputs_path, 'hesop_appts_weekly_outcome.csv'))

# Ideally this if the numbers aren't too small
hesop_appts %>% 
  filter(apptdate >= ymd('2010-01-01')) %>% 
  group_by(year, week, diabetes_type, firstatt_agg, outcome) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, outcome, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'hesop_appts_weekly_firstatt_outcome.csv'))


# Do the same for our the cohort we wish to study
# All diabetes medicine appointments with appointment date scheduled between '01.12.2015' and '28-11-2016'
# this is just a subset of the first table.

hesop_appts %>% 
  filter(apptdate %within% interval(simulation_start, simulation_end)) %>% 
  group_by(year, week, diabetes_type, firstatt_agg) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'study_period_weekly_firstatt.csv'))

hesop_appts %>% 
  filter(apptdate %within% interval(simulation_start, simulation_end)) %>% 
  group_by(year, week, diabetes_type, outcome) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, outcome, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'study_period_weekly_outcome.csv'))

# Ideally this if the numbers aren't too small
hesop_appts %>% 
  filter(apptdate %within% interval(simulation_start, simulation_end)) %>% 
  group_by(year, week, diabetes_type, firstatt_agg, outcome) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, outcome, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'study_period_weekly_firstatt_outcome.csv'))


# Apply classification algorithm to backfill where we lack new/follow up or outcome data for an appointment
# Then tally up new patients and discharges

test <- hesop_appts %>% 
  filter(apptdate %within% interval(simulation_start, simulation_end)) %>%  # filter to only include appts scheduled during study period
  group_by(patid, tretspef) %>% 
  arrange(apptdate) %>% 
  mutate(lead_firstatt = lead(firstatt),  # create variable for type and modality of next diabetes appt
         last_appt_date = ifelse(firstatt_agg == 1, NA,lag(apptdate)), # create variable for date of previous appt date, is NA if appt type is 'new'
         last_appt_date = as_date(last_appt_date), 
         next_appt_date = ifelse(outcome == 1, NA, lead(apptdate)), # if outcome for appt is discharge put next appointment is NA
         next_appt_date = as_date(next_appt_date),
         last_appt_int_weeks = ceiling(as.numeric(apptdate - last_appt_date, "weeks")),  # create interval to last appointment in wks
         next_appt_int_weeks = ceiling(as.numeric(next_appt_date - apptdate, "weeks")), # # create interval to next appointment in wks
         outcome2 = outcome,
         outcome2 = ifelse(is.na(outcome) & lead_firstatt %in% c(1,3), 1, outcome), # update outcome2 field with data if next appointment is new
         outcome2 = ifelse(is.na(next_appt_int_weeks), outcome, 
                           ifelse(next_appt_int_weeks> 104, 1, outcome)),  # update outcome2 field  as discharged if interval to next appt is > 104
         firstatt_agg = ifelse(last_appt_int_weeks > 104 | is.na(lag(apptdate)), 1, firstatt_agg)) %>%  # update appt type as new, if interval to last appt > 104 weeks or NA
  ungroup()


test %>% 
  group_by(year, week, diabetes_type, firstatt_agg) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'algorithm_test_weekly_firstatt.csv'))

test %>% 
  group_by(year, week, diabetes_type, outcome2) %>% 
  tally()%>% 
  ungroup %>% 
  complete(year, week, diabetes_type, outcome2, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'algorithm_test_weekly_outcome.csv'))


# Ideally this if the numbers aren't too small
test %>% 
  group_by(year, week, diabetes_type, firstatt_agg, outcome2) %>% 
  tally()%>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, outcome2, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'algorithm_test_weekly_firstatt_outcome.csv'))



# Repeat with the clinic cohort dataset that I have created as a check------

# Import clinic cohort file
hesop_clinic_cohort <- readRDS(str_c(processed_RDS_path, 'hesop_clinic_cohort.Rds'))


hesop_clinic_cohort <- hesop_clinic_cohort %>% 
  filter(diabetes_type %in% c('type1', 'type2') & tretspef == '307') %>% # diabetes medicine appointments for type 1 and 2 diabetics
  filter(apptdate %within% interval(simulation_start, simulation_end)) %>% 
  mutate(year = year(apptdate),
         week = week(apptdate)) 
  
hesop_clinic_cohort %>% 
  group_by(year, week, diabetes_type, firstatt_agg) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'clinic_cohort_weekly_firstatt.csv'))

hesop_clinic_cohort %>% 
  group_by(year, week, diabetes_type, outcome) %>% 
  tally()%>% 
  ungroup %>% 
  complete(year, week, diabetes_type, outcome, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'clinic_cohort_weekly_outcome.csv'))


# Ideally this if the numbers aren't too small
hesop_clinic_cohort %>% 
  group_by(year, week, diabetes_type, firstatt_agg, outcome) %>% 
  tally() %>% 
  ungroup %>% 
  complete(year, week, diabetes_type, firstatt_agg, outcome, fill = list(n = 0)) %>%
  write_csv(path = str_c(simulation_outputs_path, 'clinic_cohort_weekly_firstatt_outcome.csv'))

