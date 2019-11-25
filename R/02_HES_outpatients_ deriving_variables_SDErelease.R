# =======================================================
# Project: Diabetes outpatient care - modelling follow-up patterns
# Purpose: Deriving variables for outpatient model, Creating outcome variables where NA 
# Author: Meetali Kakad
# =======================================================

library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(tidylog)


# Source file paths
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Import data -------------------------------------------------------------

# outpatient data for our diabetic cohort
hesop_appts <- readRDS(str_c(processed_RDS_path, 'hesop_appts.Rds'))

# Define simulation parameters -------------------------------------------------

# The time period for the simuation will start from week 1 year two and last 1 year
# We use this period to define a cohort of patients whose historical data will inform our subsequent simulation model
simulation_start <- ymd('2016-12-01')
simulation_end <- ymd('2017-11-30')


# Deriving variables ------------------------------------------------------


# Create new variable last_appt_date
hesop_appts2 <- hesop_appts %>% 
  arrange(patid, tretspef, apptdate) %>% 
  group_by(patid, tretspef) %>% 
  mutate(last_appt_date = ifelse(firstatt_agg == 'New', NA_real_,
                                 ifelse(is.na(lag(apptdate)), NA_real_,
                                        lag(apptdate)))) %>%
  mutate(last_appt_date = as_date(last_appt_date)) %>% 
  ungroup()


# Create new variable next_appt_date
hesop_appts2 <- hesop_appts2 %>% 
  arrange(patid, tretspef, apptdate) %>% 
  group_by(patid, tretspef) %>% 
  mutate(next_appt_date = ifelse(outcome == 1, NA, lead(apptdate))) %>% 
  mutate(next_appt_date = as_date(next_appt_date)) %>%          
  ungroup()


# Calculate time interval since last appointment in weeks 
hesop_appts2 <- hesop_appts2 %>% 
  arrange(patid, tretspef, apptdate) %>% 
  group_by(patid, tretspef) %>% 
  mutate(last_appt_int_days = round((as.numeric(apptdate - last_appt_date, "days")), digits = 0)) %>% 
  mutate(last_appt_int_weeks = ceiling(as.numeric(apptdate - last_appt_date, "weeks")))%>% 
  ungroup()


# Calculate time interval since next appointment in weeks 
hesop_appts2 <- hesop_appts2 %>% 
  arrange(patid, tretspef, apptdate) %>% 
  group_by(patid, tretspef) %>% 
  mutate(next_appt_int_days = round((as.numeric(next_appt_date - apptdate, "days")), digits = 0)) %>% 
  mutate(next_appt_int_weeks = ceiling(as.numeric(next_appt_date - apptdate, "weeks")))%>% 
  ungroup()


# Create variables for index appointments 
# haven't used this

# Create a flag for first appointment taking place by treatment speciality after start date  study period 2015-12-01
index_study <- hesop_appts2 %>%
  filter(apptdate >= study_start) %>% 
  arrange(patid, tretspef, apptdate) %>% 
  group_by(patid, tretspef) %>%
  mutate(flag_index_study = if_else(row_number() == 1, 1,0),
         index_appt_study = as_date(ifelse(flag_index_study == 1, apptdate, NA))) %>% 
  filter(flag_index_study == 1) %>% # do we need to filter here
  ungroup() 


# Join flag and index date for appointment following study start
hesop_appts3<- hesop_appts2 %>%  
  left_join(select(index_study, 
                   'patid', 'attendkey', 'apptdate', 'tretspef', 'flag_index_study', 'index_appt_study'), 
                   by = c('patid', 'attendkey', 'apptdate', 'tretspef'))
            
saveRDS(hesop_appts3, str_c(processed_RDS_path, 'hesop_appts3.Rds'))



hesop_appts4<- hesop_appts3%>% 
  group_by(patid, tretspef) %>% 
  fill(index_appt_study, .direction = 'up') %>% 
  fill(index_appt_study, .direction = 'down') %>% 
  ungroup()

saveRDS(hesop_appts4, str_c(processed_RDS_path, 'hesop_appts4.Rds'))
# NB this file will be overwritten further down

# Create variable lead_firstatt for type of next appointment (new or follow up),
hesop_appts4 <- hesop_appts4 %>% 
mutate(lead_firstatt = lead(firstatt),
       discharged = case_when(outcome == 1 ~ 1, # Create variable discharged derived from outcome variable
                              outcome %in% c(2,3) ~ 0,
                              is.na(outcome) ~ NA_real_))


# Group hesop_appts3 by patient id and treatment speciality and arrange by appointment date
hesop_appts4 <- hesop_appts4 %>% 
  group_by(patid,tretspef) %>% 
  arrange(apptdate) %>% 
  mutate(lead_firstatt = lead(firstatt),# Create variable lead_firstatt for type of next appointment (new or follow up),
         discharged = case_when(outcome == 1 ~ 1, # Create variable discharged derived from outcome variable
                                outcome %in% c(2,3) ~ 0,
                                is.na(outcome) ~ NA_real_))
# Crosstabs
hesop_appts4 %>% 
  tabyl(discharged)

table_pre <- hesop_appts4 %>%  
  tabyl(discharged, lead_firstatt) %>% 
  adorn_title()


# Create discharged variable based on outcome variable and lead_firstatt where outcome variable is NA
hesop_appts4 <- hesop_appts4 %>% 
  arrange(patid, tretspef, apptdate) %>% 
  group_by(patid, tretspef) %>% 
  mutate(discharged = ifelse(is.na(outcome) & lead_firstatt %in% c(1,3), 1, discharged),
         discharged = ifelse(is.na(outcome) & lead_firstatt %in% c(2,4), 0, discharged)) %>% 
  ungroup()

## Crosstabs to assess whether number of NAs reduced
hesop_appts4%>% 
  tabyl(discharged)


table_post <- hesop_appts4 %>%  
  tabyl(discharged, lead_firstatt) %>% 
  adorn_title()


# Create new aggregate variable firstatt_agg 
# to group appointment types into new or follow up
hesop_appts4 <- hesop_appts4 %>% 
  mutate(firstatt_agg = ifelse(firstatt %in% c(1,3), 1, 
                               ifelse(firstatt %in% c(2,4), 2, NA_real_)))


# Label factors
# 1 = new appointment, 2= Follow up
hesop_appts4$firstatt_agg <- factor(hesop_appts4$firstatt_agg,
                                    levels = c(1, 2),
                                    labels = c("New", "Follow up"))



# Create new aggregate variable for attended 
# Group into Seen, DNA and Cancelled
hesop_appts4<- hesop_appts4 %>% 
  mutate(attended_agg = ifelse(attended %in% c(5, 6), 1,
                               ifelse(attended %in% c(2, 4), 2, 3)))

# Label factors
hesop_appts4$attended_agg <- factor(hesop_appts4$attended_agg,
                                    levels = c(1, 2, 3),
                                    labels = c("Attended", "Cancelled", "Did Not Attend"))


# Saving processed files --------------------------------------------------
saveRDS(hesop_appts4, str_c(processed_RDS_path, 'hesop_appts4.Rds'))
###check if this is hesop 5 or 4 # changed back to 4.rds on 02.11.19


##NAs

map(hesop_appts4, ~ sum(is.na(.)))
map(hesop_appts_raw, ~ sum(is.na(.)))

