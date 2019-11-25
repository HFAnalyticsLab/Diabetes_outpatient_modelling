#========================================================================================================
# Project: Diabetes outpatient care - modelling follow-up patterns
# Purpose: Create outputs for use in microsimulation model of diabetic outpatient appointments
# Author: Meetali Kakad
#========================================================================================================
# Outputs created:
# Tables of aggregated counts of attendance type and appt intervals by week and diabetes type )
# Table of cumulative outpatient volumes by week for entire cohort
# Table of cumulative outpatient volumes by week for clinic cohort
# Various transition matrices representing likelihood of next appt interval given last appointment interval 
#=========================================================================================================
# Typically these tables are created based on a subset of our 3 year study cohort that we call our historical clinic cohort
# This historical clinic cohort represents the patients that had appointments during our simulation period
# The simulation period is from wk 1 yr 2 - wk 1 yr 3 of our study period 
#=========================================================================================================

library(lubridate)
library(tidyverse)
library(tidylog)
library(janitor)


# Source file paths
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')


# Function definitions ----------------------------------------------------

# Create a function to generate transition matrices by appointment type (existing, new) and diabetes type 
# Generates two tables per combination of appt type and diabetes type.
# First is the number of appointments in each cell
#is the proportion of the row total 
# i.e. proportion of all patients whose last interval appointment was x, who had their next appt interval at y
TMF <- function(data, firstatt_agg, diabetes_type){
  
  data %>% # creates a matrix for the proportion of all appts for a given last appt interval (row total) e.g. 1 month
    # that had their next appt at an interval of 1 week, 1 month etc
    group_by(appt_int_last_binned, appt_int_next_binned) %>%
    summarize(n=n()) %>%
    mutate(prop = round(n/sum(n), digits = 2)) %>%
    subset(select = c("appt_int_last_binned", "appt_int_next_binned", 'prop')) %>%
    spread(appt_int_next_binned, prop) %>%
    ungroup()
  
}

# Create a function to generate transition matrices by appointment type (existing, new) and diabetes type 
# Generates a table for each combination of appt type and diabetes type.
# Each cell in the table represents the count of all patients whose last interval appointment was at 'x' months or weeks
# who had their next appt interval at 'y' months or weeks
TMF_count <- function(data, firstatt_agg, diabetes_type){
  
  data %>% 
    group_by(appt_int_last_binned, appt_int_next_binned) %>%
    summarize(n=n()) %>%
    mutate(total = n) %>%
    subset(select = c("appt_int_last_binned", "appt_int_next_binned", 'total')) %>%
    spread(appt_int_next_binned, total) %>%
    ungroup()
  
}

# Import data -------------------------------------------------------------

# Read in outpatient data for our diabetic cohort
# This is all outpatient data for our CRPD cohort for all treatment specialities 
hesop_appts5<- readRDS(str_c(processed_RDS_path, 'hesop_appts5.Rds'))

# Number of type 1 and type 2 diabetics who had diabetic outpatient appointments
hesop_appts5 %>% 
  filter(diabetes_type %in% c('type1', 'type2') & tretspef == '307') %$%
  length(unique(patid))

# Read in outpatient data for our diabetic cohort
Patients_clinical <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# The time period for the simuation we will do will start from week 1 year two and last 1 year
# We use this period to define a cohort of patients whose historical data will inform our subsequent simulation model
simulation_start <- ymd('2016-12-01')
simulation_end <- ymd('2017-11-30')


# Filter for diabetic appointments after study start for type 1 and 2 diabetics who had diabetic medicine appointments
hesop_diab_study <- hesop_appts5 %>% 
  filter(apptdate > study_start & diabetes_type %in% c('type1', 'type2') & tretspef == '307') %>% 
  as.data.frame(hesop_diab_study) 


# Deriving variables ---------------------------------

# Create new variables for last appointment date and next appointment date  
# Create new variable last_appt_date
hesop_diab_study <- hesop_diab_study %>% 
  arrange(patid, apptdate) %>% 
  group_by(patid) %>% 
  mutate(last_appt_date = ifelse(firstatt_agg == 'New', NA_real_,
                                 ifelse(is.na(lag(apptdate)), NA_real_,
                                        lag(apptdate)))) %>%
  mutate(last_appt_date = as_date(last_appt_date)) %>% 
  ungroup()


# Create new variable next_appt_date
hesop_diab_study <- hesop_diab_study %>% 
  arrange(patid, apptdate) %>% 
  group_by(patid) %>% 
  mutate(next_appt_date = ifelse(outcome == 1, NA, lead(apptdate))) %>% 
  mutate(next_appt_date = as_date(next_appt_date)) %>%          
  ungroup()


# Calculate time interval since last appointment in weeks 
hesop_diab_study <- hesop_diab_study %>% 
  arrange(patid, apptdate) %>% 
  group_by(patid) %>% 
  mutate(last_appt_int_days = round((as.numeric(apptdate - last_appt_date, "days")), digits = 0)) %>% 
  mutate(last_appt_int_weeks = ceiling(as.numeric(apptdate - last_appt_date, "weeks")))%>% 
  ungroup()


# Calculate time interval since next appointment in weeks 
hesop_diab_study <- hesop_diab_study %>% 
  arrange(patid, apptdate) %>% 
  group_by(patid) %>% 
  mutate(next_appt_int_days = round((as.numeric(next_appt_date - apptdate, "days")), digits = 0)) %>% 
  mutate(next_appt_int_weeks = ceiling(as.numeric(next_appt_date - apptdate, "weeks")))%>% 
  ungroup()


# Add interval categories-------

# NA represents index appointment ie no interval since last appointment
# 1 week
# 2-6 weeks = 1 month
# 7-17 weeks = 3 months
# 18-30 weeks = 6 months
# 31-41 weeks = 9 months
# >= 42 weeks = 1 year

# Categorise interval since last appointment

# Constructing new referrals
# Borrowed this from Fiona Grimm
# Step 1: if appointment is coded as first appointment, it is a new referral

hesop_diab_study <- hesop_diab_study %>% 
  mutate(appt_int_last_binned = cut(last_appt_int_weeks, breaks = c(0, 1,  6, 17, 30, 41, Inf),
                                    labels = c('1 week','1 month', '3 months', '6 months', '9 months', '12 months'), 
                                    include.lowest = TRUE, right = TRUE))

appt_int_last_binned_order <- c('1 week', '1 month', '3 months', '6 months', '9 months', '12 months', 'New referral', 'Unknown')

hesop_diab_study <- hesop_diab_study %>%    
  mutate(appt_int_last_binned = fct_expand(appt_int_last_binned, 'New referral', 'Unknown'),
         appt_int_last_binned = if_else(firstatt_agg == 'New',
                                        factor('New referral', levels = levels(appt_int_last_binned)), 
                                        factor(appt_int_last_binned)),
         appt_int_last_binned = fct_explicit_na(appt_int_last_binned, 'Unknown'),
         appt_int_last_binned = fct_relevel(appt_int_last_binned, appt_int_last_binned_order))



# Constructing discharges
# Step 1: if next appointment is a first appointment, then it was a discharge
# Step 2: if there is no next appointment within the study window, then look at 'outcome' variable to
# determine whether it was a discharge


appt_int_next_binned_order <- c('1 week', '1 month', '3 months', '6 months', '9 months', '12 months', 'Discharged', 'Unknown')


hesop_diab_study <- hesop_diab_study %>%          
  mutate(appt_int_next_binned = cut(next_appt_int_weeks, breaks = c(0, 1,  6, 17, 30, 41, Inf),
                                    labels = c('1 week','1 month', '3 months', '6 months', '9 months', '12 months'), 
                                    include.lowest = TRUE, right = TRUE),
         appt_int_next_binned = fct_expand(appt_int_next_binned, 'Discharged', 'Unknown'),
         appt_int_next_binned = if_else(is.na(appt_int_next_binned) & outcome == 1, 
                                        factor('Discharged', levels = levels(appt_int_next_binned)), 
                                        factor(appt_int_next_binned)),
         appt_int_next_binned = fct_explicit_na(appt_int_next_binned, 'Unknown'),
         appt_int_next_binned = fct_relevel(appt_int_next_binned, appt_int_next_binned_order))


nrow(hesop_diab_study)
length(unique(hesop_diab_study$patid))

saveRDS(hesop_diab_study, 'processed_data/hesop_diab_study.rds')


# Create historical clinic cohort of appointments that were on the books for period of the simulation study
# These include existing (follow up) and new patient appointments
hesop_clinic_cohort <- hesop_diab_study %>%
  group_by(patid) %>%
  filter(!(outcome == 1  & apptdate < simulation_start)) %>%   # remove those patients discharged prior to simulation start
  # Only include appointments during the period defined for our simulation %>%
  filter(apptdate %within% interval(simulation_start, simulation_end)) %>%
  # Create a variable for the week of the simulation the appointment is scheduled in, Where the first week is 1
  mutate(sim_week = ceiling(as.numeric((apptdate - simulation_start)+1, "weeks"))) %>%
  ungroup() %>%
  as.data.frame()

nrow(hesop_clinic_cohort)
length(unique(hesop_clinic_cohort$patid))


# Save the dataset
saveRDS(hesop_clinic_cohort, str_c(processed_RDS_path, 'hesop_clinic_cohort.rds'))


# Aggregating data ------

# Count the weekly number of scheduled new patients by diabetes type  
table_new_pts_weekly <- hesop_clinic_cohort %>%
  filter(firstatt_agg == "New") %>% 
  tabyl(sim_week, diabetes_type, na.rm = FALSE) %>% 
  write_csv('outputs_for_simulation/tbl_new_weekly.csv')


# Create a flag for the first appointment that occurs in the simulation period for new and existing patients 
# NB! Consider moving this to the deriving variables code file
hesop_clinic_cohort <- hesop_clinic_cohort %>% 
  arrange(patid, apptdate) %>% 
  group_by(patid) %>%
  mutate(flag_first_appt_sim = if_else(row_number() == 1, 1,0),  # Create flag
         date_first_appt_sim = as_date(ifelse(flag_first_appt_sim == 1, apptdate, NA))) %>%   # Create variable for date of index appt in simulation period
   ungroup()

view(hesop_clinic_cohort %>% 
       select(patid, apptdate, date_first_appt_sim))


# Fill column for the first appt in the simulation period (index)
# Create a new variable that for each appointment 
# that calculates the number of days since the first (index) appointment 
hesop_clinic_cohort <- hesop_clinic_cohort %>% 
  group_by(patid) %>% 
  fill(date_first_appt_sim, .direction = 'up') %>% 
  fill(date_first_appt_sim, .direction = 'down') %>% 
  mutate(time_since_first_d1 = as.numeric((apptdate - date_first_appt_sim)+1, "days")) %>%  # Create variable interval in days since first appointment after simulation start 
  ungroup()


# Count number of flagged appointments for each week of the simulation period for existing patients
table_exist_index_appt_weekly <- hesop_clinic_cohort %>%
  filter(flag_first_appt_sim == 1 &
           firstatt_agg == "Follow up") %>% # Filter for existing patients
  tabyl(sim_week, diabetes_type, na.rm = FALSE) %>%    # FIXME need to round up to 10 where numbers are less than 10
  write_csv(path = 'outputs_for_simulation/tbl_exist_index_weekly.csv' )


view(hesop_clinic_cohort %>% filter(sim_week == 53) %>%
     select(patid, apptdate, firstatt_agg, flag_first_appt_sim, date_first_appt_sim) %>%
       filter(flag_first_appt_sim == 1 & firstatt_agg == "Follow up"))


# For existing (follow up) appts
# Tabulate proportion of appts in each time to last appointment interval
# Do this using two datasets: 1 year historical clinic cohort and the 3 year study cohort
tbl_exist_last_int_cl <- hesop_clinic_cohort %>% 
  filter(firstatt_agg == "Follow up") %>% 
  tabyl(diabetes_type,appt_int_last_binned) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() %>% 
  write_csv(path = 'outputs_for_simulation/tbl_exist_last_int_cl.csv')

tbl_exist_last_int_st <- hesop_diab_study %>% 
  filter(firstatt_agg == "Follow up") %>% 
  tabyl(diabetes_type,appt_int_last_binned) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_exist_last_int_st.csv')


# Create a table of counts of attendance type categorised by time since last appointment.
# Do this for existing patients
# Do this using two datasets: 1 year historical clinic cohort and the 3 year study cohort
tbl_sum_exist_attend_last_cl <- hesop_clinic_cohort %>% 
  filter(firstatt_agg == "Follow up") %>% 
  group_by(attended_agg, appt_int_last_binned) %>% 
  summarize(n=n()) %>% 
  mutate(total = n) %>% 
  subset(select = c("appt_int_last_binned", "attended_agg", 'total')) %>% 
  spread(appt_int_last_binned, total) %>% 
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_sum_exist_attend_last_cl.csv') 


tbl_sum_exist_attend_last_st <- hesop_diab_study %>% 
  filter(firstatt_agg == "Follow up") %>% 
  group_by(appt_int_last_binned, attended_agg) %>% 
  summarize(n=n()) %>% 
  mutate(total = n) %>%
  subset(select = c("appt_int_last_binned", "attended_agg", 'total')) %>% 
  spread(appt_int_last_binned, total) %>% 
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_sum_exist_attend_last_st.csv') 


# Do the same but instead of the count calculate the proportions of appts
tbl_prop_exist_attend_last_cl <- hesop_clinic_cohort %>% 
  filter(firstatt_agg == "Follow up") %>% 
  group_by(appt_int_last_binned, attended_agg) %>% 
  summarize(n=n()) %>% 
  mutate(prop = round(n/sum(n), digits = 2)) %>% 
  subset(select = c("appt_int_last_binned", "attended_agg", 'prop')) %>% 
  spread(attended_agg, prop) %>% 
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_prop_exist_attend_last_cl.csv') 


tbl_prop_exist_attend_last_st <- hesop_diab_study %>% 
  filter(firstatt_agg == "Follow up") %>% 
  group_by(appt_int_last_binned, attended_agg) %>% 
  summarize(n=n()) %>% 
  mutate(prop = round(n/sum(n), digits = 2)) %>% 
  subset(select = c("appt_int_last_binned", "attended_agg", 'prop')) %>% 
  spread(attended_agg, prop) %>% 
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_prop_exist_attend_last_st.csv') 


# For new patients 
# calculate the proportion of new patients who attended, cancelled or DNA'd their new patient appointment
# By diabetes type
# Do this for the historical cohort and the study cohort

tbl_prop_new_attend_cl <- hesop_clinic_cohort %>% 
  filter(firstatt_agg == "New") %>% 
  tabyl(diabetes_type,attended_agg) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() %>% 
  write_csv(path = 'outputs_for_simulation/tbl_prop_new_attend_cl.csv') 


tbl_prop_new_attend_st <- hesop_diab_study %>% 
  filter(firstatt_agg == "New") %>% 
  tabyl(diabetes_type,attended_agg) %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() %>% 
  write_csv(path = 'outputs_for_simulation/tbl_prop_new_attend_st.csv') 


# For new patients 
# Calculate count of patients who attended their new patient appointment
# in each time to next appointment category (1 month/1 week etc)
# Do this for the historical cohort and the study cohort
tbl_sum_new_attend_next_cl <- hesop_clinic_cohort %>% # historical clinic cohort
  filter(firstatt_agg == "New" & attended_agg == "Attended") %>% 
  group_by(appt_int_next_binned) %>% 
  summarize(n=n()) %>% 
  mutate(total = n) %>% 
  subset(select = c("appt_int_next_binned", 'total')) %>% 
  spread(appt_int_next_binned, total) %>% 
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_sum_new_attend_next_cl.csv') 

  
tbl_sum_new_attend_next_st <- hesop_diab_study %>% # study cohort
  filter(firstatt_agg == "New" & attended_agg == "Attended") %>% 
  group_by(appt_int_next_binned) %>% 
  summarize(n=n()) %>% 
  mutate(total = n) %>% 
  subset(select = c("appt_int_next_binned", 'total')) %>% 
  spread(appt_int_next_binned, total) %>% 
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_sum_new_attend_next_st.csv') 


# For patients who attended their new patient appointment
# What is the proportion of appts occuring in each time interval to next appointment (1 month/ 1 week etc)?
# Do this for the historical clinic cohort and study cohort
tbl_prop_new_attend_next_cl <- hesop_clinic_cohort %>% # historical clinic cohort
  filter(firstatt_agg == "New" & attended_agg == "Attended") %>% 
  group_by(diabetes_type, appt_int_next_binned) %>% 
  summarize(n=n()) %>% 
  mutate(prop = round(n/sum(n), digits = 2)) %>%
  subset(select = c('diabetes_type', 'appt_int_next_binned', 'prop')) %>%
  spread(appt_int_next_binned,prop) %>%
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_prop_new_attend_next_cl.csv') 


tbl_prop_new_attend_next_st <- hesop_diab_study %>% # study cohort
  filter(firstatt_agg == "New" & attended_agg == "Attended") %>% 
  group_by(diabetes_type, appt_int_next_binned) %>% 
  summarize(n=n()) %>% 
  mutate(prop = round(n/sum(n), digits = 2)) %>%
  subset(select = c('diabetes_type', 'appt_int_next_binned', 'prop')) %>%
  spread(appt_int_next_binned,prop) %>%
  ungroup()%>% 
  write_csv(path = 'outputs_for_simulation/tbl_prop_new_attend_next_st.csv') 


# Generating transition matrices ------------------------                                                                                                  

# Create a nested dataframe 
# Borrowed some code from Tom Jemmett at the Strategy unit
data_TM <- hesop_clinic_cohort %>% 
  group_by(firstatt_agg) %>% 
  group_by(diabetes_type, add = TRUE) %>% # add the diabetes_type column to the grouping
  group_nest() # nest the dataframe by the groupings, the other columns become a nested data frame in the "data column"

data_TM



# Use pmap function from purr with our nested dataframe to generate a transition matrix for each of the rows in the data frame
TMatrices <- data_TM %>% 
  pmap(TMF)

# Generate more useful names for these matrices
TMatrices <- TMatrices %>% 
  set_names(map2_chr(data_TM$firstatt_agg, data_TM$diabetes_type, ~paste0(.x, "_", .y)))

# Save files # FIXME should do this via a function
TM_new_t1 <- TMatrices[[1]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_new_t1.csv' )

TM_new_t2 <- TMatrices[[2]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_new_t2.csv' )

TM_exist_t2 <- TMatrices[[3]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_exist_t1.csv' )

TM_exist_t2 <- TMatrices[[4]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_exist_t2.csv') 




# Use pmap function from purr with our nested dataframe to generate a transition matrix for each of the rows in the data frame
TMatrices_count <- data_TM %>% 
  pmap(TMF_count)


# Generate more useful names for these matrices
TMatrices_count <- TMatrices_count %>% 
  set_names(map2_chr(data_TM$firstatt_agg, data_TM$diabetes_type, ~paste0(.x, "_", .y)))


# Save files # FIXME should do this via a function
TM_count_new_t1 <- TMatrices_count[[1]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_count_new_t1.csv' )

TM_count_new_t2 <- TMatrices_count[[2]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_count_new_t2.csv' )

TM_count_exist_t1 <- TMatrices_count[[3]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_count_exist_t1.csv' )

TM_count_exist_t2 <- TMatrices_count[[4]] %>% 
  write_csv(path = 'outputs_for_simulation/TM_count_exist_t2.csv' )


# Create individual trajectories -----------

# We create individual timelines of appointment for our historical clinic cohort
# I didn't end up using this for the microsimulation! 
# I have changed the code to plot the trajectory during the simulation period

# NB:
# In previous versions of the code, I indexed the first appointment in the study period 
# I used this to create a timeline of appointments from this index time point onwards  
# Here is an example:
# hesop_clinic_cohort_sp <- hesop_clinic_cohort %>% 
#   select(patid, diabetes_type, 
#          #attended, firstatt
#          time_since_first_d1, dummy) %>% This variable (time_since_first_d1) is substituted for sim_week. 
# group_by(patid, diabetes_type, time_since_firstd1) %>%
# summarise(app_count = n()) %>% # collapse rows where patients apparently have more than one appointment per day
#   spread(key = time_since_first_d1, value = dummy)


# create dummy variable to allow us to spread the data
hesop_clinic_cohort <- hesop_clinic_cohort %>% 
  mutate(dummy = 1)


# Spread the data
hesop_clinic_cohort_sp <- hesop_clinic_cohort %>% 
  select(patid, diabetes_type, 
         sim_week, dummy) %>% 
  group_by(patid, diabetes_type, sim_week) %>% 
  summarise(app_count = n()) %>% # collapse rows where patients apparently have more than one appointment per day
  spread(key = sim_week, value = app_count)


# replace NAs with 0 in the data set (necessary for clustering)
hesop_clinic_appt_timeline <- hesop_clinic_cohort_sp %>% 
  replace(., is.na(.), 0)


# save a file of individual appointment timelines
saveRDS(hesop_clinic_appt_timeline, str_c(processed_RDS_path, 'hesop_clinic_appt_timeline.Rds'))


# Update with cumulative attendance values 
# To create a cumulative appointment timeline for each patient by week in simulation period
for(i in 3:ncol(hesop_clinic_appt_timeline)){  # start at column 4
  if(i == 3){
    hesop_clinic_appt_timeline[, i] <- hesop_clinic_appt_timeline[, i]
  }else{
    hesop_clinic_appt_timeline[, i] <- (hesop_clinic_appt_timeline[,(i-1)] + hesop_clinic_appt_timeline[, i])
  }
}

cum_attendance_weeks <- hesop_clinic_appt_timeline 


# Save processed files 
saveRDS(cum_attendance_weeks, str_c(processed_RDS_path, 'cum_attendance_weeks.Rds'))


# Gather data 
# long data easier to use with ggplot2
trajectory_wk_gather <- cum_attendance_weeks %>% 
  gather(sim_week, cumulative_appt_count,  
         c("1", "2", "3", "4", "5", "6", "7", 
           "8", "9", "10", "11", "12", "13", "14",
           "15", "16", "17", "18", "19", "20", "21",
           "22", "23", "24", "25", "26", "27", "28",
           "29", "30", "31", "32", "33", "34", "35",
           "36", "37", "38", "39", "40", "41", "42",
           "43", "44", "45", "46", "47", "48", "49",
           "50", "51", "52", "53")) %>%  # gather the data to make it long rather than wide
  mutate(sim_week = as.integer(sim_week)) %>% 
  arrange(sim_week)  

saveRDS(trajectory_wk_gather, str_c(processed_RDS_path, 'trajectory_wk_gather.Rds'))


# Create a table of cumulative number appointments by week by diabetes type
tbl_cum_appt_week <- trajectory_wk_gather %>%  
  group_by(sim_week, diabetes_type) %>% 
  summarize(cum_wk_appt = sum(cumulative_appt_count)) %>% 
  arrange(sim_week) %>% 
  write_csv(path = 'outputs_for_simulation/tbl_cum_appt_week.csv')


# Create a table of the number of new and follow up appointments by week, by diabetes type
# For clinic cohort
tbl_count_appt_week_diab_appttype <- hesop_clinic_cohort %>% 
  group_by(sim_week, diabetes_type, firstatt_agg) %>% 
  summarise(cum_wk_appt = n()) %>% 
  write_csv(path = 'outputs_for_simulation/tbl_count_appt_week_diab_appttype.csv')


# Calculate the mean number of weeks between cancelled appointment and next scheduled appt
# For historical clinic cohort

hesop_clinic_cohort <- hesop_clinic_cohort %>% 
  group_by(patid, diabetes_type) %>% 
  mutate(cancel_DNA_next_apptdate = as_date(ifelse(attended_agg %in% c("Cancelled", "Did Not Attend"),
                                       lead(apptdate),
                                       NA)),
         cancel_DNA_interval = ifelse(attended_agg %in% c("Cancelled", "Did Not Attend"),
                                   ceiling((as.numeric(lead(apptdate) - apptdate, "weeks"))),
                                   NA_real_)) %>% 
  ungroup() 

view(hesop_clinic_cohort %>% 
       filter(attended_agg %in% c("Cancelled", "Did Not Attend")) %>% 
       select(patid, apptdate, attended_agg, outcome, cancel_DNA_next_apptdate, cancel_DNA_interval))


# Calculate the mean interval time to next appointment in weeks for DNAs and cancellations
tbl_mean_cancel_int_cl <- hesop_clinic_cohort %>% 
  filter(attended_agg %in% c("Cancelled", "Did Not Attend")) %>% 
  group_by(diabetes_type, attended_agg, firstatt_agg) %>% 
  summarise(mean = mean(cancel_DNA_interval, na.rm = TRUE),
            n = n()) %>% 
  write_csv(path = 'outputs_for_simulation/tbl_mean_cancel_int_cl.csv')
         

# For study cohort dataset
# This is for three years worth of data

hesop_diab_study <- hesop_diab_study %>% 
  group_by(patid, diabetes_type) %>% 
  mutate(cancel_DNA_next_apptdate = as_date(ifelse(attended_agg %in% c("Cancelled", "Did Not Attend"),
                                                   lead(apptdate),
                                                   NA)),
         cancel_DNA_interval = ifelse(attended_agg %in% c("Cancelled", "Did Not Attend"),
                                      ceiling((as.numeric(lead(apptdate) - apptdate, "weeks"))),
                                      NA_real_)) %>% 
  ungroup() 

view(hesop_diab_study %>% 
       filter(attended_agg %in% c("Cancelled", "Did Not Attend")) %>% 
       select(patid, apptdate, attended_agg, outcome, cancel_DNA_next_apptdate, cancel_DNA_interval))


# Calculate the mean interval time to next appointment in weeks for in weeks for DNAs and cancellations

tbl_mean_cancel_int_st <- hesop_diab_study %>% 
  filter(attended_agg %in% c("Cancelled", "Did Not Attend")) %>% 
  group_by(diabetes_type, attended_agg, firstatt_agg) %>% 
  summarise(mean = mean(cancel_DNA_interval, na.rm = TRUE),
            n = n()) %>% 
  write_csv(path = 'outputs_for_simulation/tbl_mean_cancel_int_st.csv')

