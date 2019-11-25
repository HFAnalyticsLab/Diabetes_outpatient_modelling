# =======================================================
# Project: Diabetes outpatient care - modelling follow-up patterns
# Purpose: Descriptives of HES outpatient data
# Author: Meetali Kakad
# =======================================================

library(tidyverse)
library(janitor)
library(lubridate)
library(tableone)

# Source file paths
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')


# Import data -------------------------------------------------------------

# NB If you don't run the code 01 and 02 HES Outpatients first the file below lacks the discharged variable constructed in 02

hesop_appts4 <- readRDS(str_c(processed_RDS_path, 'hesop_appts4.Rds'))

# Lookup table for treatment specialty names
tretspef_lookup <- read_csv('../data_dictionaries_HES/lookup_tables/tretspef_lookup.csv')

hesop_appts5 <- left_join(hesop_appts4, tretspef_lookup, by = 'tretspef')
length(unique(hesop_appts5$patid ))

# Summarizing data -----

# Count of diabetic medicine appointments per patient from study start
# tretspef == 307 (diabetic medicine)
hesop_appts5 %>% 
  filter(tretspef == '307' & apptdate >= study_start & diabetes_type %in% c('type1', 'type2')) %>% 
  tabyl(diabetes_type, firstatt_agg)


# Count of attended diabetic medicine appointments per patient from  study start
# tretspef == 307 (diabetic medicine)
hesop_appts5 %>% 
  filter(attended %in% c(5,6) & tretspef == '307' & apptdate >= study_start & diabetes_type %in% c('type1', 'type2')) %>% 
  tabyl(diabetes_type, firstatt_agg) 


# Attendance type by diabetes type
addmargins(xtabs(~ diabetes_type + attended_agg, data = hesop_appts5 %>% filter(apptdate >=study_start & tretspef == '307'& diabetes_type %in% c('type1', 'type2') )))
atentype_table <- round(prop.table(xtabs(~ diabetes_type + attended_agg, data = hesop_appts5 %>% filter(apptdate >=study_start & tretspef == '307' & diabetes_type %in% c('type1', 'type2')))), 3)
as.table(atentype_table)

addmargins(xtabs(~ attended_agg + diabetes_type, data = hesop_appts5 %>% filter(apptdate >=study_start & tretspef == '307' & diabetes_type %in% c('type1', 'type2'))))
atentype_group_table <- round(prop.table(xtabs(~ attended_agg + diabetes_type, data = hesop_appts5 %>% filter(apptdate >=study_start & tretspef == '307'& diabetes_type %in% c('type1', 'type2')))), 3)
as.table(atentype_group_table)

# Create tables for appointment type by diabetes type for diabetes medicine appts
table_diabetes_op_type  <- hesop_appts5 %>% 
  filter(apptdate >=study_start & tretspef == '307' & diabetes_type %in% c('type1', 'type2')) %>% 
  tabyl(diabetes_type, firstatt_agg) %>%
  rename(Follow_up = 'Follow up' )
  

table_diabetes_op_type<- table_diabetes_op_type %>% 
  mutate(New_FU_ratio = Follow_up / New) %>% 
  adorn_rounding(digits = 1)


# Filter for appointments that were attended
# 5= Seen, 6= Late but seen
hesop_appts_attend <- hesop_appts5 %>%
  filter(apptdate >=study_start &
           tretspef == '307' & 
           diabetes_type %in% c('type1', 'type2') &
           attended %in% c(5,6))


# Create tables for appointment type by diabetes type only for appts that were attended
table_diabetes_op_attend  <- hesop_appts_attend %>%
  tabyl(diabetes_type, firstatt_agg)%>%
  rename(Follow_up = 'Follow up' )

table_diabetes_op_attend <- table_diabetes_op_attend %>%
  mutate(New_FU_ratio = Follow_up /New) %>% 
  adorn_rounding(digits = 1)


# Do same for cancelled appointments and DNA
table_OPattendance <- hesop_appts5 %>%
  filter(apptdate >=study_start &
           tretspef == '307' &
           diabetes_type %in% c('type1', 'type2')) %>%
  tabyl(diabetes_type, attended_agg) %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 0) %>% 
  # adorn_ns() 


# Tabulate attendance by diabetes type and appointment type
# Type 1
table_type1_attend <- hesop_appts5 %>%
  filter(apptdate >=study_start & tretspef == '307') %>%
  filter(diabetes_type == 'type1') %>%
  tabyl(firstatt_agg, attended_agg) %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns()


# Type 2
table_type2_attend <- hesop_appts5 %>%
  filter(apptdate >=study_start & tretspef == '307') %>%
  filter(diabetes_type == 'type2') %>%
  tabyl(firstatt_agg, attended_agg) %>%
  adorn_percentages() %>% 
  adorn_pct_formatting(digits = 0) %>% 
  adorn_ns()



# Calculate mean number of appointments attended by diabetes type 

DM_Appt_count_pp <- hesop_appts5 %>%
  filter(apptdate >=study_start &
           tretspef == '307' & 
           diabetes_type %in% c('type1', 'type2') &
           attended %in% c(5,6)) %>% 
  select(patid, diabetes_type, apptdate, firstatt_agg, attended_agg) %>% # remove if looking at all scheduled 
  arrange(patid, apptdate) %>% 
  group_by(patid, diabetes_type) %>% 
  mutate(count = n()) %>% 
  tally() %>% 
  ungroup


table_diabetes_mean_app  <- DM_Appt_count_pp %>% 
  group_by(diabetes_type) %>% 
  summarise(sum = sum(n), mean = mean(n)) 
    

DM_Appt_count_newFU <- hesop_appts5 %>%
  filter(apptdate >=study_start &
           tretspef == '307' & 
           diabetes_type %in% c('type1', 'type2') &
           attended %in% c(5,6)) %>% # Remove for to obtain values for all scheduled appointments
  select(patid, diabetes_type, apptdate, firstatt_agg, attended_agg) %>%
  arrange(patid, apptdate) %>% 
  group_by(patid, diabetes_type, firstatt_agg) %>% 
  mutate(count = n()) %>% 
  tally() %>% 
  ungroup


table_diabetes_mean_newFU  <- DM_Appt_count_newFU %>% 
  filter(firstatt_agg == "Follow up") %>% 
  group_by(diabetes_type, firstatt_agg) %>% 
  summarise(sum = sum(n), mean = mean(n)) 


# Create summary table for all outpatient data using tableone package--------------------

# Get variable names
dput(names(hesop_appts5))

# Vector of summary variables
myVars <- c("ethnos", "admincat", "attended", 
            "firstatt", "outcome", "priority", 
            "refsourc", "servtype", "stafftyp",
            "tretspef_name", "discharged")


# Vector of categorical variables to be transformed
catVars <- c("admincat", "attended", "firstatt", 
             "outcome", "priority", "refsourc",
             "servtype", "stafftyp", "tretspef",
             "tretspef_name","discharged")


# Create Table
Table1 <- CreateTableOne(vars = myVars, strata = c("diabetes_type"), 
                         data = hesop_appts5 %>% filter(apptdate >=study_start & diabetes_type %in% c('type1', 'type2')), 
                         factorVars = catVars,
                         test = FALSE,
                         includeNA = TRUE)

Table1_csv <- print(Table1,
                    quote = FALSE, noSpaces = TRUE)

write.csv(Table1_csv, str_c(summary_stats_path, 'HES_descriptives/20191028_table1_all2.csv'))

# Create summary table for all diabetes outpatients data using tableone package--------------

# Vector of summary variables
myVars <- c("ethnos", "admincat", "attended", 
            "firstatt", "outcome", "priority", "refsourc", "servtype", 
            "stafftyp", "tretspef_name",
            "diabetes_type", "discharged")

# Vector of categorical variables to be transformed
catVars <- c("admincat", "attended", "firstatt", 
             "outcome", "priority", "refsourc",
             "servtype", "stafftyp", "tretspef", "tretspef_name",
             "discharged")


# Create Table %>% 
Table2 <- CreateTableOne(vars = myVars, strata = c("diabetes_type"), 
                         data = hesop_appts5 %>% filter(apptdate >=study_start & tretspef == '307'& diabetes_type %in% c('type1', 'type2')), 
                         factorVars = catVars,
                         test = FALSE,
                         includeNA = TRUE)

Table2_csv <- print(Table2,
                    quote = FALSE, noSpaces = TRUE)

write.csv(Table2_csv, str_c(summary_stats_path, 'HES_descriptives/20191028_table2_diabetes_medicine_stratified by diabetes.csv'))

summary(Table2)

