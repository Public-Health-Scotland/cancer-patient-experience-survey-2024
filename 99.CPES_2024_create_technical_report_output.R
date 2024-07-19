# Written by Martin Leitch
# July 2024.
# Adapted from the HACE 2024 code by Martin Leitch

# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  output analyses at all levels of reporting.
# 
# Approximate run time: 1 min total (approx)
# 
# Approximate memory usage: 570 MiB
# 
# *****************************************

#Inputs: 
#data_path,"/sample/Master Sample File/2024.07.17_finalised_master_CPES_list.rds"
#analysis_output_path,"validated_results.rds"

#Outputs:
#analysis_output_path, "/technical report/2024_02_28_technical_report_populated_tables.xlsx"

#Technical report output.
#Purpose: Create output for the CPES 2024 Technical Output.
#Output based on 2018 CPES Technical Report:
#Response rate by age group: 
#	16 to 34
# 35 to 44
#	45 to 54
#	55 to 64
#	65 to 74
#	75+
#Response rate by sex:
#Response rate by NHS Board of Treatment
#Response rate by Deprivation (quintile)
#Response rate by urban / rural category
#Response rate by Tumour Group
#Response by method


#Run CPES functions####
source("00.CPES_2024_set_up_packages.R")
library(formattable)
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Read in Master Sample File as at the end of the mailing process
Final_Master_Sample_File <- readRDS(paste0(data_path,"sample/2024.07.17_finalised_master_CPES_list.rds"))
ls(Final_Master_Sample_File)
Final_Master_Sample_File <- Final_Master_Sample_File %>%
  select(uniquepatientsurveyid,iqvia_flagdate,iqvia_exclude)
Final_Master_Sample_File$flag = 1

#Read in list on selected patients for surveying - this also returns validated results to obtain survey respondents and response method####
patient_info <- readRDS(paste0(analysis_output_path,"validated_results.rds")) 
#match on Master Sample File information####
patient_info <- left_join(patient_info,Final_Master_Sample_File,by = c("patientid" = "uniquepatientsurveyid"))
## Deaths: Were there any cases where: ####
# * 1) the patient died, but completed the survey prior to death? Yes: these cases will remain in the sample.
# * 2) the patient died, but this was not picked-up by NHSCR / CHILI? 
#      (i.e. Notified by family and after last mailout day) Yes: these cases will be removed from the sample.
## Code into respondents / non-respondents and remove patients who were excluded from the sample before final analysis. ####
# * Recode Response.Code into patients who responded to the survey and those who did not. 
# * Also flag patients who should be excluded from the sample (patients who had moved, were deceased, were ineligible or were distressed).
patient_info <- patient_info %>% 
  mutate(Responded = case_when(responsecode == 1~ 1, #Responded
                               responsecode %in% c(4,6,NA) ~ 2, #Did not respond
                               responsecode %in% c(2, 3, 5, 7) ~ 3, #Exclude
                               TRUE ~ 9))
table(patient_info$Responded,patient_info$responsecode,useNA = c("always"))
table(patient_info$Responded,useNA = c("always"))
patient_info <- patient_info %>% 
  filter(Responded != 3)
rm(Final_Master_Sample_File)
patient_info$flag <- NULL

patient_info <- patient_info %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), "")))

#Table # Response rate by submission method (PUBLISHED)####
#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").
Submission_Response_rate_base <- subset(patient_info, responsecode == 1)
Submission_Response_rate_all <- Submission_Response_rate_base %>%
  count(responsecode) %>%
  group_by(responsecode) %>%
  mutate(percent=n/sum(n))

Submission_Response_rate_all$percent <- percent (Submission_Response_rate_all$percent)

#Rename column "responsecode" to "Method" for later merging
colnames(Submission_Response_rate_all)[which(names(Submission_Response_rate_all) == "responsecode")] <-"Method"
Submission_Response_rate_all$Method[Submission_Response_rate_all$Method=="1"] <- "Total"

#CALCULATE NUMBER OF RESPONSES BY SUBMISSION METHOD
Submission_Response_rate <- Submission_Response_rate_base %>%
  count(responsesubcode) %>%
  mutate(percent=n/sum(n))

Submission_Response_rate$percent <- percent (Submission_Response_rate$percent)

#Rename column "response.sub.code" to "Method" for later merging
colnames(Submission_Response_rate)[which(names(Submission_Response_rate) == "responsesubcode")] <-"Method"

#CREATE df Submission_Response_rate_OVERALL"
Submission_Response_rate_OVERALL <-bind_rows(Submission_Response_rate,Submission_Response_rate_all)

#Rename column "n" to "Number of Responses" 
colnames(Submission_Response_rate_OVERALL)[which(names(Submission_Response_rate_OVERALL) == "n")] <-"Number of responses"

rm(Submission_Response_rate_base)
rm(Submission_Response_rate_all)
rm(Submission_Response_rate)

#SET A SCOTLAND LEVEL RESPONSE RATE LINE####
#Recalculate total number of forms sent out.
patient_info <- patient_info %>% 
  mutate(Scotland_mark=1)
SCOTLAND_Response_rate_base <- patient_info %>%
  count(Scotland_mark) %>%
  group_by(Scotland_mark) 

#Rename column "response.code.x" to "Method" for later merging
colnames(SCOTLAND_Response_rate_base)[which(names(SCOTLAND_Response_rate_base) == "n")] <-"Total_number_of_forms_sent_out"
SCOTLAND_Response_rate_base$Scotland_mark[SCOTLAND_Response_rate_base$Scotland_mark=="1"] <- "Scotland"

#Recalculate total number of respondents
SCOTLAND_Response_rate <- subset(patient_info, responsecode == 1)
SCOTLAND_Response_rate <- SCOTLAND_Response_rate %>%
  count(responsecode) %>%
  group_by(responsecode) 

#Rename column "response.sub.code" to "Method" for later merging
colnames(SCOTLAND_Response_rate)[which(names(SCOTLAND_Response_rate) == "n")] <-"Number_of_Responses"
colnames(SCOTLAND_Response_rate)[which(names(SCOTLAND_Response_rate) == "responsecode")] <-"Scotland_mark"
SCOTLAND_Response_rate$Scotland_mark[SCOTLAND_Response_rate$Scotland_mark=="1"] <- "Scotland"

#CREATE df "Practice_Population_Response_rate_OVERALL"
SCOTLAND_Response_rate_OVERALL <- left_join(SCOTLAND_Response_rate_base,SCOTLAND_Response_rate,by = c("Scotland_mark"))

#Calculate percentage response
SCOTLAND_Response_rate_OVERALL <- SCOTLAND_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
SCOTLAND_Response_rate_OVERALL$Response_Rate <- percent (SCOTLAND_Response_rate_OVERALL$Response_Rate)

rm(SCOTLAND_Response_rate_base)
rm(SCOTLAND_Response_rate)

#Table # Response rate by Health Board of treatment####

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
HB_Response_rate_all <- patient_info %>%
  count(board_of_treatment) %>%
  group_by(board_of_treatment) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(HB_Response_rate_all)[which(names(HB_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY HB
HB_Response_rate_base <- subset(patient_info, responsecode == 1)
HB_Response_rate <- HB_Response_rate_base %>%
  count(board_of_treatment)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(HB_Response_rate)[which(names(HB_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "HB_Response_rate_OVERALL"
HB_Response_rate_OVERALL <- left_join(HB_Response_rate_all,HB_Response_rate,by = c("board_of_treatment"))

#Calculate percentage response
HB_Response_rate_OVERALL <- HB_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
HB_Response_rate_OVERALL$Response_Rate <- percent (HB_Response_rate_OVERALL$Response_Rate)

rm(HB_Response_rate_all)
rm(HB_Response_rate_base)
rm(HB_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "Practice_Population" to "board_of_treatment" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "Scotland_mark")] <-"board_of_treatment"

#CREATE df HB_Response_rate_OVERALL"
HB_Response_rate_OVERALL <-bind_rows(HB_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Table # Response rate by deprivation quintile ####

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
SIMD_Response_rate_all <- patient_info %>%
  count(simd2020v2_sc_quintile_smr01) %>%
  group_by(simd2020v2_sc_quintile_smr01) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SIMD_Response_rate_all)[which(names(SIMD_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY SIMD
SIMD_Response_rate_base <- subset(patient_info, responsecode == 1)
SIMD_Response_rate <- SIMD_Response_rate_base %>%
  count(simd2020v2_sc_quintile_smr01)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SIMD_Response_rate)[which(names(SIMD_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "SIMD_Response_rate_OVERALL"
SIMD_Response_rate_OVERALL <- left_join(SIMD_Response_rate_all,SIMD_Response_rate,by = c("simd2020v2_sc_quintile_smr01"))

#Calculate percentage response
SIMD_Response_rate_OVERALL <- SIMD_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
SIMD_Response_rate_OVERALL$Response_Rate <- percent (SIMD_Response_rate_OVERALL$Response_Rate)

rm(SIMD_Response_rate_all)
rm(SIMD_Response_rate_base)
rm(SIMD_Response_rate)

#reformat SIMD_Response_rate_OVERALL$simd2020v2_sc_quintile_smr01 to character in order to match up with SCOTLAND_Response_rate_OVERALL
SIMD_Response_rate_OVERALL$simd2020v2_sc_quintile_smr01 <- as.character(SIMD_Response_rate_OVERALL$simd2020v2_sc_quintile_smr01)

#IN SCOTLAND_Response_rate_OVERALL Rename column "practice_hscp_name" to "simd2020v2_sc_quintile_smr01" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "board_of_treatment")] <-"simd2020v2_sc_quintile_smr01"

#CREATE df SIMD_Response_rate_OVERALL"
SIMD_Response_rate_OVERALL <-bind_rows(SIMD_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)

#Table # Response rate by urban / rural location ####

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
UR6_Response_rate_all <- patient_info %>%
  count(ur6_2020_smr01) %>%
  group_by(ur6_2020_smr01) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(UR6_Response_rate_all)[which(names(UR6_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY UR6
UR6_Response_rate_base <- subset(patient_info, responsecode == 1)
UR6_Response_rate <- UR6_Response_rate_base %>%
  count(ur6_2020_smr01)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(UR6_Response_rate)[which(names(UR6_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "SIMD_Response_rate_OVERALL"
UR6_Response_rate_OVERALL <- left_join(UR6_Response_rate_all,UR6_Response_rate,by = c("ur6_2020_smr01"))

#Calculate percentage response
UR6_Response_rate_OVERALL <- UR6_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
UR6_Response_rate_OVERALL$Response_Rate <- percent (UR6_Response_rate_OVERALL$Response_Rate)

rm(UR6_Response_rate_all)
rm(UR6_Response_rate_base)
rm(UR6_Response_rate)

#reformat ur6_2016 to character in order to match up with SCOTLAND_Response_rate_OVERALL
UR6_Response_rate_OVERALL$ur6_2020_smr01 <- as.character(UR6_Response_rate_OVERALL$ur6_2020_smr01)

#IN SCOTLAND_Response_rate_OVERALL Rename column "simd2020v2_sc_quintile_smr01" to "ur6_2016" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "simd2020v2_sc_quintile_smr01")] <-"ur6_2020_smr01"

#CREATE df SIMD_Response_rate_OVERALL"
UR6_Response_rate_OVERALL <-bind_rows(UR6_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)

#Table # Response rate by age group ####

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
Age_Response_rate_all <- patient_info %>%
  count(age_group_chi) %>%
  group_by(age_group_chi) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Age_Response_rate_all)[which(names(Age_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY AGE
Age_Response_rate_base <- subset(patient_info, responsecode == 1)
Age_Response_rate <- Age_Response_rate_base %>%
  count(age_group_chi)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Age_Response_rate)[which(names(Age_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "SIMD_Response_rate_OVERALL"
Age_Response_rate_OVERALL <- left_join(Age_Response_rate_all,Age_Response_rate,by = c("age_group_chi"))

#Calculate percentage response
Age_Response_rate_OVERALL <- Age_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
Age_Response_rate_OVERALL$Response_Rate <- percent (Age_Response_rate_OVERALL$Response_Rate)

rm(Age_Response_rate_all)
rm(Age_Response_rate_base)
rm(Age_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "ur6_2020" to "age_band_6" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "ur6_2020_smr01")] <-"age_group_chi"

#CREATE df SIMD_Response_rate_OVERALL"
Age_Response_rate_OVERALL <-bind_rows(Age_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)

#Table # Response rate by sex ####
#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
Sex_Response_rate_all <- patient_info %>%
  count(smr01_sex_label) %>%
  group_by(smr01_sex_label) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Sex_Response_rate_all)[which(names(Sex_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY SEX
Sex_Response_rate_base <- subset(patient_info, responsecode == 1)
Sex_Response_rate <- Sex_Response_rate_base %>%
  count(smr01_sex_label)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Sex_Response_rate)[which(names(Sex_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "SIMD_Response_rate_OVERALL"
Sex_Response_rate_OVERALL <- left_join(Sex_Response_rate_all,Sex_Response_rate,by = c("smr01_sex_label"))

#Calculate percentage response
Sex_Response_rate_OVERALL <- Sex_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
Sex_Response_rate_OVERALL$Response_Rate <- percent (Sex_Response_rate_OVERALL$Response_Rate)

rm(Sex_Response_rate_all)
rm(Sex_Response_rate_base)
rm(Sex_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "age_band_6" to "SEX_DESC" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "age_group_chi")] <-"smr01_sex_label"

#CREATE df SIMD_Response_rate_OVERALL"
Sex_Response_rate_OVERALL <-bind_rows(Sex_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Table # Response rate by Tumour Group####

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
Tumour_group_Response_rate_all <- patient_info %>%
  count(tumour_group_text) %>%
  group_by(tumour_group_text) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Tumour_group_Response_rate_all)[which(names(Tumour_group_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY Tumour_group
Tumour_group_Response_rate_base <- subset(patient_info, responsecode == 1)
Tumour_group_Response_rate <- Tumour_group_Response_rate_base %>%
  count(tumour_group_text)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Tumour_group_Response_rate)[which(names(Tumour_group_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "Tumour_group_Response_rate_OVERALL"
Tumour_group_Response_rate_OVERALL <- left_join(Tumour_group_Response_rate_all,Tumour_group_Response_rate,by = c("tumour_group_text"))

#Calculate percentage response
Tumour_group_Response_rate_OVERALL <- Tumour_group_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
Tumour_group_Response_rate_OVERALL$Response_Rate <- percent (Tumour_group_Response_rate_OVERALL$Response_Rate)

rm(Tumour_group_Response_rate_all)
rm(Tumour_group_Response_rate_base)
rm(Tumour_group_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "Practice_Population" to "tumour_group_text" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "smr01_sex_label")] <-"tumour_group_text"

#CREATE df Tumour_group_Response_rate_OVERALL"
Tumour_group_Response_rate_OVERALL <-bind_rows(Tumour_group_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)

#DONE TO HERE####

#Pick up reporting template and populate, save outfile as xlsx####

#Add Date Tables Run
Table_Last_Updated <-Sys.Date()
Table_Last_Updated <-format(Table_Last_Updated, format ="%d %B %Y")
run_date <-data.frame(Table_Last_Updated)

template <- loadWorkbook(paste0("output/technical report/technical_report_tables_template.xlsx"))
writeData(template, "Submission Method", Submission_Response_rate_OVERALL, startCol = 3, startRow = 6)
writeData(template, "Practice List Size", Practice_Population_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "Board", HB_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "HSCP", HSCP_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "SIMD", SIMD_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "UR6", UR6_Response_rate_OVERALL, startCol = 3, startRow = 6)
writeData(template, "Sex", Sex_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "Age", Age_Response_rate_OVERALL, startCol = 2, startRow = 6)


writeData(template, "Submission Method", run_date, startCol = 1, startRow = 15)
writeData(template, "Practice List Size", run_date, startCol = 1, startRow = 18)
writeData(template, "Board", run_date, startCol = 1, startRow = 29)
writeData(template, "HSCP", run_date, startCol = 1, startRow = 43)
writeData(template, "SIMD", run_date, startCol = 1, startRow = 21)
writeData(template, "UR6", run_date, startCol = 1, startRow = 20)
writeData(template, "Sex", run_date, startCol = 1, startRow = 16)
writeData(template, "Age", run_date, startCol = 1, startRow = 18)


saveWorkbook(template, (paste0("output/technical report/2024_02_28_technical_report_populated_tables.xlsx")), overwrite =TRUE)

###################################################################################################################################

#5.1 Technical report: count of removals for summary template####
# Written by Martin Leitch
# February 2022.

# *****************************************
# load in libraries
library(readxl)
#version 1.3.1
library(tidyverse)
#version 1.2.1
library(readr)
#version 1.3.1
library(formattable)
#version 0.2.1
library(openxlsx)
#version 4.2.5

#Technical report output - removals note:

#Total No Sampled 
#No Removed Pre Survey and Day 1 Mail Out (Deaths)
#No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)
#Total No Sent out 
#Total Deaths Day 2 (Rest of Initial Mail Out)
#Total No of Removals Sent to QH (after Day 2)

#=========

#Inputs: "data/sampling/Master Sample File/2023.12.01_master_HACE_list_post_mailout.parquet"
#Output Template: "output/technical_report/removals_template.xlsx"
#Outputs:"output/technical_report/sample_removals_note.xlsx"

#Define directories
setwd("/conf/bss/pat-exp-surveys/health-and-care/202324/")

#Read in list on selected patients for surveying
master_list <- read_parquet("data/sampling/Master Sample File/2023.12.01_master_HACE_list_post_mailout.parquet", as_data_frame = FALSE) %>%
  slice_head(n = 1) %>%
  collect()
ls(master_list)
master_list <- read_parquet("data/sampling/Master Sample File/2023.12.01_master_HACE_list_post_mailout.parquet",
                            col_select = c("IQVIA_flagdate","nhscr_date","NHSCR_reason","chili_date","chili_reason",
                                           "IQVIA_exclude","pre_survey_exclusion","reason","primary_exclusion_source"))
colnames(master_list)<-tolower(colnames(master_list))  

master_list$iqvia_flagdate <- as.character(master_list$iqvia_flagdate)
master_list$nhscr_date <- as.character(master_list$nhscr_date)
master_list$chili_date <- as.character(master_list$chili_date)
master_list <- master_list %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), "")))

master_list <- master_list %>%
  mutate(marker=1)

table(master_list$iqvia_flagdate)
#2023-10-19 2023-10-23 2023-10-24 2023-10-25 2023-11-08 2023-11-09 2023-11-10 2023-11-13 2023-11-14 2023-11-15 2023-11-16 2023-11-17 
#      1504        114         54         36        311         35         24         39         17         49         23         32  
table(master_list$nhscr_date)
#2023-10-12 2023-10-19 2023-10-23 2023-10-24 2023-10-25 2023-11-08 2023-11-09 2023-11-10 2023-11-13 2023-11-14 2023-11-15 2023-11-16 2023-11-17 
#      1014        354         56         22         19        266         36         24         25         21         37         28         29 
table(master_list$nhscr_reason)
#Death Other 
#  993   938
table(master_list$chili_date)
#2023-10-19 2023-10-23 2023-10-24 2023-10-25 2023-11-08 2023-11-09 2023-11-13 2023-11-15 2023-11-16 2023-11-17 
#       911        154         64         50        169        145         34         85         39         15
table(master_list$chili_reason)
#CHI Redundant         Death   Transferred 
#           25           949           692 
table(master_list$pre_survey_exclusion)
#  0    1 
#620 1618
table(master_list$reason)
#Death Other 
# 1016  1222
table(master_list$primary_exclusion_source)
#CHILI NHSCR 
#  743  1495 
table(master_list$iqvia_flagdate,master_list$reason)
#           Death Other
#2023-10-19   458  1046
#2023-10-23    54    60
#2023-10-24    27    27
#2023-10-25    20    16
#2023-11-08   238    73
#2023-11-09    35     0
#2023-11-10    24     0
#2023-11-13    39     0
#2023-11-14    17     0
#2023-11-15    49     0
#2023-11-16    23     0
#2023-11-17    32     0

#Line1 Total No Sampled ####
Line1 <- master_list %>%
  count(marker) %>%
  group_by(marker) 
Line1$marker[Line1$marker=="1"] <- "Total No Sampled" #528376

#line2 No Removed Pre Survey and Day 1 Mail Out (Deaths)####
Line2 <- master_list %>%
  filter((iqvia_flagdate =="2023-10-19" | iqvia_flagdate =="2023-10-23") & reason == "Death")
Line2 <- Line2 %>%
  count(marker) %>%
  group_by(marker) 
Line2$marker[Line2$marker=="1"] <- "No Removed Pre Survey and Day 1 Mail Out (Deaths)" #512

#Line3 No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)####
Line3 <- master_list %>%
  filter((iqvia_flagdate =="2023-10-19" & reason =="Other") | iqvia_flagdate =="2023-10-23" & reason =="Other")
Line3 <- Line3 %>%
  count(marker) %>%
  group_by(marker) 
Line3$marker[Line3$marker=="1"] <- "No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)" #1106

#Line4 Total No Sent out ####
#Line4 <- master_list %>%
#  filter(iqvia_flagdate != "2023-10-19" & iqvia_flagdate !="2023-10-23")
Line4 <- master_list %>%
  filter(iqvia_flagdate !="2023-10-19" & iqvia_flagdate  !="2023-10-23")
Line4 <- Line4 %>%
  count(marker) %>%
  group_by(marker) 
Line4$marker[Line4$marker=="1"] <- "Total No Sent out on day 1 of mail out (23/10/23)" #526758

#Line5 Total Deaths Day 2 (Rest of Initial Mail Out) ####
Line5 <- master_list %>%
  filter(iqvia_flagdate =="2023-10-24" | iqvia_flagdate =="2023-10-25")
Line5 <- Line5 %>%
  count(marker) %>%
  group_by(marker) 
Line5$marker[Line5$marker=="1"] <- "Total Exclusions (Rest of Initial Mail Out phase)" #90

#Line6 Total No of Removals Sent to QH (as part of the reminder mail out)####
Line6 <- master_list %>%
  filter((iqvia_flagdate !="2023-10-19" & iqvia_flagdate !="2023-10-23" 
          & iqvia_flagdate !="2023-10-24" & iqvia_flagdate !="2023-10-25") & iqvia_exclude == 1)
Line6 <- Line6 %>%
  count(marker) %>%
  group_by(marker) 
Line6$marker[Line6$marker=="1"] <- "Total No of Removals Sent to QH (as part of reminder mail out phase)" #530

removals <-bind_rows(Line1,Line2,Line3,Line4,Line5,Line6)
rm(Line1)
rm(Line2)
rm(Line3)
rm(Line4)
rm(Line5)
rm(Line6)
rm(master_list)

colnames(removals)[which(names(removals) == "marker")] <-"Mailout Stage"

#Pick up reporting template and populate, save outfile as xlsx####

template <- loadWorkbook(paste0("output/technical report/removals_template.xlsx"))
writeData(template, "removals_summary", removals, startCol = 2, startRow = 9)
saveWorkbook(template, (paste0("output/technical report/2024_02_28_sample_removals_note.xlsx")), overwrite =TRUE)



