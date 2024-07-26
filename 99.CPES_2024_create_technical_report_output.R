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
#Response rate by NHS Board of Residence
#Response rate by Deprivation (quintile)
#Response rate by urban / rural category
#Response rate by Cancer Group
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
  select(uniquepatientsurveyid,iqvia_flagdate,iqvia_exclude)#iqvia_exclude already in "validated_results.rds"
Final_Master_Sample_File$flag = 1

#Read in list on selected patients for surveying - this also returns validated results to obtain survey respondents and response method####
patient_info <- readRDS(paste0(analysis_output_path,"validated_results.rds")) #after exclusions (i.e.patients who had moved, were deceased, were ineligible or were distressed)
rm(Final_Master_Sample_File)
patient_info$flag <- NULL

patient_info <- patient_info %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), "")))

#Table 1 Response rate by submission method (PUBLISHED)####
#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").
Submission_Response_rate_base <- subset(patient_info, responsecode == 1)
Submission_Response_rate_all <- Submission_Response_rate_base %>%
  count(responsecode) %>%
  group_by(responsecode) %>%
  mutate(percent=n/sum(n))

Submission_Response_rate_all$percent <- percent (Submission_Response_rate_all$percent)

#Rename column "responsecode" to "Method" for later merging
colnames(Submission_Response_rate_all)[which(names(Submission_Response_rate_all) == "responsecode")] <-"Method"
Submission_Response_rate_all$Method[Submission_Response_rate_all$Method=="1"] <- "Scotland"

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

#SET SCOTLAND LEVEL RESPONSE RATE LINE####
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


#Table 2 Response rate by Health Board of treatment####
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


#Table 3 Response rate by Health Board of residence####
#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
HBR_Response_rate_all <- patient_info %>%
  count(board_of_residence) %>%
  group_by(board_of_residence) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(HBR_Response_rate_all)[which(names(HBR_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY HBR
HBR_Response_rate_base <- subset(patient_info, responsecode == 1)
HBR_Response_rate <- HBR_Response_rate_base %>%
  count(board_of_residence)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(HBR_Response_rate)[which(names(HBR_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "HBR_Response_rate_OVERALL"
HBR_Response_rate_OVERALL <- left_join(HBR_Response_rate_all,HBR_Response_rate,by = c("board_of_residence"))

#Calculate percentage response
HBR_Response_rate_OVERALL <- HBR_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
HBR_Response_rate_OVERALL$Response_Rate <- percent (HBR_Response_rate_OVERALL$Response_Rate)

rm(HBR_Response_rate_all)
rm(HBR_Response_rate_base)
rm(HBR_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "Practice_Population" to "board_of_treatment" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "board_of_treatment")] <-"board_of_residence"

#CREATE df HB_Response_rate_OVERALL"
HBR_Response_rate_OVERALL <-bind_rows(HBR_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Table 4 Response rate by deprivation quintile ####
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
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "board_of_residence")] <-"simd2020v2_sc_quintile_smr01"

#CREATE df SIMD_Response_rate_OVERALL"
SIMD_Response_rate_OVERALL <-bind_rows(SIMD_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Table 5 Response rate by urban / rural location ####
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


#Table 6 Response rate by age group ####
patient_info <- patient_info %>%
  mutate(age_band_6 = six_age_bands(age_chi))

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
Age_Response_rate_all <- patient_info %>%
  count(age_band_6) %>%
  group_by(age_band_6) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Age_Response_rate_all)[which(names(Age_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY AGE
Age_Response_rate_base <- subset(patient_info, responsecode == 1)
Age_Response_rate <- Age_Response_rate_base %>%
  count(age_band_6)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Age_Response_rate)[which(names(Age_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "SIMD_Response_rate_OVERALL"
Age_Response_rate_OVERALL <- left_join(Age_Response_rate_all,Age_Response_rate,by = c("age_band_6"))

#Calculate percentage response
Age_Response_rate_OVERALL <- Age_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
Age_Response_rate_OVERALL$Response_Rate <- percent (Age_Response_rate_OVERALL$Response_Rate)

rm(Age_Response_rate_all)
rm(Age_Response_rate_base)
rm(Age_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "ur6_2020" to "age_band_6" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "ur6_2020_smr01")] <-"age_band_6"

#CREATE df SIMD_Response_rate_OVERALL"
Age_Response_rate_OVERALL <-bind_rows(Age_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Table 7 Response rate by sex ####
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
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "age_band_6")] <-"smr01_sex_label"

#CREATE df SIMD_Response_rate_OVERALL"
Sex_Response_rate_OVERALL <-bind_rows(Sex_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Table 8 Response rate by cancer group####
#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
Cancer_group_Response_rate_all <- patient_info %>%
  count(cancer_group_smr06) %>%
  group_by(cancer_group_smr06) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Cancer_group_Response_rate_all)[which(names(Cancer_group_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY Cancer_group
Cancer_group_Response_rate_base <- subset(patient_info, responsecode == 1)
Cancer_group_Response_rate <- Cancer_group_Response_rate_base %>%
  count(cancer_group_smr06)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Cancer_group_Response_rate)[which(names(Cancer_group_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "Cancer_group_Response_rate_OVERALL"
Cancer_group_Response_rate_OVERALL <- left_join(Cancer_group_Response_rate_all,Cancer_group_Response_rate,by = c("cancer_group_smr06"))

#Calculate percentage response
Cancer_group_Response_rate_OVERALL <- Cancer_group_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
Cancer_group_Response_rate_OVERALL$Response_Rate <- percent (Cancer_group_Response_rate_OVERALL$Response_Rate)

rm(Cancer_group_Response_rate_all)
rm(Cancer_group_Response_rate_base)
rm(Cancer_group_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "Practice_Population" to "cancer_group_text" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "smr01_sex_label")] <-"cancer_group_smr06"

#CREATE df Cancer_group_Response_rate_OVERALL"
Cancer_group_Response_rate_OVERALL <-bind_rows(Cancer_group_Response_rate_OVERALL,SCOTLAND_Response_rate_OVERALL)


#Pick up reporting template and populate, save outfile as xlsx####

#Add Date Tables Run
Table_Last_Updated <-Sys.Date()
Table_Last_Updated <-format(Table_Last_Updated, format ="%d %B %Y")
run_date <-data.frame(Table_Last_Updated)

template <- loadWorkbook(paste0(output_path,"technical_report/technical_report_tables_template.xlsx"))
writeData(template, "Submission Method", Submission_Response_rate_OVERALL, startCol = 3, startRow = 6)
writeData(template, "HBT", HB_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "HBR", HBR_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "SIMD", SIMD_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "UR6", UR6_Response_rate_OVERALL, startCol = 3, startRow = 6)
writeData(template, "Age", Age_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "Sex", Sex_Response_rate_OVERALL, startCol = 2, startRow = 6)
writeData(template, "Cancer Group", Cancer_group_Response_rate_OVERALL, startCol = 2, startRow = 6)

writeData(template, "Submission Method", run_date, startCol = 1, startRow = 15)
writeData(template, "HBT", run_date, startCol = 1, startRow = 27)
writeData(template, "HBR", run_date, startCol = 1, startRow = 26)
writeData(template, "SIMD", run_date, startCol = 1, startRow = 19)
writeData(template, "UR6", run_date, startCol = 1, startRow = 18)
writeData(template, "Age", run_date, startCol = 1, startRow = 18)
writeData(template, "Sex", run_date, startCol = 1, startRow = 15)
writeData(template, "Cancer Group", run_date, startCol = 1, startRow = 24)

saveWorkbook(template, (paste0(output_path,"technical_report/technical_report_populated_tables.xlsx")), overwrite =TRUE)

###################################################################################################################################
#Technical report output - removals note: ####

#Total No Sampled 
#No Removed Pre Survey and Day 1 Mail Out (Deaths)
#No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)
#Total No Sent out 
#Total Deaths Day 2 (2nd Mail Out day)
#Total Deaths Day 3 (3rd Mail Out day)
#Total No of Removals Sent to QH (as part of the reminder mail out)

#Inputs: data_path,"sample/2024.07.17_finalised_master_CPES_list.rds"
#Output Template: output_path,"technical_report/removals_template.xlsx"
#Outputs:output_path,"technical_report/sample_removals_note.xlsx"

#Read in list on selected patients for surveying
master_list <- readRDS(paste0(data_path,"sample/2024.07.17_finalised_master_CPES_list.rds"))
ls(master_list)
master_list <- master_list %>% 
  select("iqvia_flagdate","nhscr_date","nhscr_reason","chili_date","chili_reason",
                                           "iqvia_exclude","reason","additional_exclusion_flag")

master_list$iqvia_flagdate <- as.character(master_list$iqvia_flagdate)
master_list$nhscr_date <- as.character(master_list$nhscr_date)
master_list$chili_date <- as.character(master_list$chili_date)
master_list <- master_list %>%
  mutate(across(everything(), ~ replace(.x, is.na(.x), "")))

master_list <- master_list %>%
  mutate(marker=1)

#Line1 Total No Sampled ####
Line1 <- master_list %>%
  count(marker) %>%
  group_by(marker) 
Line1$marker[Line1$marker=="1"] <- "Total No Sampled" 

#Line2 No Removed Pre Survey and Day 1 Mail Out (Deaths)####
Line2 <- master_list %>%
  filter((iqvia_flagdate =="2024-02-07" | iqvia_flagdate =="2024-02-14") & reason == "Death")
Line2 <- Line2 %>%
  count(marker) %>%
  group_by(marker) 
Line2$marker[Line2$marker=="1"] <- "No Removed Pre Survey and Day 1 Mail Out (Deaths)" 

#Line3 No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)####
Line3 <- master_list %>%
  filter((iqvia_flagdate =="2024-02-07" & reason =="Other") | iqvia_flagdate =="2024-02-14" & reason =="Other")
Line3 <- Line3 %>%
  count(marker) %>%
  group_by(marker) 
Line3$marker[Line3$marker=="1"] <- "No Removed Pre Survey and Day 1 Mail Out (Other/Non Scots)" 

#Line4 Total No Sent out ####
Line4 <- master_list %>%
  filter(iqvia_flagdate !="2024-02-07" & iqvia_flagdate  !="2024-02-14")
Line4 <- Line4 %>%
  count(marker) %>%
  group_by(marker) 
Line4$marker[Line4$marker=="1"] <- "Total No Sent out on day 1 of mail out (14/02/24)" 

#Line5 Total Deaths Day 2 (2nd Mail Out day) ####
Line5 <- master_list %>%
  filter(iqvia_flagdate =="2024-03-06")
Line5 <- Line5 %>%
  count(marker) %>%
  group_by(marker) 
Line5$marker[Line5$marker=="1"] <- "Total Exclusions (Mail out day 2 (06/03/24))" 

#Line6 Total Deaths Day 3 (3rd Mail Out day) ####
Line6 <- master_list %>%
  filter(iqvia_flagdate =="2024-03-20" )
Line6 <- Line6 %>%
  count(marker) %>%
  group_by(marker) 
Line6$marker[Line6$marker=="1"] <- "Total Exclusions (Mail out day 3 (20/03/24))" 

#Line7 Total No of Removals Sent to QH (as part of the reminder mail out)####
Line7 <- master_list %>%
  filter((iqvia_flagdate =="2024-02-07" | iqvia_flagdate =="2024-02-14" 
          | iqvia_flagdate =="2024-03-06" | iqvia_flagdate =="2024-03-20") & iqvia_exclude == 1)
Line7 <- Line7 %>%
  count(marker) %>%
  group_by(marker) 
Line7$marker[Line7$marker=="1"] <- "Total No of Removals Sent to QH" 

removals <-bind_rows(Line1,Line2,Line3,Line4,Line5,Line6,Line7)
rm(Line1,Line2,Line3,Line4,Line5,Line6,Line7,master_list)

colnames(removals)[which(names(removals) == "marker")] <-"Mailout Stage"
colnames(removals)[which(names(removals) == "n")] <-"Number of forms"

#Pick up reporting template and populate, save outfile as xlsx####

template <- loadWorkbook(paste0(output_path,"technical_report/removals_template.xlsx"))
writeData(template, "removals_summary", removals, startCol = 2, startRow = 9)
saveWorkbook(template, (paste0(output_path,"technical_report/sample_removals_note.xlsx")), overwrite =TRUE)