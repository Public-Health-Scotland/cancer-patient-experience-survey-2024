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
Final_Master_Sample_File <- readRDS(paste0(data_path,"sample/2024.08.12_finalised_master_CPES_list.rds"))
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

#Table 6 Response rate by age####
#patient_info <- patient_info %>%
#  mutate(age_band_6 = six_age_bands(age_chi))

#CALCULATE TOTAL NUMBER OF RESPONSES (bottom line of df "Total").  
Age_Response_rate_all <- patient_info %>%
  count(age_chi) %>%
  group_by(age_chi) 

#Rename column "n" to "Total_number_of_forms_sent_out" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Age_Response_rate_all)[which(names(Age_Response_rate_all) == "n")] <-"Total_number_of_forms_sent_out"

#CALCULATE NUMBER OF RESPONSES BY AGE
Age_Response_rate_base <- subset(patient_info, responsecode == 1)
Age_Response_rate <- Age_Response_rate_base %>%
  count(age_chi)

#Rename column "n" to "Number_of_Responses" for later merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(Age_Response_rate)[which(names(Age_Response_rate) == "n")] <-"Number_of_Responses"

#CREATE df "SIMD_Response_rate_OVERALL"
Age_Response_rate_OVERALL <- left_join(Age_Response_rate_all,Age_Response_rate,by = c("age_chi"))

#Calculate percentage response
Age_Response_rate_OVERALL <- Age_Response_rate_OVERALL %>%
  mutate(Response_Rate=(Number_of_Responses/Total_number_of_forms_sent_out))
Age_Response_rate_OVERALL$Response_Rate <- percent (Age_Response_rate_OVERALL$Response_Rate)

rm(Age_Response_rate_all)
rm(Age_Response_rate_base)
rm(Age_Response_rate)

#IN SCOTLAND_Response_rate_OVERALL Rename column "ur6_2020" to "age_band_6" for merging - UPDATE THIS MINI-SECTION AS NECESSARY
colnames(SCOTLAND_Response_rate_OVERALL)[which(names(SCOTLAND_Response_rate_OVERALL) == "Scotland_mark")] <-"age_chi"

#save out #Age_Response_rate_OVERALL"
write.xlsx(Age_Response_rate_OVERALL,paste0(output_path,"technical_report/age_query_euan_050924.xlsx.xlsx"))