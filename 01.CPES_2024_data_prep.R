# Name of file: 01.CPES_2024_data_prep.R
# 
# Original author(s): Catriona Haddow 
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Reads in patient level data for CPES 2024 Survey and applies and checks validation rules
# 
# Approximate run time: 2 min
# 
# Approximate memory usage:  GiB
###################################################################################

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Inputs: 
#data_path,"Results from Contractor/SCC24_FINAL_Data_V1.xlsx"
#data_path,"sample/2024.06.13_finalised_master_CPES_list.rds"

#Outputs: 
#data_path,"Results from Contractor/Final_unrouted_data.rds"
#analysis_output_path,analysis_output,"anonymised_unvalidated_response_data_with_patient_data_for_SG.rds"
#analysis_output/"anonymised_unvalidated_response_data_with_patient_data_for_SG.xlsx"
#rules_summary, file = "output/analysis_output/rules_summary.xlsx"
#analysis_output_path,"validated_results.rds"

###################################################################################

#Step 1: Read in contractor data and master sample file####
#Read in contractor data
#opening final set of unrouted results received from contractor
contractor_data <- read.xlsx(paste0(data_path,"Results from Contractor/SCC24_FINAL_Data_V1_2.xlsx"),sheet = "UNROUTED")
sum(duplicated(contractor_data$PatientRecordNumber))
contractor_data2 <- read.xlsx(paste0(data_path,"Results from Contractor/SCC24_FINAL_Data_V1_2.xlsx"),sheet = "Non-Completes")
sum(duplicated(contractor_data2$PatientRecordNumber))
contractor_data <- rbind(contractor_data,contractor_data2)
rm(contractor_data2)

##Read in variables required from master sample file. This should cover both those that SG require, and those needed for PHS further analyses. ####
#Retain smr01_sex,age_group_chi,simd2020v2_sc_quintile_smr01,ur6_2020_smr01,ur6_2020_name_smr01,hscp2019,hscp2019name,smr01_hbtreat_keydate,smr01_hbres_keydate,
#smr01_location,smr01_location2,smr01_locname,full_site_name,ECC_flag,smr06_stage,smr06_method_1st_detection

master_sample_file <- readRDS(paste0(data_path,"sample/2024.07.17_finalised_master_CPES_list.rds")) 
ls(master_sample_file)
master_sample_file <- master_sample_file %>%
  select(uniquepatientsurveyid,smr01_sex,smr01_sex_label,age_chi,age_group_chi,simd2020v2_sc_quintile_smr01,ur6_2020_smr01,ur6_2020_name_smr01,
         hscp2019,hscp2019name,smr01_hbtreat_keydate,board_of_treatment,smr01_hbres_keydate,board_of_residence,network_of_residence_smr01,network_of_treatment_smr01,
         smr01_location,smr01_location2,smr01_locname,full_site_name,smr06_stage,smr06_method_1st_detection,smr06_method_1st_detection_description,
         tumour_group_2_smr06,tumour_group_text_smr06,cancer_group_smr06,iqvia_exclude,additional_exclusion_flag)
table(master_sample_file$tumour_group_text_smr06)
# Match on population data from master sample file.
contractor_data <- contractor_data %>% 
  left_join(master_sample_file, by = c("PatientRecordNumber" = "uniquepatientsurveyid"))
table(contractor_data$inmastersamplefile,useNA = c("always"))
#check duplicates & number of records
sum(duplicated(contractor_data$Participant.ID))
sum(duplicated(contractor_data$uniquepatientsurveyid))
nrow(contractor_data)  
table(contractor_data$iqvia_exclude)
table(contractor_data$additional_exclusion_flag)

## Deaths: Were there any cases where: ####
# * 1) the patient died, but completed the survey prior to death? Yes: these cases will remain in the sample.
# * 2) the patient died, but this was not picked-up by NHSCR / CHILI? 
#      (i.e. Notified by family and after last mailout day) Yes: these cases will be removed from the sample.
table(contractor_data$iqvia_exclude,contractor_data$Response.Code,useNA = c("always"))
## Code into respondents / non-respondents and remove patients who were excluded from the sample before final analysis. ####
# * Recode Response.Code into patients who responded to the survey and those who did not. 
# * Also flag patients who should be excluded from the sample (patients who had moved, were deceased, were ineligible or were distressed).
contractor_data <- contractor_data %>% 
  mutate(Responded = case_when(Response.Code == 1~ 1, #Responded
                               Response.Code %in% c(4,6,NA) ~ 2, #Did not respond
                               Response.Code %in% c(2, 3, 5, 7) ~ 3, #Exclude
                               TRUE ~ 9))
table(contractor_data$Responded,contractor_data$Response.Code,useNA = c("always"))
table(contractor_data$Responded,useNA = c("always"))
contractor_data <- contractor_data %>% 
  filter(Responded != 3)

#check variable names and classes
ls(contractor_data)
sapply(contractor_data, class)

#Rename Variables as necessary: 
contractor_data <- contractor_data %>% 
  rename_with(tolower) %>% #all lower case
  rename(patientid = patientrecordnumber) %>% 
  mutate(response.date.time = as.Date(response.date.time,origin = "1899-12-30")) %>% #reformat response date details
  rename(qh_psid = participant.id) %>%
  rename(responsecode = response.code) %>%  
  rename(responsesubcode = response.sub.code) %>%
  rename(responsedatetime = response.date.time) %>%
  rename(network_of_tx = network_of_treatment_smr01) %>% # for 02.CPES_2024_add_weights.R
  rename(network_of_residence_tx = network_of_residence_smr01) %>% # for 02.CPES_2024_add_weights.R
  rename(board_of_tx = smr01_hbtreat_keydate) %>% # for 02.CPES_2024_add_weights.R
  rename(board_of_residence_tx = smr01_hbres_keydate) %>% # for 02.CPES_2024_add_weights.R
  rename(tumour_group_text = tumour_group_text_smr06) %>% # for 02.CPES_2024_add_weights.R
  rename(sex = smr01_sex) %>% # for 02.CPES_2024_add_weights.R
  rename(location_2 = smr01_location2) # for 03.CPES_2024_create_responses_longer.R

## add patientid_SG ####
index <- c(rep(1:nrow(contractor_data)))
contractor_data <- contractor_data %>% 
  mutate(patientid_sg = paste0("Patsg",str_pad(index,6,c("left"),pad = "0"))) %>% 
  relocate(patientid_sg, .after = patientid) #relocate patientid_sg

#check if the same as before
hist.file <- readRDS(paste0(data_path,"Results from Contractor/Final_unrouted_data.rds")) 
all.equal(hist.file,contractor_data) 

##Save out reformatted data ####
saveRDS(contractor_data, paste0(data_path,"Results from Contractor/Final_unrouted_data.rds"))

##Create file to send on to SG on 17th June 2024 with unvalidated data results plus patient data that SG are allowed to hold. ####
##Also recreated to take account of inclusion of non-respondents. ####
sg_unrouted_data <- readRDS(paste0(data_path,"Results from Contractor/Final_unrouted_data.rds")) 

##Produce final unrouted data file for SG ####
sg_unrouted_data <- sg_unrouted_data %>%
  select(-qh_psid,-paper.questionnaire.id,-patientid,-smr01_location,location_2,-full_site_name,-age_chi,
         -network_of_residence_tx,-network_of_tx,
         -tumour_group_2_smr06,-tumour_group_text,-cancer_group_smr06,iqvia_exclude,additional_exclusion_flag) %>%
  relocate(patientid_sg, .before = responsecode) #relocate patientid_sg
hist.file <- readRDS(paste0(analysis_output_path,"anonymised_unvalidated_response_data_with_patient_data_for_SG.rds")) 
identical(hist.file,contractor_data) 

##Save out file for SG data ####
saveRDS(sg_unrouted_data, paste0(analysis_output_path,"anonymised_unvalidated_response_data_with_patient_data_for_SG.rds"))
write.xlsx(sg_unrouted_data, paste0(analysis_output_path,"anonymised_unvalidated_response_data_with_patient_data_for_SG.xlsx"))

#Step 2: Apply validation rules####
unrouted_data <- readRDS(paste0(data_path,"Results from Contractor/Final_unrouted_data.rds"))

#First, output the frequencies of all the question responses to a text file
sink(file = paste0(analysis_output_path,"prevalidation.txt"))  ## open sink connection
timestamp()
apply(unrouted_data %>% select(starts_with("q")), MARGIN=2, table,useNA = c("always"))
sink()  ## close it!


## Rule 1. Q2: If respondent did not talk to anyone at their GP, then set Q3: time to first appointment with HCP to blank####
tabyl(unrouted_data,q02,q03, useNA = c("always"))

# Apply routing rules to sub-routing question Q3.
#Rule 1: how many records are affected?
Rule01 <- sum(if_else(unrouted_data$q02 %in% c(5,NA) & unrouted_data$q03 %in% c(1,2,3),1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q03= case_when(q02 %in% c(5,NA) ~ NA, TRUE ~ q03))

tabyl(unrouted_data,q02,q03, useNA = c("always"))

##Rule 2. Q7. Tick all that apply. When you were first told that you had cancer, were you told in advance that you could bring a family member or friend with you?####
#a)	If respondent ticks 1 and 2 (yes and no) set both to blank 
tabyl(unrouted_data,q07a,q07b, useNA = c("always"))
Rule02a <- sum(unrouted_data$q07a == 1& unrouted_data$q07b == 1,na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q07a = case_when(q07a == 1 & q07b == 1 ~ NA,
                          TRUE ~ q07a),
         q07b = case_when(q07a == 1 & q07b == 1 ~ NA,
                          TRUE ~ q07b))
tabyl(unrouted_data,q07a,q07b, useNA = c("always"))

#b)	 If respondent ticks any of (1,2) (yes, no) and 5 (donÃ¢ÂÂt know) is also ticked, set 1,2 & 5 to blank
Rule02b <- sum(if_else(unrouted_data$q07e == 1 & (unrouted_data$q07a == 1 | unrouted_data$q07b == 1),1,0),na.rm = TRUE)

#Create temporary rule02b_flag
unrouted_data$rule02b_flag <- 0
unrouted_data <- unrouted_data %>%
  mutate(rule02b_flag = if_else(q07e == 1 & (q07a == 1 | q07b == 1),1,0))
#relocate rule02b_flag
unrouted_data <- unrouted_data %>% relocate(rule02b_flag, .before = q01)
table(unrouted_data$rule02b_flag)
tabyl(unrouted_data,rule02b_flag, useNA = c("always"))
table(unrouted_data$q07a)
table(unrouted_data$q07b)
table(unrouted_data$q07e)
tabyl(unrouted_data,q07a,q07e)
tabyl(unrouted_data,q07b,q07e)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q07a = case_when(rule02b_flag == 1 ~ NA,
                          TRUE ~ q07a),
         q07b = case_when(rule02b_flag == 1 ~ NA,
                          TRUE ~ q07b),
         q07e = case_when(rule02b_flag == 1 ~ NA,
                          TRUE ~ q07e))

table(unrouted_data$q07a)
table(unrouted_data$q07b)
table(unrouted_data$q07e)
tabyl(unrouted_data,q07a,q07e)
tabyl(unrouted_data,q07b,q07e)
#Remove temporary rule02b_flag
unrouted_data$rule02b_flag <- NULL

## Rule 3: Q16: Have you had an operation, such as removal of a tumour or lump, for your cancer?####
# Do we want to reverse code? Previous CPES surveys were. i.e. recode Q16 based on routed questions having a response other than Ã¢ÂÂdonÃ¢ÂÂt know / canÃ¢ÂÂt rememberÃ¢ÂÂ.
# a) If Q16 is blank, and Q17 is in (1,2,3,4,5) or Q18 is in (1,2,3,4) or Q19 in (1,2) or Q20 is in (1,2) then set Q16 to 1 (Yes)
# Or just
# b) If Q16 ^1 (Yes), then set Q17 to Q20 to blank

q17toq20 <- c("q17","q18","q19","q20")
lapply(unrouted_data[q17toq20],table,unrouted_data$q16,useNA = c("always")) #values before applying rule
table(unrouted_data$q16,useNA = c("always"))
#Rule 3a: In how many cases did respondent fail to answer Q16, but gave a substantive answer to Q17-Q20?
Rule03a <- sum(if_else(is.na(unrouted_data$q16) & 
                         (unrouted_data$q17 %in% c(1,2,3,4,5)|unrouted_data$q18 %in% c(1,2,3,4)|
                            unrouted_data$q19 %in% c(1,2)|unrouted_data$q20 %in% c(1,2)),1,0),na.rm = TRUE)


unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q16 = case_when(is.na(q16) & 
                           (q17 %in% c(1,2,3,4,5)|q18 %in% c(1,2,3,4)|
                              q19 %in% c(1,2)|q20 %in% c(1,2)) ~ 1,
                         TRUE ~ q16))

lapply(unrouted_data[q17toq20],table,unrouted_data$q16,useNA = c("always")) #values after applying rule
table(unrouted_data$q16,useNA = c("always"))

#Now apply routing rules to sub-routing questions Q17-Q20.
#Rule 3b If respondent did not have an operation (Q16 != 1), set Q17-Q20 to "Not applicable" 

Rule03b <- sum(if_else(unrouted_data$q16 %in% c(2,NA) & rowSums(unrouted_data[q17toq20],na.rm = TRUE) !=0,1,0),na.rm = TRUE)

lapply(unrouted_data[q17toq20],table,unrouted_data$q16,useNA = c("always")) #values before applying rule

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(across(all_of(q17toq20),~ case_when(q16 %in% c(2,NA) ~ NA,TRUE ~ .)))

lapply(unrouted_data[q17toq20],table,unrouted_data$q16,useNA = c("always"))  #values after applying rule

##Rule 4.	Q21: Have you had radiotherapy treatment?####
#Rule 4 a)  If Q21 is blank or 3 (No) then set Q22 to blank
Rule04a <- sum(if_else(unrouted_data$q21 %in% c(3,NA) & unrouted_data$q22 %in% c(1,2,3),1,0),na.rm = TRUE)

tabyl(unrouted_data,q21,q22) #values before applying rule

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q22 = case_when(q21 %in% c(3,NA) & q22 %in% c(1,2,3) ~ NA,TRUE ~ q22))

tabyl(unrouted_data,q21,q22) #values after applying rule

# Check that radiotherapy occurred in one of the locations that provides radiotherapy.
#b)	If Q21 is 1 - had radiotherapy treatment at the hospital named on the survey letter and 
# hospital is not one that provides radiotherapy, then set Q21 to 2 Ã¢ÂÂ had radiotherapy at a different hospital to the one named
#CH note have temporarily add location S116H

radiotherapy_location <- if_else(unrouted_data$location_2 %in% c("G516A", "G516B", "T101H", "N101H", "S116A","S116B", "S116H",
                                                                 "H202H", "L106H"), 1,0)
table(unrouted_data$q21,radiotherapy_location,useNA = c("always"))

#which locations that do not provide radiotherapy are respondents saying they had radiotherapy treatment
table(unrouted_data$location_2[radiotherapy_location == 0 & unrouted_data$q21 == 1])

Rule04b <- sum(if_else(radiotherapy_location == 0 & unrouted_data$q21 == 1,1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>% 
  mutate(q21 = if_else(radiotherapy_location == 0 & q21 == 1,2,q21))

##Rule 5. Q23: Have you had chemotherapy treatment?####
# If respondent did not have chemo (Q23 = 3 or NA), set Q24 to blank.

tabyl(unrouted_data,q23)
tabyl(unrouted_data,q23,q24) #values before applying rule
Rule05 <- sum(if_else(unrouted_data$q23 %in% c(3,NA) & unrouted_data$q24 %in% c(1,2,3),1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q24 = case_when(q23 %in% c(3,NA) & q24 %in% c(1,2,3) ~ NA,TRUE ~ q24))

tabyl(unrouted_data,q23,q24) #values after applying rule

##Rule 6.	Q29: Were you given the name of a Clinical Nurse Specialist, or another named contact, who would support you through your treatment?####
#If respondent did not have a CNS / named contact or can't remember, set sub routing questions to blank.
#If Q29 is blank or (3,4) then set Q30 to Q31 to blank.

q30toq31 <- c("q30","q31")

Rule06 <- sum(if_else(unrouted_data$q29 %in% c(3,4,NA) & rowSums(unrouted_data[q30toq31]) != 0,1,0),na.rm = TRUE)

lapply(unrouted_data[q30toq31],table,unrouted_data$q29,useNA = c("always")) #values before applying rule

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(across(all_of(q30toq31),~ case_when(q29 %in%c(3,4,NA) ~ NA,TRUE ~ .)))

lapply(unrouted_data[q30toq31],table,unrouted_data$q29,useNA = c("always")) #values after applying rule

## Rule 7.	Q46: Which of the following difficulties did you experience when travelling to appointments relating to your cancer care? (tick all that apply)####
# If respondent ticks I had no difficulties in addition to substantive difficulties, then set I had no difficulties to blank
# If Q46j = 1 and any of Q46a to Q46i = 1 set Q46j to blank
q46 <- names(unrouted_data %>% select(starts_with("q46")))
q46atoi <- q46[!q46 =="q46j"]
apply(unrouted_data[q46], MARGIN=2, table,unrouted_data$q46j) #values before applying rule #values before applying rule
lapply(unrouted_data[q46atoi],table,unrouted_data$q46j,useNA = c("always")) #values after applying rule
lapply(unrouted_data[q46],table,useNA = c("always")) #values after applying rule

Rule07 <- sum(if_else(unrouted_data$q46j == 1 & rowSums(unrouted_data[q46atoi],na.rm = TRUE) != 0,1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q46atoi = rowSums(across(q46a:q46i),na.rm = TRUE)) %>% 
  mutate(q46j = if_else(q46j == 1 & q46atoi != 0,NA,q46j))

table(unrouted_data$q46atoi,unrouted_data$q46j,useNA = c("always"))
#Apply tick all that apply rule. Blanks should be set to "No" (2) unless none of the response options were ticked, in which case all response options should be set.

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q46 = if_else(if_any(all_of(q46), ~ !is.na(.x)),1,0),
         across(all_of(q46), function(x) if_else(q46 == 1, replace_na(x,2),x)))

lapply(unrouted_data[q46],table,useNA = c("always")) #values after applying rule
table(unrouted_data$q46,useNA = c("always"))

#Split Q47 into separate variables. These should be 1 (Yes) when the value is found, NA if the mother variable is NA, and 2 (No) otherwise
unrouted_data <- unrouted_data %>%
  mutate(across(starts_with("q47a"), function(x) case_when(grepl("1",x) == TRUE~ 1,
                                                           !is.na(x) ~ 2,
                                                           is.na(x) ~ NA), 
                .names = "{col}_1"),
         across(starts_with("q47a") & !matches("_"), function(x) case_when(grepl("2",x) == TRUE~ 1,
                                                                           !is.na(x) ~ 2,
                                                                           is.na(x) ~ NA), 
                .names = "{col}_2"),
         across(starts_with("q47a") & !matches("_"), function(x) case_when(grepl("3",x) == TRUE~ 1,
                                                                           !is.na(x) ~ 2,
                                                                           is.na(x) ~ NA),
                .names = "{col}_3"),
         across(starts_with("q47b"), function(x) case_when(grepl("1",x) == TRUE~ 1,
                                                           !is.na(x) ~ 2,
                                                           is.na(x) ~ NA),
                .names = "{col}_1"),
         across(starts_with("q47b") & !matches("_"), function(x) case_when(grepl("2",x) == TRUE~ 1,
                                                                           !is.na(x) ~ 2,
                                                                           is.na(x) ~ NA),
                .names = "{col}_2"),
         across(starts_with("q47b") & !matches("_"), function(x) case_when(grepl("3",x) == TRUE~ 1,
                                                                           !is.na(x) ~ 2,
                                                                           is.na(x) ~ NA),
                .names = "{col}_3"),
         across(starts_with("q47b") & !matches("_"), function(x) case_when(grepl("4",x) == TRUE~ 1,
                                                                           !is.na(x) ~ 2,
                                                                           is.na(x) ~ NA),
                .names = "{col}_4"),
         across(starts_with("q47b") & !matches("_"), function(x) case_when(grepl("5",x) == TRUE~ 1,
                                                                           !is.na(x) ~ 2,
                                                                           is.na(x) ~ NA), 
                .names = "{col}_5"))
# 

unrouted_data <- unrouted_data %>%  #redefine original variables as either 1 if complete, or NA if not
  mutate(across(starts_with("q47") & !matches("_"), function(x) if_else(is.na(x),NA,1)))

lapply(unrouted_data%>%select(starts_with("q47")),table,useNA = c("always"))
## Rule 8. Q48: Were you able to bring a family member, friend or someone else to your appointments to support you when you wanted to?####
#If respondent always had someone/didnÃ¢ÂÂt want to bring someone, set sub routing question to blank.
#If Q48 is blank or (1,6) then set Q49 to blank.

tabyl(unrouted_data,q48,q49) #values after applying rule
Rule08 <- sum(if_else(unrouted_data$q48 %in% c(1,6,NA) & unrouted_data$q49 != 0,1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q49= case_when(q48 %in%c(1,6,NA) ~ NA,TRUE ~ q49))

tabyl(unrouted_data,q48,q49) #values after applying rule

##Rule 9.	Q50: How did the health and care team communicate with you at each stage? (tick all that apply)####
q50 <- names(unrouted_data %>% select(starts_with("q50")))
# Rule 9a.	Q50: If respondent ticks canÃ¢ÂÂt remember in addition to any other communication method (including not applicable), then set canÃ¢ÂÂt remember to blank
# If Q50_6 = 1 and any of Q50_(1 to 5) = 1 set Q50_6  to blank (xx records)

lapply(unrouted_data[q50],table) #values before applying rule

Rule09aa <- sum(if_else(str_detect(unrouted_data$q50a,"6") & 
                          str_detect(unrouted_data$q50a,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule09ab <- sum(if_else(str_detect(unrouted_data$q50b,"6") & 
                          str_detect(unrouted_data$q50b,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule09ac <- sum(if_else(str_detect(unrouted_data$q50c,"6") & 
                          str_detect(unrouted_data$q50c,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule09ad <- sum(if_else(str_detect(unrouted_data$q50d,"6") & 
                          str_detect(unrouted_data$q50d,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule09ae <- sum(if_else(str_detect(unrouted_data$q50e,"6") & 
                          str_detect(unrouted_data$q50e,"1|2|3|4|5"),1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rules 9a
  mutate(across(starts_with("q50"), function(x) if_else(str_detect(x,"6") & str_detect(x,"1|2|3|4|5"),str_replace(x,"6",""),x)))
lapply(unrouted_data[q50],table) #values after applying rule

# Rule 9b.	Q50: If respondent ticks not applicable in addition to substantive communication methods, then set not applicable to blank

lapply(unrouted_data[q50],table) #values before applying rule

Rule09ba <- sum(if_else(str_detect(unrouted_data$q50a,"5") & 
                          str_detect(unrouted_data$q50a,"1|2|3|4"),1,0),na.rm = TRUE)
Rule09bb <- sum(if_else(str_detect(unrouted_data$q50b,"5") & 
                          str_detect(unrouted_data$q50b,"1|2|3|4"),1,0),na.rm = TRUE)
Rule09bc <- sum(if_else(str_detect(unrouted_data$q50c,"5") & 
                          str_detect(unrouted_data$q50c,"1|2|3|4"),1,0),na.rm = TRUE)
Rule09bd <- sum(if_else(str_detect(unrouted_data$q50d,"5") & 
                          str_detect(unrouted_data$q50d,"1|2|3|4"),1,0),na.rm = TRUE)
Rule09be <- sum(if_else(str_detect(unrouted_data$q50e,"5") & 
                          str_detect(unrouted_data$q50e,"1|2|3|4"),1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rules 9b
  mutate(across(starts_with("q50"), function(x) if_else(str_detect(x,"5") & str_detect(x,"1|2|3|4"),
                                                        str_replace(x,"5",""),x)))
lapply(unrouted_data[q50],table) #values after applying rule
#Split Q50 into separate variables. These should be 1 (Yes) when the value is found, NA if the mother variable is NA, and 2 (No) otherwise
unrouted_data <- unrouted_data %>%
  mutate(across(starts_with("q50"), function(x) case_when(str_detect(x,"1") ~ 1,
                                                          !is.na(x) ~ 2,
                                                          is.na(x) ~ NA), 
                .names = "{col}_1"),
         across(starts_with("q50") & !matches("_"), function(x) case_when(str_detect(x,"2")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA), 
                .names = "{col}_2"),
         across(starts_with("q50") & !matches("_"), function(x) case_when(str_detect(x,"3")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA),
                .names = "{col}_3"),
         across(starts_with("q50") & !matches("_"), function(x) case_when(str_detect(x,"4")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA),
                .names = "{col}_4"),
         across(starts_with("q50") & !matches("_"), function(x) case_when(str_detect(x,"5")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA), 
                .names = "{col}_5"),
         across(starts_with("q50") & !matches("_"), function(x) case_when(str_detect(x,"6")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA), 
                .names = "{col}_6"))



unrouted_data <- unrouted_data %>%  #redefine original variables as either 1 if complete, or NA if not
  mutate(across(starts_with("q50") & !matches("_"), function(x) if_else(is.na(x),NA,1)))

lapply(unrouted_data %>% select(starts_with("q50")),table,useNA = c("always")) #values after applying rule

##Rule 10.	Q51: What would have been your preferred method of communication for each of these stages? (tick all that apply)####

# Rule 10a.	Q51: If respondent ticks canÃ¢ÂÂt remember in addition to any other communication method (including not applicable), then set canÃ¢ÂÂt remember to blank
# If Q51_6 = 1 and any of Q51_(1 to 5) = 1 set Q51_6  to blank (xx records)

lapply(unrouted_data %>% select(starts_with("q51")),table,useNA = c("always"))  #values before applying rule

Rule10aa <- sum(if_else(str_detect(unrouted_data$q51a,"6") & 
                          str_detect(unrouted_data$q51a,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule10ab <- sum(if_else(str_detect(unrouted_data$q51b,"6") & 
                          str_detect(unrouted_data$q51b,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule10ac <- sum(if_else(str_detect(unrouted_data$q51c,"6") & 
                          str_detect(unrouted_data$q51c,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule10ad <- sum(if_else(str_detect(unrouted_data$q51d,"6") & 
                          str_detect(unrouted_data$q51d,"1|2|3|4|5"),1,0),na.rm = TRUE)
Rule10ae <- sum(if_else(str_detect(unrouted_data$q51e,"6") & 
                          str_detect(unrouted_data$q51e,"1|2|3|4|5"),1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rules 10a
  mutate(across(starts_with("q51"), function(x) if_else(str_detect(x,"6") & str_detect(x,"1|2|3|4|5"),str_replace(x,"6",""),x)))
lapply(unrouted_data%>%select(starts_with("q51")),table) #values after applying rule

# Rule 10b.	q51: If respondent ticks not applicable in addition to substantive communication methods, then set not applicable to blank

Rule10ba <- sum(if_else(str_detect(unrouted_data$q51a,"5") & 
                          str_detect(unrouted_data$q51a,"1|2|3|4"),1,0),na.rm = TRUE)
Rule10bb <- sum(if_else(str_detect(unrouted_data$q51b,"5") & 
                          str_detect(unrouted_data$q51b,"1|2|3|4"),1,0),na.rm = TRUE)
Rule10bc <- sum(if_else(str_detect(unrouted_data$q51c,"5") & 
                          str_detect(unrouted_data$q51c,"1|2|3|4"),1,0),na.rm = TRUE)
Rule10bd <- sum(if_else(str_detect(unrouted_data$q51d,"5") & 
                          str_detect(unrouted_data$q51d,"1|2|3|4"),1,0),na.rm = TRUE)
Rule10be <- sum(if_else(str_detect(unrouted_data$q51e,"5") & 
                          str_detect(unrouted_data$q51e,"1|2|3|4"),1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rules 10b
  mutate(across(starts_with("q51"), function(x) if_else(str_detect(x,"5") & str_detect(x,"1|2|3|4"),
                                                        str_replace(x,"5",""),x)))
lapply(unrouted_data%>% select(starts_with("q51")),table) #values after applying rule

#Split q51 into separate variables. These should be 1 (Yes) when the value is found, NA if the mother variable is NA, and 2 (No) otherwise
unrouted_data <- unrouted_data %>%
  mutate(across(starts_with("q51"), function(x) case_when(str_detect(x,"1") ~ 1,
                                                          !is.na(x) ~ 2,
                                                          is.na(x) ~ NA), 
                .names = "{col}_1"),
         across(starts_with("q51") & !matches("_"), function(x) case_when(str_detect(x,"2")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA), 
                .names = "{col}_2"),
         across(starts_with("q51") & !matches("_"), function(x) case_when(str_detect(x,"3")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA),
                .names = "{col}_3"),
         across(starts_with("q51") & !matches("_"), function(x) case_when(str_detect(x,"4")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA),
                .names = "{col}_4"),
         across(starts_with("q51") & !matches("_"), function(x) case_when(str_detect(x,"5")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA), 
                .names = "{col}_5"),
         across(starts_with("q51") & !matches("_"), function(x) case_when(str_detect(x,"6")~ 1,
                                                                          !is.na(x) ~ 2,
                                                                          is.na(x) ~ NA), 
                .names = "{col}_6"))



unrouted_data <- unrouted_data %>%  #redefine original variables as either 1 if complete, or NA if not
  mutate(across(starts_with("q51") & !matches("_"), function(x) if_else(is.na(x),NA,1)))

lapply(unrouted_data %>% select(starts_with("q51")),table,useNA = c("always")) #values after applying rule

##Rule 11.	Q60: Do you have any physical or mental health conditions or illnesses lasting or expected to last 12 months or more?####
#If respondent had no conditions or prefers not to say, set sub routing questions to blank.
#If Q60 is blank or (2,3) set Q61 to Q62 to blank
q61toq62 <- names(unrouted_data %>% select(matches("q61|q62")))
lapply(unrouted_data %>% select(matches("q61|q62")), table,useNA = c("always")) #values before applying rule
Rule11 <- sum(if_else(unrouted_data$q60 %in% c(2,3,NA) & rowSums(unrouted_data[q61toq62],na.rm = TRUE) != 0,1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(across(all_of(q61toq62),~ case_when(q60 %in% c(2,3,NA) ~ NA,TRUE ~ .)))

lapply(unrouted_data %>% select(matches("q61|q62")), table,useNA = c("always")) #values after applying rule

## Rule 12.	Q61: Do any of these conditions or illnesses affect you in any of the following areas? (tick all that apply)####
# If respondent ticks any of (a to j) (substantive conditions) set k (prefer not to say) to blank 
# Blanks should be set to "No" (2) unless none of the response options were ticked, in which case all response options should be set.
# to "No response" (NA).

q61 <- names(unrouted_data %>% select(starts_with("q61")))
q61atoj <- q61[!q61 =="q61k"]
apply(unrouted_data[q61], MARGIN=2, table,unrouted_data$q61k) #values before applying rule 
lapply(unrouted_data[q61atoj],table,unrouted_data$q61k,useNA = c("always")) 
lapply(unrouted_data[q61],table,useNA = c("always")) #values after applying rule

Rule12 <- sum(if_else(unrouted_data$q61k == 1 & rowSums(unrouted_data[q61atoj],na.rm = TRUE) != 0,1,0),na.rm = TRUE)

unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q61atoj = rowSums(across(q61a:q61j),na.rm = TRUE)) %>% 
  mutate(q61k = if_else(q61k == 1 & q61atoj != 0,NA,q61k))

table(unrouted_data$q61atoj,unrouted_data$q61k,useNA = c("always"))
table(unrouted_data$q61k,useNA = c("always"))
unrouted_data <- unrouted_data %>%  select(-q61atoj)#drop extra variable

#Apply tick all that apply rule. Blanks should be set to "No" (2) unless none of the response options were ticked, in which case all response options should be set.
unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q61 = if_else(if_any(all_of(q61), ~ !is.na(.x)),1,0),
         across(all_of(q61), function(x) if_else(q61 == 1, replace_na(x,2),x)))  

lapply(unrouted_data[q61],table,useNA = c("always")) #values after applying rule
table(unrouted_data$q61,useNA = c("always"))

ls(unrouted_data)

#Final tidy. Add any other corrections here
table(unrouted_data$q55,useNA = c("always"))
unrouted_data <- unrouted_data %>%  #apply rule
  mutate(q55 = if_else(q55 == 98,NA,q55))
table(unrouted_data$q55,useNA = c("always"))


#This outputs the frequencies of all the question responses to a text file
sink(file = paste0(analysis_output_path,"postvalidation.txt"))  ## open sink connection
timestamp()
apply(unrouted_data %>% select(starts_with("q")), MARGIN=2, table)
sink()  ## close it!

#Step 3: Create rule summary####
rule_names <- ls(pattern = "^Rule")
rule_values <- c(as.numeric("Rule01"),"Rule02a",  "Rule02b",  "Rule03a",  "Rule03b",  "Rule04a",  "Rule04b",  "Rule05","Rule06",  
                 "Rule07","Rule08","Rule09aa", "Rule09ab", "Rule09ac", "Rule09ad", "Rule09ae", "Rule09ba", "Rule09bb",
                 "Rule09bc", "Rule09bd", "Rule09be", "Rule10aa", "Rule10ab", "Rule10ac", "Rule10ad", "Rule10ae", "Rule10ba",
                 "Rule10bb", "Rule10bc", "Rule10bd", "Rule10be", "Rule11", "Rule12")
rules_summary <- data.frame("Rules" = rule_names,"Records affected" = rule_values)

#save out rule summary
write.xlsx(rules_summary, paste0(analysis_output_path,"rules_summary.xlsx"))

#Step 4: Save out file.####
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"validated_results.rds")) 
all.equal(hist.file,unrouted_data)  

#Save out reformatted data
saveRDS(unrouted_data,paste0(analysis_output_path,"validated_results.rds"))