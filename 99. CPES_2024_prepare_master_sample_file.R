# Name of file: 99.CPES_2024_prepare_master_sample_file.R
# 
# Original author(s): Catriona Haddow 
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Reads in master sample file created as part of sampling process, and add addition / grouping variables
# 
# Approximate run time: 2 min
# 
# Approximate memory usage: 685 MiB
###################################################################################
source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Read in master sample file
master_sample_file <- readRDS(paste0(data_path,"sample/2024.03.20_master_CPES_list.rds")) %>% 
  rename_with(tolower) %>% 
  mutate(inmastersamplefile = 1)

#Read in and match on NHS Lothian location data (\\~Data~ SMR01-SMR06 Extract\SCIR24001_SCPES_Lothian_v1.xlsx) to flag ECC patients
lothian_file <- read.xlsx(paste0(data_path,"SMR01-SMR06 Extract/SCIR24001_SCPES_Lothian_v1.xlsx")) #1135
lothian_file <- lothian_file %>%
  rename_with(tolower) %>% 
  select(uniquepatientsurveyid,ecc_flag) 
  
#add to master sample file. smr01_location2: S116A=Western General; S116B=Edinburgh Cancer Centre.
master_sample_file <- master_sample_file %>%
  left_join(lothian_file, by = c("uniquepatientsurveyid" = "uniquepatientsurveyid")) %>% 
  mutate(smr01_location2 = case_when(smr01_location == "S116H" & ecc_flag == "Y" ~ "S116B",
                                     smr01_location == "S116H" & ecc_flag == "N" ~ "S116A",
                                     TRUE ~ smr01_location2),
         smr01_locname = case_when(smr01_location == "S116H" & ecc_flag == "Y" ~ "Edinburgh Cancer Centre",
                                   TRUE ~ smr01_locname)) %>% 
  select(-ecc_flag)

#Read in chili_chi_extractfile of 18/01/24 to create Age and Age group (16-64, 65-74, 75+) based on (CHI) (\\~Data~ chili team file\CHI22071_CPES_CHI_EXTRACT.csv)
chili_data <- read.csv(paste0(data_path,"chili team file/CHI22071_CPES_CHI_EXTRACT.csv"))
chili_data <- chili_data %>%
  select(uniquepatientsurveyid,DATE_OF_BIRTH) %>%
  mutate(dob = as.Date(DATE_OF_BIRTH,origin = "1899-12-30")) %>%
  mutate(age_chi = phsmethods::age_calculate(dob,census_date)) %>%
  mutate(age_group_chi = three_age_bands(age_chi)) 

table(chili_data$age_group_chi, useNA = c("always"))
chili_data <- chili_data %>%
  select(uniquepatientsurveyid,age_chi,age_group_chi)

#Match chili data onto master_sample_file
master_sample_file <- master_sample_file %>%
  left_join(chili_data, by = c("uniquepatientsurveyid" = "uniquepatientsurveyid"))

#Read in and match on HSCP and CA based on smr01 postcode at time of sample taken using \\stats\cl-out\lookups\Unicode\Geography\Scottish Postcode Directory\Scottish_Postcode_Directory_2023_2.rds (05/09/23)
#Scottish Postcode Directory\Scottish_Postcode_Directory_2024_1.rds Date = 07/02/24 (After sample taken)
hscp_file <- readRDS(paste0(data_path,"SMR01-SMR06 Extract/CPES_HSCP_CA_based_on_smr01_addendum_file.rds"))
master_sample_file <- master_sample_file %>%
  left_join(hscp_file, by = c("uniquepatientsurveyid" = "patientid" ,"smr01_dr_postcode" = "smr01_dr_postcode"))
table(master_sample_file$pc7flag) 

#read in tumour groups for weighting
CPES_cancer_group <- read_excel(paste0(lookup_path,"CPES_2024_Cancer_Group_Lookup.xlsx")) %>% 
  rename_with(tolower) 

#Add tumour groups for weighting. These may need to be updated. Brain and sarcoma to be grouped together with "Other". They were combined for weighting purposes.

master_sample_file <- master_sample_file %>% 
  mutate(tumour_group_2_smr06 = case_when(tumour_group_smr06 == 99 ~ 14,# Where tumour group can't be established, set the tumour group to 14 (Cancer Group Unknown).
                                    tumour_group_smr06 %in% c(1,9) ~ 13,#combines the least common tumour types (brain and sarcoma) into "Other"
                                    TRUE ~ tumour_group_smr06),
         tumour_group_text_smr06 = case_when(tumour_group_smr06 == 99 ~ "Tumour Group Unknown",# Where tumour group can't be established, set the tumour group to 14 (Cancer Group Unknown).
                                       tumour_group_smr06 %in% c(1,9) ~ "Other",#combines the least common tumour types (brain and sarcoma) into "Other"
                                       TRUE ~ tumour_group_text_smr06))

#prepare cancer groups for analysis grouping
CPES_cancer_group <- CPES_cancer_group %>% 
  rename("cancer_group_smr06" = "cancer_group") %>% 
  select(smr06_site10,cancer_group_smr06)

#Add cancer types for analysis grouping
master_sample_file <- master_sample_file %>% 
  left_join(CPES_cancer_group, by = c("smr06_site10"))  

table(master_sample_file$cancer_group_smr06,useNA = c("always"))
#Add name label for smr01_sex
table(master_sample_file$smr01_sex)
master_sample_file$smr01_sex_label <- "Initialise"
master_sample_file$smr01_sex_label[master_sample_file$smr01_sex == "1"] <- "Male"
master_sample_file$smr01_sex_label[master_sample_file$smr01_sex == "2"] <- "Female"
master_sample_file$smr01_sex_label[master_sample_file$smr01_sex == "0"] <- "Unknown"
master_sample_file <- master_sample_file %>%
  relocate(smr01_sex_label, .after = smr01_sex) #smr01_sex_label
table(master_sample_file$smr01_sex_label)

#Add on Board of Treatment name
board_file <- read.csv(paste0(data_path,"Lookup/SMRA.ANALYSIS.HEALTH_BOARD.csv"))
board_file <- board_file %>%
  select("GRO_HB9_2019" , "DESCRIPTION")
colnames(board_file)[which(names(board_file) == "DESCRIPTION")] <-"board_of_treatment"
master_sample_file <- left_join(master_sample_file, board_file, by = c("smr01_hbtreat_keydate" = "GRO_HB9_2019")) %>%
  relocate(board_of_treatment, .after = smr01_hbtreat_keydate) #relocate board_of_treatment
#Add on Board of Residence name
colnames(board_file)[which(names(board_file) == "board_of_treatment")] <-"board_of_residence"
master_sample_file <- left_join(master_sample_file, board_file, by = c("smr01_hbres_keydate" = "GRO_HB9_2019")) %>%
  relocate(board_of_residence, .after = smr01_hbres_keydate) #relocate board_of_residence

#Add on Method of first treatment description
table(master_sample_file$smr06_method_1st_detection)
master_sample_file$smr06_method_1st_detection_description <- "Initialise"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "1"] <- "Screening examination (routine cervical, breast, bowel screening in the absence of symptoms)"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "2"] <- "Incidental finding (on examination or at surgery for an unrelated reason)"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "3"] <- "Clinical presentation (with relevant symptoms)"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "4"] <- "Incidental finding at autopsy"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "5"] <- "Interval cancer (this is when a patient presents with a breast, colorectal or cervical tumour after a negative screening but before their next screening is due)"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "8"] <- "Other"
master_sample_file$smr06_method_1st_detection_description[master_sample_file$smr06_method_1st_detection == "9"] <- "Not known"
table(master_sample_file$smr06_method_1st_detection_description)
master_sample_file <- master_sample_file %>%
  relocate(smr06_method_1st_detection_description, .after = smr06_method_1st_detection) 
  
#Save out updated version of Master Sample File for completeness
#check if the same as before
hist.file <- readRDS(paste0(data_path,"sample/2024.07.15_finalised_master_CPES_list.rds")) 
all.equal(hist.file,master_sample_file) 
#Save out reformatted data
saveRDS(master_sample_file, paste0(data_path,"sample/2024.07.17_finalised_master_CPES_list.rds"))
ls(master_sample_file)