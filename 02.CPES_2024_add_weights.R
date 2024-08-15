# Name of file: 02.CPES_2024_add_weights.R
# 
# Original author(s): Catriona Haddow 
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Syntax to add weights to 2024 survey data.  
# 
# Approximate run time: <1 min
# 
# Approximate memory usage: < MiB
#cancer_group_smr06
###################################################################################

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Inputs: 
#analysis_output_path,"validated_results.rds"

#Outputs: 
#analysis_output_path,"CPES_2024_weighting_group_information.xlsx"
#analysis_output_path,"responses_with_weights.rds"
#analysis_output_path,"sg_responses_with_weights.rds"

# Get prepared dataset that lists all cases (respondents and non-respondents).
validated_results <- readRDS(paste0(analysis_output_path,"validated_results.rds")) 
table(validated_results$tumour_group_text)
ls(validated_results)

#Step 1:  Calculate weights for each level of reporting.####
validated_results <- validated_results %>% 
  mutate(scotland = "Scotland") %>% #for calculating national weights
  mutate(age_band_2 = two_age_bands(age_chi),
         age_band_3 = three_age_bands(age_chi),
         age_band_4 = four_age_bands(age_chi),
         age_band_5 = five_age_bands(age_chi),
         age_band_7 = seven_age_bands(age_chi))
table(validated_results$report_area)

make_weights <- function(report_area,age_band) {
  weight <- validated_results %>%
    mutate(record = 1) %>% 
    group_by(report_area = as.character({{report_area}}),tumour_group_text,age_band = {{age_band}},sex) %>%
    summarise(n_respondents_weightgrp=sum(record[responded ==1],na.rm = TRUE), 
              n_non_respondents_weightgrp=sum(record[responded == 2],na.rm = TRUE), 
              n_population_weightgrp=sum(record),.groups = "keep") %>% 
    mutate(weight = n_population_weightgrp / n_respondents_weightgrp)
  return(weight)}

nat_weights <- make_weights(report_area = scotland,age_band = age_band_3) %>% mutate(level = "National")
network_tx_weights <- make_weights(report_area = network_of_tx,age_band = age_band_3)%>% mutate(level = "Region - Treatment")
network_of_residence_tx_weights <- make_weights(report_area = network_of_residence_tx,age_band = age_band_3)%>% mutate(level = "Region - Residence")
board_tx_weights <- make_weights(report_area = board_of_tx,age_band = age_band_2)%>% mutate(level = "NHS Board - Treatment")
board_of_residence_tx_weights <- make_weights(report_area = board_of_residence_tx,age_band = age_band_2)%>% mutate(level = "NHS Board - Residence")
board_of_residence2_tx_weights <- make_weights(report_area = board_of_residence2,age_band = age_band_2)%>% mutate(level = "NHS Board - Residence (alt)")

#Step 2:  Save out the weights for each level of reporting.####
weights_summary <- rbind(nat_weights,network_tx_weights,network_of_residence_tx_weights,board_tx_weights,board_of_residence_tx_weights,board_of_residence2_tx_weights) %>% 
  group_by(level = factor(level,levels = c("National","Region - Treatment","Region - Residence","NHS Board - Treatment","NHS Board - Residence","NHS Board - Residence (alt)"))) %>% 
  summarise(age_band_count = length(unique(age_band)),
            tumour_group_count = length(unique(tumour_group_text)),
            categories_no_response = length(report_area[n_respondents_weightgrp == 0 & n_population_weightgrp != 0]),
            population_categories_no_response = sum(n_population_weightgrp[n_respondents_weightgrp == 0 & n_population_weightgrp != 0]))

list_of_datasets <- list("Blank Category Summary" = weights_summary, "National" = nat_weights ,
                         "Network - TX" = network_tx_weights,"Network - Res" = network_of_residence_tx_weights,
                         "Board - TX" = board_tx_weights,"Board - Res" = board_of_residence_tx_weights, "Board - Res (alt)" = board_of_residence2_tx_weights)

write.xlsx(list_of_datasets,paste0(analysis_output_path,"CPES_2024_weighting_group_information.xlsx"))


#Step 3:  Add and trim the weights for each level of reporting.####
responses <- validated_results %>% 
  filter(responsecode == 1) # Remove non-responses from the dataset.

#define the weights function
add_weights <- function(weight_file) {
  responses <- responses %>%
    left_join(weight_file,by = c(as.character("report_area"),"age_band","tumour_group_text","sex")) %>% 
    mutate(weight_cap = mean(weight)+2*sd(weight), #Aggregate the file to calculate the mean weight, to be used in capping the weights by 2 standard deviations
           wt_final = if_else(weight > weight_cap,weight_cap,weight))}

#apply the function at each level
responses <- responses %>%  mutate(report_area = scotland,age_band = age_band_3) 
responses <-  add_weights(weight_file = nat_weights) %>% 
  rename(nat_wt = wt_final) %>% select(-weight_cap,-weight,-matches("weightgrp|level"))

responses <- responses %>%  mutate(report_area = network_of_tx,age_band = age_band_3) 
responses <-  add_weights(weight_file = network_tx_weights) %>% 
  rename(nett_wt = wt_final) %>% select(-weight_cap,-weight,-matches("weightgrp|level"))

responses <- responses %>%  mutate(report_area = network_of_residence_tx,age_band = age_band_3) 
responses <-  add_weights(weight_file = network_of_residence_tx_weights) %>% 
  rename(netr_wt = wt_final) %>% select(-weight_cap,-weight,-matches("weightgrp|level"))

responses <- responses %>%  mutate(report_area = board_of_tx,age_band = age_band_2) 
responses <- add_weights(weight_file = board_tx_weights) %>% 
  rename(hbt_wt = wt_final) %>% select(-weight_cap,-weight,-matches("weightgrp|level"))

responses <- responses %>%  mutate(report_area = board_of_residence_tx,age_band = age_band_2) 
responses <- add_weights(weight_file = board_of_residence_tx_weights) %>% 
  rename(hbr_wt = wt_final) %>% select(-weight_cap,-weight,-matches("weightgrp|level"))

responses <- responses %>%  mutate(report_area = board_of_residence2,age_band = age_band_2) 
responses <- add_weights(weight_file = board_of_residence2_tx_weights) %>% 
  rename(hbr2_wt = wt_final) %>% select(-weight_cap,-weight,-matches("weightgrp|level"))

ls(responses)

#Step 4:  Save respondent dataset with weights for each level.####
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"responses_with_weights.rds")) 
identical(hist.file,responses) 

saveRDS(responses,paste0(analysis_output_path,"responses_with_weights.rds"))

#Step 5:  Save SG copy of respondent dataset with weights for each level and the variables that they are allowed to have.####
phs_responses_file <- readRDS(paste0(analysis_output_path,"responses_with_weights.rds"))
ls(phs_responses_file)
sg_responses_file <- phs_responses_file %>%
  select (patientid_sg,responsecode,responsesubcode,responsedatetime,
          #questionnaire responses
          (starts_with('q')), -qh_psid,
          all_of(sg_variables)) %>%
  rename(network_of_treatment_code_smr01 = network_of_tx) %>%
  rename(network_of_residence_code_smr01 = network_of_residence_tx) %>%
  rename(smr01_hbtreat_code_keydate = board_of_tx) %>%
  rename(smr01_hbres_code_keydate = board_of_residence_tx)

#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"sg_responses_with_weights.rds")) 
all.equal(hist.file,sg_responses_file) 

saveRDS(sg_responses_file,paste0(analysis_output_path,"sg_responses_with_weights.rds"))
