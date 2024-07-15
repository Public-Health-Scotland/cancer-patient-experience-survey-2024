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

###################################################################################

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Inputs: 
#analysis_output_path,"validated_results.rds"

#Outputs: 
#analysis_output_path,"CPES_2024_weighting_group_information.xlsx"
#analysis_output_path,"responses_with_weights.rds"

# Get prepared dataset that lists all cases (respondents and non-respondents).
validated_results <- readRDS(paste0(analysis_output_path,"validated_results.rds")) 

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

#Step 2:  Save out the weights for each level of reporting.####
weights_summary <- rbind(nat_weights,network_tx_weights,network_of_residence_tx_weights,board_tx_weights,board_of_residence_tx_weights) %>% 
  group_by(level = factor(level,levels = c("National","Region - Treatment","Region - Residence","NHS Board - Treatment","NHS Board - Residence"))) %>% 
  summarise(age_band_count = length(unique(age_band)),
            tumour_group_count = length(unique(tumour_group_text)),
            categories_no_response = length(report_area[n_respondents_weightgrp == 0 & n_population_weightgrp != 0]),
            population_categories_no_response = sum(n_population_weightgrp[n_respondents_weightgrp == 0 & n_population_weightgrp != 0]))

list_of_datasets <- list("Blank Category Summary" = weights_summary, "National" = nat_weights ,
                         "Network - TX" = network_tx_weights,"Network - Res" = network_of_residence_tx_weights,
                         "Board - TX" = board_tx_weights,"Board - Res" = board_of_residence_tx_weights)

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
          #1. Getting diagnosed 
          q01,q02,q03,q04,q05,q06,  
          #2.Finding out you had cancer  
          q07a,q07b,q07c,q07d,q07e,q07f,q08,q09,q10, 
          #3. Deciding the best treatment for you
          q11,q12,q13,q14,q15, 
          #4. Operations, Radiotherapy and Chemotherapy  
          q16,q17,q18,q19,q20,q21,q22,q23,q24, 
          q25a,q25b,q25c,q25d,q25e,q25f,q26,q27,q28,q29,q30,q31,  #5.Hospital Care                                   
          q32a,q32b,q32c,q32d,q32e,q32f,q32g,q33,q34a,q34b,q34c,q34d,q34e,q34f,q34g, #6. Wider Support                                 
          q35,q36,q37,q38,q39,q40,q41, #7. Information and Other Support
          #8. Your overall experience
          q42,q43,q44,q45,q46,q46atoi,q46a,q46b,q46c,q46d,q46e,q46f,q46g,q46h,q46i,q46j,                                                                        
          q47a01, q47a01_1, q47a01_2, q47a01_3,
          q47a02, q47a02_1, q47a02_2, q47a02_3,
          q47a03, q47a03_1, q47a03_2, q47a03_3,                            
          q47a04, q47a04_1, q47a04_2, q47a04_3,                              
          q47a05, q47a05_1, q47a05_2, q47a05_3,                   
          q47a06, q47a06_1, q47a06_2, q47a06_3,                            
          q47a07, q47a07_1, q47a07_2, q47a07_3,                              
          q47b01, q47b01_1, q47b01_2, q47b01_3, q47b01_4, q47b01_5,                   
          q47b02, q47b02_1, q47b02_2, q47b02_3, q47b02_4, q47b02_5,   
          q47b03, q47b03_1, q47b03_2, q47b03_3, q47b03_4, q47b03_5,                               
          q47b04, q47b04_1, q47b04_2, q47b04_3, q47b04_4, q47b04_5,
          q47b05, q47b05_1, q47b05_2, q47b05_3, q47b05_4, q47b05_5,
          q47b06, q47b06_1, q47b06_2, q47b06_3, q47b06_4, q47b06_5,
          q47b07, q47b07_1, q47b07_2, q47b07_3, q47b07_4, q47b07_5,                               
          q48,
          q49,                                   
          q50a, q50a_1, q50a_2, q50a_3, q50a_4, q50a_5, q50a_6,
          q50b, q50b_1, q50b_2, q50b_3, q50b_4, q50b_5, q50b_6,   
          q50c, q50c_1, q50c_2, q50c_3, q50c_4, q50c_5, q50c_6,                                
          q50d, q50d_1, q50d_2, q50d_3, q50d_4, q50d_5, q50d_6,                                 
          q50e, q50e_1, q50e_2, q50e_3, q50e_4, q50e_5, q50e_6, 
          q51a, q51a_1, q51a_2, q51a_3, q51a_4, q51a_5, q51a_6,                                
          q51b, q51b_1, q51b_2, q51b_3, q51b_4, q51b_5, q51b_6,  
          q51c, q51c_1, q51c_2, q51c_3, q51c_4, q51c_5, q51c_6, 
          q51d, q51d_1, q51d_2, q51d_3, q51d_4, q51d_5, q51d_6,                                
          q51e, q51e_1, q51e_2, q51e_3, q51e_4, q51e_5, q51e_6, 
          q52,
          q53, 
          q54,  
          q55,q55_ave, 
          #9. Other comments
          q56a,q56b,q56c,q57,q58,q59,q60,q61,q61a,q61b,q61c,q61d,q61e,q61f,q61g,q61h,q61i,q61j,q61k,q62,                    
          sex,smr01_sex_label,
          age_group_chi,
          simd2020v2_sc_quintile_smr01,
          ur6_2020_smr01,ur6_2020_name_smr01,
          hscp2019,hscp2019name,
          board_of_tx,board_of_treatment,
          board_of_residence_tx,board_of_residence,
          network_of_tx,network_of_residence_tx,
          smr01_location,location_2,smr01_locname,
          #Cancer centre in here,
          #Cancer group in here,
          smr06_stage,
          smr06_method_1st_detection,smr06_method_1st_detection_description,
          nat_wt,
          nett_wt,
          netr_wt,
          hbt_wt,
          hbr_wt)
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"sg_responses_with_weights.rds")) 
identical(hist.file,sg_responses_file) 

saveRDS(sg_responses_file,paste0(analysis_output_path,"sg_responses_with_weights.rds"))