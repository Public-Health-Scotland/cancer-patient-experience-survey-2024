# Name of file: 03.CPES_2024_create_responses_longer.R
# 
# Original author(s): Catriona Haddow 
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Read in responses, restructure to be longer, and allocate cancer centre
# 
# Approximate run time: <1min
# 
# Approximate memory usage: 1.7GiB

#Inputs:
#analysis_output_path,"responses_with_weights.rds"
#lookup_path,"question_lookup.rds"

#Outputs: 
#analysis_output_path,"responses_longer.rds"

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#read in results data####
responses_with_weights <- readRDS(paste0(analysis_output_path,"responses_with_weights.rds")) 

ls(responses_with_weights)

#pivot longer####
responses_longer <- responses_with_weights %>%
  pivot_longer(all_of(questions),names_to = "question", values_to = "response_option")%>%
  select("patientid",location_2,all_of(report_areas),tumour_group_text,ends_with('_wt'),question,response_option) %>% 
  mutate(response_option = as.character(response_option))

responses_longer <- responses_longer %>% 
  select("patientid",all_of(report_areas),location_2,tumour_group_text,all_of(report_area_wt),question,response_option)

# Step 5: Allocate responses to main cancer centres. ####
# * Allocation Rules for Cancer Centre Reports.
# * Where treatment took place at one of the five main cancer centres, the response will be allocated to the associated cancer centre report. 
# * Where treatment did not take place at one of these centres, the response will not be allocated to a cancer centre report ("Not allocated"). 
# * The exception to the above rule(s) is for sub-routing questions where the patient was specifically asked if the treatment took place at the 
# * hospital indicated on the covering letter. For example:
# * Q6 (Did your first diagnostic test take place at the hospital named on the survey letter?) 
# * If the respondent answered "Yes" then the responses to Q4-Q5 should be allocated to the cancer centre report.
# * However, if the respondent answered "No" or "Don't know" or the answer is missing, then responses to these questions will not be 
# * allocated to a cancer centre report.
# * Need to set-up standard questions first and then adjust any sub-routing questions in individual batches.

# Allocate main centre of treatment.
# Only interested in the five main treatment centres - all other cases are set to "Not allocated".
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds"))

responses_longer <- responses_longer %>% 
  mutate(cancer_centre = case_when(location_2 %in% c("G516B","H202H","N101H","S116B","T101H") ~ location_2,
                                   TRUE ~ "Not allocated"))

table(responses_longer$question)
allocation_responses <- responses_longer %>% 
  ungroup() %>% 
  filter(question %in% allocation_questions) %>% 
  distinct(patientid,question,response_option) %>% 
  mutate(allocate_response = if_else(response_option == "1",1,0)) %>% 
  pivot_wider(names_from = question,id_cols = patientid,values_from = allocate_response,names_prefix = "allocation_",)

## Now adjust for the sub-routing questions based on the patient's response as to whether care took place at the hospital named on their covering letter. ####
# * If the patient did not answer 'Yes' to Q6 then the responses for Q4-5 will not be included in cancer centre report. 
# * If the patient did not answer 'Yes' to Q20 (or not applicable) then the responses for Q17-19 will not be included in cancer centre report. 
# * If the patient did not answer 'Yes' to Q21 then the responses for Q22 will not be included in cancer centre report. 
# * If the patient did not answer 'Yes' to Q23 then the responses for Q24 will not be included in cancer centre report. 
# * If the patient did not answer 'Yes' to Q27 then the responses for Q25, Q26 & Q28 will not be included in cancer centre report. 
responses_longer <- responses_longer %>% 
  left_join(allocation_responses, by = c("patientid")) %>% 
  left_join(question_lookup %>% 
              select(question,cancercentreallocation) %>% 
              distinct(), by = c("question"))%>% 
  mutate(cancer_centre = case_when(cancercentreallocation == "Q6_Resp" & allocation_q06 %in% c(NA,0) ~ "Not allocated",  #not sure I have treated 'NA' appropriately
                                   cancercentreallocation == "Q20_Resp" & allocation_q20 %in% c(NA,0) ~"Not allocated",
                                   cancercentreallocation == "Q21_Resp" & allocation_q21 %in% c(NA,0) ~"Not allocated",
                                   cancercentreallocation == "Q23_Resp" & allocation_q23 %in% c(NA,0) ~"Not allocated",
                                   cancercentreallocation == "Q27_Resp" & allocation_q27 %in% c(NA,0) ~"Not allocated",
                                   TRUE ~ cancer_centre)) %>% 
  select(-starts_with("allocation"))

##check if the same as before, then save new responses_longer.rds file ####
hist.file <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))
identical(hist.file,responses_longer)
file.remove(paste0(analysis_output_path,"responses_longer.rds")) # remove existing file 
saveRDS(responses_longer, paste0(analysis_output_path,"responses_longer.rds"))