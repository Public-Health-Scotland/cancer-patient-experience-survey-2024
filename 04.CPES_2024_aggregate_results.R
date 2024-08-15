# Name of file: 04.CPES_2024_aggregate_results.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  output analyses at all levels of reporting.
# 
# Approximate run time: 30 minutes
# 
# Approximate memory usage: 2 * CPU
# 
# *****************************************

#Inputs: 
#analysis_output_path,"responses_longer.rds"
#lookup_path,"SMRA.ANALYSIS.HEALTH_BOARD.csv"
#lookup_path,"question_lookup.rds"

#Outputs: 
#analysis_output_path,"output.xlsx"
#analysis_output_path,"output.rds"
#analysis_output_path,"cancer_group_output.xlsx"
#analysis_output_path,"cancer_group_output.rds"

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#read in responses longer data####
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

responses_longer <- responses_longer %>%   mutate(no_wt = 1) #add a no_wt variable for cancer centres

question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) #read in lookup 

#match question lookup onto responses longer to add on response_text_analysis only   
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

responses_longer <- responses_longer %>%   mutate(no_wt = 1) #add a no_wt variable for cancer centres

question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) #read in lookup 

#match question lookup onto responses longer to add on response_text_analysis only. Recode Exclude to NA for consistency
responses_longer <- responses_longer %>% 
  left_join(question_lookup %>% select(question,response_option,response_text_analysis),by = c("question","response_option")) %>% 
  mutate(response_text_analysis = case_match(response_text_analysis,"Exclude" ~ NA,.default = response_text_analysis))

#define survey design objects
options(survey.lonely.psu="remove")
responses_sdo_nat <- as_survey_design(responses_longer,ids = patientid,weights = nat_wt) 
responses_sdo_nett <- as_survey_design(responses_longer,ids = patientid,weights = nett_wt) 
responses_sdo_netr <- as_survey_design(responses_longer,ids = patientid,weights = netr_wt) 
responses_sdo_hbt <- as_survey_design(responses_longer,ids = patientid,weights = hbt_wt) 
responses_sdo_hbr <- as_survey_design(responses_longer,ids = patientid,weights = hbr_wt) 
responses_sdo_cc <- as_survey_design(responses_longer,ids = patientid,weights = no_wt) 

#define aggregate_responses function
aggregate_responses <- function(survey_design_object,report_areas) {
  plan(multisession) #tells furrr to parallel process across available CPUs
  survey_design_object %>% 
    filter(!is.na(response_text_analysis)) %>% 
    group_by("report_area" = {{report_areas}}) %>%
    group_split() |>
    future_map(\(report_area) report_area |> #tells furrr to split process up by area
                 group_by(question,response_text_analysis) %>%
                 summarise(report_area = unique(report_area) , 
                           wgt_percent = survey_mean(vartype = c("ci"), level = 0.95),
                           n_response = n(),
                           n_wgt_response = sum(cur_svy_wts(),na.rm = TRUE))) |>
    list_rbind()}

#run at each level####
nat <- aggregate_responses(responses_sdo_nat,scotland)  %>% mutate(level = "Scotland")
nett <- aggregate_responses(responses_sdo_nett,network_of_tx)  %>% mutate(level = "Network of treatment")
netr <- aggregate_responses(responses_sdo_netr,network_of_residence_tx) %>%  mutate(level = "Network of residence")
hbt <- aggregate_responses(responses_sdo_hbt,board_of_tx) %>%  mutate(level =  "NHS board of treatment")
hbr <- aggregate_responses(responses_sdo_hbr,board_of_residence_tx) %>%   mutate(level = "NHS board of residence")
cc <- aggregate_responses(responses_sdo_cc,cancer_centre) %>%  mutate(level = "Cancer centre")
cg <- aggregate_responses(responses_sdo_nat,cancer_group_smr06) %>%  mutate(level = "Cancer group")

#Create an index of the required rows, for using in creating master tables with all possible question and response options
#Prepare question lookup first
question_lookup <- question_lookup %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude") &  #filter only for those questions and response options being included in output
           question %in% questions) %>%
  group_by(question,question_text,response_text_analysis,topic,question_type,`2018_question`,`2015_question`) %>% 
  summarise(response_option = min(response_option),.groups = "keep") %>%
  arrange(question,response_option)

#define the expand table function.####
expand_table <- function(df) {
  idx <- rep(1:nrow(question_lookup), nrow(unique(df[c("report_area", "level")])))
  expand.table <- question_lookup[idx,]
  new_columns <- data.frame(unique(df[c("report_area", "level")])) %>% #create columns for report_area and level, repeated for the length of question lookup
    slice(rep(1:n(), each = nrow(question_lookup)))
  expand.table <- cbind(expand.table,new_columns)}

#create output for geographical and treatment areas ####
output <- bind_rows(nat,nett,netr,hbt,hbr,cc)

#read in lookups
hb_names <- read.csv(paste0(lookup_path,"SMRA.ANALYSIS.HEALTH_BOARD.csv"))%>%
  select(GRO_HB9_2019,DESCRIPTION) %>%
  rename(hb_code = GRO_HB9_2019) %>%
  rename(hb_name = DESCRIPTION) %>%
  filter(hb_code != "")

#create master question with one row for every question and response option,for every report area and level. 
master_question_table <- expand_table(output) 

output <- master_question_table %>% 
  #match on data to master question table
  left_join(output, 
            by = c("level","report_area","question","response_text_analysis")) %>% 
  mutate(across(c(wgt_percent,wgt_percent_low,wgt_percent_upp,n_response,n_wgt_response), ~replace_na(.,0))) %>% 
  group_by(level,report_area,question) %>%
  mutate(n_includedresponses = sum(n_response,na.rm = TRUE),
         n_wgt_includedresponses = sum(n_wgt_response,na.rm = TRUE),#recalculate to fill in gaps with na values
         percent = n_response / n_includedresponses) %>% 
  ungroup() %>% 
  #tidy tick all that apply responses
  filter(!(grepl("tick all that apply",question_type) == TRUE & response_option == "2")) %>% #remove 'no' values for tick all that apply %>% 
  
  mutate(response_option = case_when(grepl("tick",question_type) == TRUE & nchar(question) == 4 ~
                                       str_extract(question,"[[:alpha:]]$"), #if tick all that apply in format, eg q07a, extract final letter from question to be response option
                                     grepl("tick",question_type) == TRUE & grepl("_",question) == TRUE ~ #if tick all that apply in format, eg q47a01_1, extract final number from question to be response option
                                       str_extract(question,"[[:digit:]]$"),
                                     TRUE ~ response_option),
         question = case_when(grepl("tick",question_type) == TRUE & nchar(question) == 4 ~ substr(question,1,3), #if tick all that apply in format, eg q07a, extract original question number
                              grepl("tick",question_type) == TRUE & grepl("_",question) == TRUE ~ substr(question,1,str_locate(question,"_")[,1]-1),   #eg if tick all that apply in format, eg q47a01_1, extract original question number
                              TRUE ~ question),
         response_text_analysis = case_when(grepl("tick",question_type) == TRUE & response_text_analysis == "Yes"  ~ substr(question_text,str_locate(question_text,"\\?")[,1]+1,nchar(question_text)),   #eg if tick all that apply in format, extract response text
                                            TRUE ~ response_text_analysis),
         response_text_analysis = str_trim(response_text_analysis, side = c("both")), #tidy text
         question_text_2 = case_when(grepl("tick",question_type) == TRUE & nchar(question) == 3 #if tick all that apply, remove text after question mark as this is the response option
                                     ~substr(question_text,1,str_locate(question_text,"\\?")),
                                     TRUE ~question_text)) %>%  
  mutate(question_text = question_text_2) %>% 
  select(-question_text_2) %>% 
  #tidy report_area names
  left_join(hb_names, by = c("report_area" = "hb_code")) %>% 
  mutate(report_area_name = case_when(level == "National" ~ "Scotland",
                                      level %in% c("NHS board of treatment","NHS board of residence") ~ hb_name,
                                      report_area == "G516B" ~ "Beatson West of Scotland Cancer Centre",
                                      report_area ==  "H202H" ~"Raigmore Hospital",
                                      report_area ==  "N101H" ~"Aberdeen Royal Infirmary",
                                      report_area ==  "Not allocated"~ "Not allocated" ,  
                                      report_area ==  "S116B"  ~ "Edinburgh Cancer Centre",                                              
                                      report_area ==  "T101H"  ~ "Ninewells Hospital" ,
                                      report_area == "1" ~ "North of Scotland (NOSCAN)",
                                      report_area == "2" ~ "South of Scotland (SCAN)",
                                      report_area == "3" ~ "West of Scotland (WOSCAN)",
                                      report_area == "4" ~ "No network",
                                      TRUE ~ report_area)) %>% 
  select(-hb_name) %>% 
  arrange(level,report_area,question, response_option)

#create cancer_group_output####
cancer_group_output <- bind_rows(nat,cg)

#create master question with one row for every question and response option,for every report area and level. 
master_question_table <- expand_table(cancer_group_output) 

cancer_group_output <- master_question_table %>% 
  #match on data to master question table
  left_join(cancer_group_output, 
            by = c("level","report_area","question","response_text_analysis")) %>% 
  mutate(across(c(wgt_percent,wgt_percent_low,wgt_percent_upp,n_response,n_wgt_response), ~replace_na(.,0))) %>% 
  group_by(level,report_area,question) %>%
  mutate(n_includedresponses = sum(n_response,na.rm = TRUE),
         n_wgt_includedresponses = sum(n_wgt_response,na.rm = TRUE),#recalculate to fill in gaps with na values
         percent = n_response / n_includedresponses) %>% 
  ungroup() %>% 
  #tidy tick all that apply responses
  filter(!(grepl("tick all that apply",question_type) == TRUE & response_option == "2")) %>% #remove 'no' values for tick all that apply %>% 
  mutate(response_option = case_when(grepl("tick",question_type) == TRUE & nchar(question) == 4 ~
                                       str_extract(question,"[[:alpha:]]$"), #if tick all that apply in format, eg q07a, extract final letter from question to be response option
                                     grepl("tick",question_type) == TRUE & grepl("_",question) == TRUE ~ #if tick all that apply in format, eg q47a01_1, extract final number from question to be response option
                                       str_extract(question,"[[:digit:]]$"),
                                     TRUE ~ response_option),
         question = case_when(grepl("tick",question_type) == TRUE & nchar(question) == 4 ~ substr(question,1,3), #if tick all that apply in format, eg q07a, extract original question number
                              grepl("tick",question_type) == TRUE & grepl("_",question) == TRUE ~ substr(question,1,str_locate(question,"_")[,1]-1),   #eg if tick all that apply in format, eg q47a01_1, extract original question number
                              TRUE ~ question),
         response_text_analysis = case_when(grepl("tick",question_type) == TRUE & response_text_analysis == "Yes"  ~ substr(question_text,str_locate(question_text,"\\?")[,1]+1,nchar(question_text)),   #eg if tick all that apply in format, extract response text
                                            TRUE ~ response_text_analysis),
         response_text_analysis = str_trim(response_text_analysis, side = c("both")), #tidy text
         question_text_2 = case_when(grepl("tick",question_type) == TRUE & nchar(question) == 3 #if tick all that apply, remove text after question mark as this is the response option
                                     ~substr(question_text,1,str_locate(question_text,"\\?")),
                                     TRUE ~question_text)) %>%  
  mutate(question_text = question_text_2) %>% 
  select(-question_text_2) %>% 
  arrange(level,report_area,question, response_option)

#this is largely for checking purposes. Remove if not needed
output <- output %>%  
  select(question,question_text,response_text_analysis,topic,question_type,`2018_question`,`2015_question`,
         response_option,report_area,level,n_response,n_wgt_response,n_includedresponses,n_wgt_includedresponses,
         wgt_percent,percent,wgt_percent_low,wgt_percent_upp,report_area_name)


hist_output <- readRDS(paste0(analysis_output_path,"output.rds"))
all.equal(hist_output$level,output$level)
all.equal(hist_output %>% select(-wgt_percent_low,-wgt_percent_upp),output%>% select(-wgt_percent_low,-wgt_percent_upp))

write.xlsx(output,paste0(analysis_output_path,"output.xlsx"))
saveRDS(output, paste0(analysis_output_path,"output.rds"))

hist_cancer_group_output <- readRDS(paste0(analysis_output_path,"cancer_group_output.rds"))
all.equal(hist_cancer_group_output,cancer_group_output)
write.xlsx(cancer_group_output,paste0(analysis_output_path,"cancer_group_output.xlsx"))
saveRDS(cancer_group_output, paste0(analysis_output_path,"cancer_group_output.rds"))


#run average for question 55 ####
#define the aggregate function.####
aggregate_responses_average <- function(survey_design_object,report_areas) {
  plan(multisession)
  survey_design_object %>% 
    filter(!is.na(response_text_analysis)) %>% 
    filter(question == "q55") %>% 
    mutate(question = "q55_ave",
           response_option = as.numeric(response_option))%>% 
    group_by("report_area" = {{report_areas}}) %>%
    group_split() |>
    future_map(\(report_area) report_area |>
                 group_by(question) %>%
                 summarise(report_area = unique(report_area) , 
                           wgt_mean = survey_mean(response_option,vartype = c("ci"), level = 0.95),
                           n_response = n(),
                           n_wgt_response = sum(cur_svy_wts(),na.rm = TRUE))) |>
    list_rbind()}

#run at each level####
nat_q55  <- aggregate_responses_average(responses_sdo_nat,scotland) %>%  mutate(level = "Scotland") 
nett_q55 <- aggregate_responses_average(responses_sdo_nett,network_of_residence_tx) %>%  mutate(level = "Network of treatment")
netr_q55 <- aggregate_responses_average(responses_sdo_netr,network_of_tx) %>%  mutate(level = "Network of residence")
hbt_q55 <- aggregate_responses_average(responses_sdo_hbt,board_of_tx) %>%  mutate(level =  "NHS board of treatment")
hbr_q55 <- aggregate_responses_average(responses_sdo_hbr,board_of_residence_tx) %>%   mutate(level = "NHS board of residence")
cc_q55 <- aggregate_responses_average(responses_sdo_cc,cancer_centre) %>%  mutate(level = "Cancer centre")
cg_q55 <- aggregate_responses_average(responses_sdo_nat,cancer_group_smr06) %>%  mutate(level = "Cancer group")

#create q55 output for all levels####
q55_output <- bind_rows(nat_q55,nett_q55,netr_q55,hbt_q55,cc_q55,cg_q55)

q55_output <- q55_output %>% 
  #tidy report_area names
  left_join(hb_names, by = c("report_area" = "hb_code")) %>% 
  mutate(report_area_name = case_when(level == "National" ~ "Scotland",
                                      level %in% c("NHS board of treatment","NHS board of residence") ~ hb_name,
                                      report_area == "G516B" ~ "Beatson West of Scotland Cancer Centre",
                                      report_area ==  "H202H" ~"Raigmore Hospital",
                                      report_area ==  "N101H" ~"Aberdeen Royal Infirmary",
                                      report_area ==  "Not allocated"~ "Not allocated" ,  
                                      report_area ==  "S116B"  ~ "Edinburgh Cancer Centre",                                              
                                      report_area ==  "T101H"  ~ "Ninewells Hospital" ,
                                      report_area == "1" ~ "North of Scotland (NOSCAN)",
                                      report_area == "2" ~ "South of Scotland (SCAN)",
                                      report_area == "3" ~ "West of Scotland (WOSCAN)",
                                      report_area == "4" ~ "No network",
                                      TRUE ~ report_area)) %>% 
  select(-hb_name) %>% 
  arrange(level,report_area,question)

hist_output <- readRDS(paste0(analysis_output_path,"q55_output.rds"))
all.equal(hist_output$wgt_mean,q55_output$wgt_mean)

write.xlsx(q55_output,paste0(analysis_output_path,"q55_output.xlsx"))
saveRDS(q55_output, paste0(analysis_output_path,"q55_output.rds"))

