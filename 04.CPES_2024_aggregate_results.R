# Name of file: 04.CPES_2024_aggregate_results.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  output analyses at all levels of reporting.
# 
# Approximate run time: 3hrs 35mins (approx)
# 
# Approximate memory usage: 2.33GiB
# 
# *****************************************

#Inputs: 
#analysis_output_path,"responses_longer.rds"
#lookup_path,"SMRA.ANALYSIS.HEALTH_BOARD.csv"
#lookup_path,"question_lookup.rds"

#Outputs: #UPDATE!
#analysis_output_path,"output.xlsx"
#analysis_output_path,"output.rds"
#analysis_output_path,"tumour_output.xlsx"
#analysis_output_path,"tumour_output.rds"


#to do: filter out allocation questions, no responses to tick all that apply type question, rename variables

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#read in responses longer data####
responses_longer <- readRDS(paste0(analysis_output_path,"responses_longer.rds"))

responses_longer <- responses_longer %>%   mutate(no_wt = 1) #add a no_wt variable for cancer centres

#read in lookups
hb_names <- read.csv(paste0(lookup_path,"SMRA.ANALYSIS.HEALTH_BOARD.csv"))%>% 
  select(GRO_HB9_2019,DESCRIPTION) %>% 
  rename(hb_code = GRO_HB9_2019) %>% 
  rename(hb_name = DESCRIPTION) %>% 
  filter(hb_code != "")

question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) %>% select(-response_value,-response_text) #read in lookup 
responses_longer <- responses_longer %>% 
  left_join(question_lookup,by = c("question","response_option","cancercentreallocation")) %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude")) 

question_lookup <- question_lookup %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude")) %>% 
  select(question,question_text,response_option,response_text_analysis,topic) %>% 
  distinct()

##define the aggregate function.####
aggregate_responses <- function(report_areas,wt) {
  responses_longer <- responses_longer %>%
    mutate(response = 1) %>% 
    as_survey_design(weights = {{wt}}) %>% 
    group_by("report_area" = {{report_areas}},question,response_text_analysis) %>%
    summarise(wgt_percent = survey_prop(na.rm = TRUE,vartype = c("ci"),level = 0.95,proportion = TRUE,deff = FALSE),
              n_response = n(),
              n_wgt_response = sum({{wt}})) %>% 
    group_by(report_area,question) %>%
    mutate(n_includedresponses = sum(n_response),
           n_wgt_includedresponses = sum(n_wgt_response))}

##define the expand table function.####
#Create an index of the required rows, then use to create a master table to ensure all possible option combinations exist
expand_table <- function(df) {
  idx <- rep(1:nrow(question_lookup), length(unique(df$report_area)))
  expand.table <- question_lookup[idx,]
  expand.table <- expand.table %>% 
    mutate(report_area = rep(unique(df$report_area),each = nrow(question_lookup)))%>%
    left_join(df,by = c("report_area","question","response_text_analysis"))
}

#run at each level####
df <- aggregate_responses(scotland,nat_wt)
nat <- expand_table(df)%>%  mutate(level = "Scotland")

df <- aggregate_responses(network_of_tx,nett_wt)
nett <- expand_table(df) %>% mutate(level = "Network of treatment")

df <- aggregate_responses(network_of_residence_tx,netr_wt)
netr <- expand_table(df) %>%  mutate(level = "Network of residence")

df <- aggregate_responses(board_of_tx,hbt_wt)
hbt <- expand_table(df)%>%  mutate(level =  "NHS board of treatment")

df <- aggregate_responses(board_of_residence_tx,hbr_wt)
hbr <- expand_table(df)%>%   mutate(level = "NHS board of residence")

df <- aggregate_responses(cancer_centre,no_wt)
cc <- expand_table(df)%>%  mutate(level = "Cancer centre")

df <- aggregate_responses(cancer_group_smr06,nat_wt)
cg <- expand_table(df)%>%  mutate(level = "Cancer group")

saveRDS(nat,paste0(analysis_output_path,"nat.rds"))
saveRDS(nett,paste0(analysis_output_path,"nett.rds"))
saveRDS(netr,paste0(analysis_output_path,"netr.rds"))
saveRDS(hbt,paste0(analysis_output_path,"hbt.rds"))
saveRDS(hbr,paste0(analysis_output_path,"hbr.rds"))
saveRDS(cc,paste0(analysis_output_path,"cc.rds"))
saveRDS(cg,paste0(analysis_output_path,"cg.rds"))

output <- distinct(bind_rows(nat,nett,netr,hbt,hbr,cc)) %>% 
  left_join(hb_names, by = c("report_area" = "hb_code")) %>% 
  mutate(report_area_name = case_when(level == "National" ~ "Scotland",
                                      level %in% c("NHS board of treatment","NHS board of residence") ~ hb_name,
                                      report_area == "G516B" ~ "Beatson West of Scotland Cancer Centre",
                                      report_area ==  "H202H" ~"Raigmore Hospital",
                                      report_area ==  "N101H" ~"Aberdeen Royal Infirmary",
                                      report_area ==  "Not allocated"~ "Not allocated" ,  
                                      report_area ==  "S116B"  ~ "Edinburgh Cancer Centre",                                              
                                      report_area ==  "T101H"  ~ "Ninewells Hospital" ,
                                      report_area == "1" ~ "NOSCAN",
                                      report_area == "2" ~ "SCAN",
                                      report_area == "3" ~ "WOSCAN",
                                      report_area == "4" ~ "No network",
                                      TRUE ~ report_area)) %>% 
  mutate(percent = n_response / n_includedresponses)
saveRDS(output, paste0(analysis_output_path,"provisional_output.rds"))
write.xlsx(output,paste0(analysis_output_path,"provisional_output.xlsx"))

################

#run average for question 55#### 
##define the aggregate function.####
aggregate_responses_average <- function(report_areas,wt) {
  responses_longer <- responses_longer %>%
    filter(question == "q55") %>% 
    mutate(question == "q55_ave",
           response = 1,
           response_option = as.numeric(response_option)) %>% 
    as_survey_design(weights = {{wt}}) %>% 
    group_by("report_area" = {{report_areas}},question) %>%
    summarise('wgt_mean' = survey_mean(response_option,na.rm = TRUE,vartype = c("ci"),level = 0.95,proportion = FALSE,deff = FALSE),
              n_response = n(),
              n_wgt_response = sum({{wt}}),
              mean = mean(response_option)) %>% 
    group_by(report_area,question) %>%
    mutate(n_includedresponses = sum(n_response),
           n_wgt_includedresponses = sum(n_wgt_response))}

df <- aggregate_responses_average(scotland,nat_wt)
nat_q55 <- df %>% mutate(level = "Scotland")
nat_q55$question[nat_q55$question=="q55"] <- "q55_ave"

df <- aggregate_responses_average(network_of_tx,nett_wt)
nett_q55 <- df %>% mutate(level = "Network of treatment")
nett_q55$question[nett_q55$question=="q55"] <- "q55_ave"

df <- aggregate_responses_average(network_of_residence_tx,netr_wt)
netr_q55 <- df %>% mutate(level = "Network of residence")
netr_q55$question[netr_q55$question=="q55"] <- "q55_ave"

df <- aggregate_responses_average(board_of_tx,hbt_wt)
hbt_q55 <- df %>% mutate(level =  "NHS board of treatment")
hbt_q55$question[hbt_q55$question=="q55"] <- "q55_ave"

df <- aggregate_responses_average(board_of_residence_tx,hbr_wt)
hbr_q55 <- df %>% mutate(level = "NHS board of residence")
hbr_q55$question[hbr_q55$question=="q55"] <- "q55_ave"

df <- aggregate_responses_average(cancer_centre,no_wt)
cc_q55 <- df %>% mutate(level = "Cancer centre")
cc_q55$question[cc_q55$question=="q55"] <- "q55_ave"

df <- aggregate_responses_average(cancer_group_smr06,nat_wt)
cg_q55 <- df %>% mutate(level = "Cancer group")
cg_q55$question[cg_q55$question=="q55"] <- "q55_ave"

saveRDS(nat_q55,paste0(analysis_output_path,"nat_q55.rds"))
saveRDS(nett_q55,paste0(analysis_output_path,"nett_q55.rds"))
saveRDS(netr_q55,paste0(analysis_output_path,"netr_q55.rds"))
saveRDS(hbt_q55,paste0(analysis_output_path,"hbt_q55.rds"))
saveRDS(hbr_q55,paste0(analysis_output_path,"hbr_q55.rds"))
saveRDS(cc_q55,paste0(analysis_output_path,"cc_q55.rds"))
saveRDS(cg_q55,paste0(analysis_output_path,"cg_q55.rds"))

output_q55ave <- distinct(bind_rows(nat_q55,nett_q55,netr_q55,hbt_q55,hbr_q55,cc_q55,cg_q55)) %>% 
  left_join(hb_names, by = c("report_area" = "hb_code")) %>% 
  mutate(report_area_name = case_when(level == "National" ~ "Scotland",
                                      level %in% c("NHS board of treatment","NHS board of residence") ~ hb_name,
                                      report_area == "G516B" ~ "Beatson West of Scotland Cancer Centre",
                                      report_area ==  "H202H" ~"Raigmore Hospital",
                                      report_area ==  "N101H" ~"Aberdeen Royal Infirmary",
                                      report_area ==  "Not allocated"~ "Not allocated" ,  
                                      report_area ==  "S116B"  ~ "Edinburgh Cancer Centre",                                              
                                      report_area ==  "T101H"  ~ "Ninewells Hospital" ,
                                      report_area == "1" ~ "NOSCAN",
                                      report_area == "2" ~ "SCAN",
                                      report_area == "3" ~ "WOSCAN",
                                      report_area == "4" ~ "No network",
                                      TRUE ~ report_area)) %>% 
  mutate(percent = n_response / n_includedresponses)
saveRDS(output_q55ave, paste0(analysis_output_path,"provisional_output_q55ave.rds"))
write.xlsx(output_q55ave,paste0(analysis_output_path,"provisional_output_q55ave.xlsx"))

#######

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

output <- readRDS(paste0(analysis_output_path,"provisional_output.rds"))
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) 

question_lookup <- question_lookup %>% 
  select(-response_value) %>%
  filter(!response_text_analysis %in% c(NA,"Exclude")) %>% 
  group_by(question,question_type,response_text_analysis,`2018_question`,`2015_question`) %>% 
  summarise(response_option = first(response_option))
#`2018_question`,`2018_option`,`2015_question`,`2015_option`

output <- output %>% 
  select(-hb_name) %>% 
  left_join(question_lookup, by = c("question","response_text_analysis","response_option")) %>% 
  arrange(level,report_area,question,response_option) %>%
  filter(!question_type %in% c(NA)) # to remove duplicated negative values

table(output$question[output$question_type== "Information (tick all that apply)"])
#Code to deal with 'tick all that apply' questions in output.
#Removes the "No" response to the "tick all that apply" questions q07a-f, q46a-j, q47a01_1-q47b07_5, q50a_1-q50e_6, q51a_1-q51e_6, q61a-k####
#tidies question numbers, text, response options and response text
output<- output %>%
  mutate(information_questions_tata = case_when(question_type == "Information (tick all that apply)" & response_text_analysis == "No" ~ question)) %>%
  mutate(tata_remove = case_when(question_type == "Information (tick all that apply)" & response_text_analysis == "No" ~ 1)) %>%
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
        response_text_analysis = str_trim(response_text_analysis, side = c("both"))) #tidy text

output <- output %>%
  filter(is.na(tata_remove)) %>%
  select(-information_questions_tata,-tata_remove)

#Create cancer_group_output####
cancer_group_output <- distinct(bind_rows(nat,cg)) %>% 
  left_join(question_lookup, by = c("question","response_text_analysis","response_option")) %>% 
  arrange(level,report_area,question,response_option) %>%
  filter(!question_type %in% c(NA)) # to remove duplicated negative values

#Code to deal with 'tick all that apply' questions in cancer_group_output
#Removes the "No" response to the "tick all that apply" questions q07a-f, q46a-j, q47a01_1-q47b07_5, q50a_1-q50e_6, q51a_1-q51e_6, q61a-k####
cancer_group_output <- cancer_group_output %>%
  mutate(information_questions_tata = case_when(question_type == "Information (tick all that apply)" & response_text_analysis == "No" ~ question)) %>%
  mutate(tata_remove = case_when(question_type == "Information (tick all that apply)" & response_text_analysis == "No" ~ 1)) %>%
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
         response_text_analysis = str_trim(response_text_analysis, side = c("both"))) #tidy text

cancer_group_output <- cancer_group_output %>%
  filter(is.na(tata_remove)) %>%
  select(-information_questions_tata,-tata_remove)

write.xlsx(output,paste0(analysis_output_path,"output.xlsx"))
saveRDS(output, paste0(analysis_output_path,"output.rds"))
write.xlsx(cancer_group_output,paste0(analysis_output_path,"cancer_group_output.xlsx"))
saveRDS(cancer_group_output, paste0(analysis_output_path,"cancer_group_output.rds"))