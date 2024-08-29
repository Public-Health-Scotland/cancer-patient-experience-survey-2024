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
# Approximate memory usage: <2 GiB
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
#analysis_output_path,"q55_output.xlsx"
#analysis_output_path,"q55_output.rds"

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

responses_wider <- responses_longer %>% 
  pivot_wider(id_cols = c(patientid,all_of(report_areas_output),all_of(report_area_wt),no_wt,cancer_group_smr06),names_from = question,values_from = response_text_analysis)

#define survey design objects
options(survey.lonely.psu="remove")
surveydesign_nat<-svydesign(ids=~1, weights=~nat_wt, data=responses_wider)
surveydesign_nett<-svydesign(ids=~1, weights=~nett_wt, data=responses_wider)
surveydesign_netr<-svydesign(ids=~1, weights=~netr_wt, data=responses_wider)
surveydesign_hbt<-svydesign(ids=~1, weights=~hbt_wt, data=responses_wider)
surveydesign_hbr<-svydesign(ids=~1, weights=~hbr_wt, data=responses_wider)
surveydesign_hbr2<-svydesign(ids=~1, weights=~hbr2_wt, data=responses_wider)
surveydesign_cc<-svydesign(ids=~1, weights=~no_wt, data=responses_wider)

#define function to calculate CIs using survey package
get_survey_CIs <- function(x,survey_design_object,report_areas) {
  survey_design_object <- update(survey_design_object, analysis_var=factor(get(x)), grouping_var=factor(get(report_areas)))
  as.data.frame(confint(svyby(~analysis_var, by=~grouping_var, survey_design_object,svymean, na.rm=TRUE, deff="replace", keep.var=TRUE))) %>% 
    rename(wgt_percent_low = `2.5 %`,
           wgt_percent_upp = `97.5 %`) %>% 
    mutate("question" = x,
           divider = str_locate(rownames(.),":"),
           divider2 = if_else(str_detect(rownames(.),"[[:punct:]]{3}") == T,
                              str_locate(rownames(.),"[[:punct:]]{3}")-1,
                              nchar(rownames(.))),#get variable values from row names
           report_area = substr(rownames(.),1,divider-1),
           response_text_analysis = substr(rownames(.),divider+13,divider2)) %>% 
    select(report_area, question, response_text_analysis, wgt_percent_low,wgt_percent_upp)%>% 
    remove_rownames(.)}

#define aggregate_responses function
aggregate_responses <- function(report_areas,weights) {
  responses_longer %>% 
    filter(!is.na(response_text_analysis)) %>% 
    group_by("report_area" = {{report_areas}},question,response_text_analysis) %>%
    summarise(n_response = n(),
              n_wgt_response = sum({{weights}},na.rm = TRUE),.groups = "keep")}

#run at each level####
nat_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_nat,"scotland")) %>%  bind_rows() %>% mutate(level = "Scotland")
nat <- aggregate_responses(scotland,nat_wt) 
nat <- nat_cis %>%   left_join(nat,by = c("report_area","question","response_text_analysis"))


nett_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_nett,"network_of_tx")) %>%  bind_rows() %>% mutate(level = "Network of treatment")
nett <- aggregate_responses(network_of_tx,nett_wt) 
nett <- nett_cis %>%   left_join(nett,by = c("report_area","question","response_text_analysis"))

netr_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_netr,"network_of_residence_tx")) %>%  bind_rows() %>% mutate(level = "Network of residence")
netr <- aggregate_responses(network_of_residence_tx,netr_wt) 
netr <- netr_cis %>%   left_join(netr,by = c("report_area","question","response_text_analysis"))

hbt_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_hbt,"board_of_tx")) %>%  bind_rows() %>% mutate(level = "NHS board of treatment")
hbt <- aggregate_responses(board_of_tx,hbt_wt) 
hbt <- hbt_cis %>%   left_join(hbt,by = c("report_area","question","response_text_analysis"))

hbr_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_hbr,"board_of_residence_tx")) %>%  bind_rows() %>%  mutate(level = "NHS board of residence")
hbr <- aggregate_responses(board_of_residence_tx,hbr_wt) 
hbr <- hbr_cis %>%  left_join(hbr,by = c("report_area","question","response_text_analysis"))

hbr2_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_hbr,"board_of_residence2")) %>%  bind_rows() %>% mutate(level = "NHS board of residence (alt)")
hbr2 <- aggregate_responses(board_of_residence2,hbr2_wt) 
hbr2 <- hbr2_cis %>%   left_join(hbr2,by = c("report_area","question","response_text_analysis"))

cc_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_cc,"cancer_centre")) %>%  bind_rows() %>% mutate(level = "Cancer centre")
cc <- aggregate_responses(cancer_centre,no_wt) 
cc <- cc_cis %>%   left_join(cc,by = c("report_area","question","response_text_analysis"))

cg_cis <- lapply(questions, function (x) get_survey_CIs(x,surveydesign_nat,"cancer_group_smr06")) %>%  bind_rows() %>% mutate(level = "Cancer group")
cg <- aggregate_responses(cancer_group_smr06,nat_wt) 
cg <- cg_cis %>%  left_join(cg,by = c("report_area","question","response_text_analysis"))

#Create an index of the required rows, for using in creating master tables with all possible question and response options
#Prepare question lookup first
question_lookup <- question_lookup %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude") &  #filter only for those questions and response options being included in output
           question %in% questions) %>%
  group_by(question,question_text,question_text_dashboard,response_text_analysis,topic,question_type,`2018_question`,`2015_question`) %>% 
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
output <- bind_rows(nat,nett,netr,hbt,hbr,hbr2,cc)

#read in lookups
hb_names <- read.csv(paste0(lookup_path,"SMRA.ANALYSIS.HEALTH_BOARD.csv"))%>%
  select(GRO_HB9_2019,DESCRIPTION) %>%
  rename(hb_code = GRO_HB9_2019) %>%
  rename(hb_name = DESCRIPTION) %>%
  mutate(hb_name = case_match(hb_name,"National Facility" ~ "NHS Golden Jubilee",.default = hb_name)) %>% 
  filter(hb_code != "")


#create master question with one row for every question and response option,for every report area and level. 
master_question_table <- expand_table(output) 

output <- master_question_table %>% 
  #match on data to master question table
  left_join(output, 
            by = c("level","report_area","question","response_text_analysis")) %>% 
  mutate(across(c(n_response,n_wgt_response), ~replace_na(.,0))) %>% 
  group_by(level,report_area,question) %>%
  mutate(n_includedresponses = sum(n_response,na.rm = TRUE),
         n_wgt_includedresponses = sum(n_wgt_response,na.rm = TRUE),#recalculate to fill in gaps with na values
         wgt_percent = n_wgt_response / n_wgt_includedresponses,
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
                                      report_area == "1" ~ "NCA – North Cancer Alliance",
                                      report_area == "2" ~ "SCAN – South East Cancer Network",
                                      report_area == "3" ~ "WoSCAN – West of Scotland Cancer Network",
                                      report_area == "4" ~ "No network",
                                      TRUE ~ report_area)) %>% 
  select(-hb_name) %>% 
  arrange(level,report_area,question, response_option) %>% 
  select(question,question_text,question_text_dashboard,response_text_analysis,topic,question_type,`2018_question`,`2015_question`,
         response_option,report_area,level,n_response,n_wgt_response,n_includedresponses,n_wgt_includedresponses,
         wgt_percent,percent,wgt_percent_low,wgt_percent_upp,report_area_name)

#create cancer_group_output####
cancer_group_output <- bind_rows(nat,cg)

#create master question with one row for every question and response option,for every report area and level. 
master_question_table <- expand_table(cancer_group_output) 

cancer_group_output <- master_question_table %>% 
  #match on data to master question table
  left_join(cancer_group_output, 
            by = c("level","report_area","question","response_text_analysis")) %>% 
  mutate(across(c(n_response,n_wgt_response), ~replace_na(.,0))) %>% 
  group_by(level,report_area,question) %>%
  mutate(n_includedresponses = sum(n_response,na.rm = TRUE),
         n_wgt_includedresponses = sum(n_wgt_response,na.rm = TRUE),#recalculate to fill in gaps with na values
         wgt_percent = n_wgt_response / n_wgt_includedresponses,
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
  arrange(level,report_area,question, response_option) %>% 
  select(question,question_text,question_text_dashboard,response_text_analysis,topic,question_type,`2018_question`,`2015_question`,
         response_option,report_area,level,n_response,n_wgt_response,n_includedresponses,n_wgt_includedresponses,
         wgt_percent,percent,wgt_percent_low,wgt_percent_upp)

hist_output <- readRDS(paste0(analysis_output_path,"output.rds"))
all.equal(hist_output,output)
all.equal(hist_output %>% select(-wgt_percent_low,-wgt_percent_upp,-report_area_name),output%>% select(-wgt_percent_low,-wgt_percent_upp,-report_area_name))
write.xlsx(output,paste0(analysis_output_path,"output.xlsx"))
saveRDS(output, paste0(analysis_output_path,"output.rds"))

hist_cancer_group_output <- readRDS(paste0(analysis_output_path,"cancer_group_output.rds"))
all.equal(hist_cancer_group_output$question_text_dashboard,cancer_group_output$question_text_dashboard)
all.equal(hist_cancer_group_output %>% select(-wgt_percent_low,-wgt_percent_upp),
          cancer_group_output%>% select(-wgt_percent_low,-wgt_percent_upp))
write.xlsx(cancer_group_output,paste0(analysis_output_path,"cancer_group_output.xlsx"))
saveRDS(cancer_group_output, paste0(analysis_output_path,"cancer_group_output.rds"))


#run average for question 55 ####
#define the aggregate function.####
responses_wider_q55 <- responses_longer %>% 
  filter(question == "q55") %>% 
  mutate(question = "q55_ave",
         response_option = as.numeric(response_option))%>% 
  pivot_wider(id_cols = c(patientid,all_of(report_areas_output),all_of(report_area_wt),no_wt,cancer_group_smr06),names_from = question,values_from = response_option)

#define survey design objects
options(survey.lonely.psu="remove")
surveydesign_nat_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = nat_wt) 
surveydesign_nett_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = nett_wt) 
surveydesign_netr_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = netr_wt) 
surveydesign_hbt_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = hbt_wt) 
surveydesign_hbr_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = hbr_wt) 
surveydesign_hbr2_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = hbr2_wt) 
surveydesign_cc_q55 <- as_survey_design(responses_wider_q55,ids = 1,weights = no_wt) 

aggregate_responses_average <- function(survey_design_object,report_areas) {
  survey_design_object %>% 
    group_by(question = "q55_ave","report_area" = {{report_areas}}) %>%
    summarise(wgt_mean = survey_mean(q55_ave,na.rm = TRUE),
             n_response = n(),
             n_wgt_response = sum(cur_svy_wts(),na.rm = TRUE),
             wgt_mean2  = mean(q55_ave,na.rm = TRUE)) %>% 
    select(-wgt_mean_se)}

get_survey_CIs_ave <- function(survey_design_object,report_areas) {
  survey_design_object <- update(survey_design_object, analysis_var=q55_ave, grouping_var=factor(get(report_areas)))
  as.data.frame(confint(svyby(~analysis_var, by=~grouping_var, survey_design_object,svymean, na.rm=TRUE, deff="replace", keep.var=TRUE))) %>% 
  rename(wgt_mean_low = `2.5 %`,
       wgt_mean_upp = `97.5 %`) %>% 
  mutate("question" = "q55_ave",#get variable values from row names
         report_area = rownames(.)) %>% 
  select(report_area, question, wgt_mean_low,wgt_mean_upp)%>% 
    remove_rownames(.)}

#run at each level####
nat_q55_cis  <- get_survey_CIs_ave(surveydesign_nat_q55,"scotland") %>%   mutate(level = "Scotland")
nat_q55  <- aggregate_responses_average(surveydesign_nat_q55,scotland) 
nat_q55 <- nat_q55_cis %>%   left_join(nat_q55,by = c("report_area","question"))

nett_q55_cis  <- get_survey_CIs_ave(surveydesign_nett_q55,"network_of_tx") %>%   mutate(level = "Network of treatment")
nett_q55  <- aggregate_responses_average(surveydesign_nett_q55,network_of_tx) 
nett_q55 <- nett_q55_cis %>%   left_join(nett_q55,by = c("report_area","question"))

netr_q55_cis  <- get_survey_CIs_ave(surveydesign_netr_q55,"network_of_residence_tx") %>%   mutate(level = "Network of residence")
netr_q55  <- aggregate_responses_average(surveydesign_netr_q55,network_of_residence_tx) 
netr_q55 <- netr_q55_cis %>%   left_join(netr_q55,by = c("report_area","question"))

hbt_q55_cis  <- get_survey_CIs_ave(surveydesign_hbt_q55,"board_of_tx") %>%   mutate(level = "NHS board of treatment")
hbt_q55  <- aggregate_responses_average(surveydesign_hbt_q55,board_of_tx) 
hbt_q55 <- hbt_q55_cis %>%   left_join(hbt_q55,by = c("report_area","question"))

hbr_q55_cis  <- get_survey_CIs_ave(surveydesign_hbr_q55,"board_of_residence_tx") %>%   mutate(level = "NHS board of residence")
hbr_q55  <- aggregate_responses_average(surveydesign_hbr_q55,board_of_residence_tx) 
hbr_q55 <- hbr_q55_cis %>%   left_join(hbr_q55,by = c("report_area","question"))

hbr2_q55_cis  <- get_survey_CIs_ave(surveydesign_hbr2_q55,"board_of_residence2") %>%   mutate(level = "NHS board of residence (alt)")
hbr2_q55  <- aggregate_responses_average(surveydesign_hbr2_q55,board_of_residence2) 
hbr2_q55 <- hbr2_q55_cis %>%   left_join(hbr2_q55,by = c("report_area","question"))

cc_q55_cis  <- get_survey_CIs_ave(surveydesign_cc_q55,"cancer_centre") %>%   mutate(level = "Cancer centre")
cc_q55  <- aggregate_responses_average(surveydesign_cc_q55,cancer_centre) 
cc_q55 <- cc_q55_cis %>%   left_join(cc_q55,by = c("report_area","question"))

cg_q55_cis  <- get_survey_CIs_ave(surveydesign_nat_q55,"cancer_group_smr06") %>%   mutate(level = "Cancer group")
cg_q55  <- aggregate_responses_average(surveydesign_nat_q55,cancer_group_smr06) 
cg_q55 <- cg_q55_cis %>%   left_join(cg_q55,by = c("report_area","question"))

#create q55 output for all levels####
q55_output <- bind_rows(nat_q55,nett_q55,netr_q55,hbt_q55,hbr_q55,hbr2_q55,cc_q55,cg_q55)

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
                                      report_area == "1" ~ "NCA – North Cancer Alliance",
                                      report_area == "2" ~ "SCAN – South East Cancer Network",
                                      report_area == "3" ~ "WoSCAN – West of Scotland Cancer Network",
                                      report_area == "4" ~ "No network",
                                      TRUE ~ report_area)) %>% 
  select(-hb_name) %>% 
  arrange(level,report_area,question)

hist_output <- readRDS(paste0(analysis_output_path,"q55_output.rds"))
all.equal(hist_output,q55_output)

write.xlsx(q55_output,paste0(analysis_output_path,"q55_output.xlsx"))
saveRDS(q55_output, paste0(analysis_output_path,"q55_output.rds"))

