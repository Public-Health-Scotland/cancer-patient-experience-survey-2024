# Name of file: 05.CPES_2024_historic_matching.R
# 
# Original author(s): Martin Leitch
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  output analyses at all levels of reporting.
# 
# Approximate run time: <1min
# 
# Approximate memory usage: 590 MiB
# 
# *****************************************

#Inputs: 
#analysis_output_path,"output.rds"
#lookup_path,"question_lookup.rds"
#data_path_2018,"output.rds"
#data_path_2015,"output.rds"
##analysis_output_path,"cancer_group_output.rds"
#data_path_2018,"cancer_group_output.rds"
#data_path_2015,"cancer_group_output.rds"

#Outputs: 
#analysis_output_path,"output_2024.xlsx"
#analysis_output_path,"output_2024.rds"
#analysis_output_path,"cancer_group_output_2024.xlsx"
#analysis_output_path,"cancer_group_output_2024.rds"

#Join historical results for 2018 and 2015 on to the 2024 results####
##Read in historic data for 2018 and 2015 and then match onto the 2024 results files ("output" & "tumour_output")
##Need to do separately for output and tumour output equivalent files.

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Geographical output####
##Read in 2024 output.rds####
output <- readRDS(paste0(analysis_output_path,"output.rds"))
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) %>% 
  select(question,response_option,response_text_dashboard,`2018_question`,`2018_option`,comparability_2018,`2015_question`,`2015_option`,comparability_2015)
#
#Amend 2018 response option (`2018_option`) for q29f (`2018_question`) to ensure 2024 match up with historic_2018
question_lookup$`2018_option`[question_lookup$`2018_question` == "q29f" & question_lookup$`2018_option` == 5] <- 4
question_lookup$`2018_option`[question_lookup$`2018_question` == "q29f" & question_lookup$`2018_option` == 2] <- 1

#Amend 2024 response option for q53 (`2018_question`) to ensure 2024 match up with historic_2018
question_lookup$response_option[question_lookup$question == "q46a" & question_lookup$response_option == 1] <- "a"
question_lookup$response_option[question_lookup$question == "q46b" & question_lookup$response_option == 1] <- "b"
question_lookup$response_option[question_lookup$question == "q46c" & question_lookup$response_option == 1] <- "c"
question_lookup$response_option[question_lookup$question == "q46d" & question_lookup$response_option == 1] <- "d"
question_lookup$response_option[question_lookup$question == "q46e" & question_lookup$response_option == 1] <- "e"
question_lookup$response_option[question_lookup$question == "q46f" & question_lookup$response_option == 1] <- "f"
question_lookup$response_option[question_lookup$question == "q46g" & question_lookup$response_option == 1] <- "g"
question_lookup$response_option[question_lookup$question == "q46h" & question_lookup$response_option == 1] <- "h"
question_lookup$response_option[question_lookup$question == "q46i" & question_lookup$response_option == 1] <- "i"
question_lookup$response_option[question_lookup$question == "q46j" & question_lookup$response_option == 1] <- "j"
#Amend 2024 question for q46 to ensure 2024 match up with historic_2018
question_lookup <- question_lookup %>%
  mutate(question = if_else((question == "q46a" & response_option == "a") | (question == "q46b" & response_option == "b") | (question == "q46c" & response_option == "c") |
                            (question == "q46d" & response_option == "d") | (question == "q46e" & response_option == "e") | (question == "q46f" & response_option == "f") |
                            (question == "q46g" & response_option == "g") | (question == "q46h" & response_option == "h") | (question == "q46i" & response_option == "i") |
                            (question == "q46j" & response_option == "j"),"q46",question))

#Amend 2018 response option (`2018_option`) for q53 (`2018_question`) to ensure 2024 match up with historic_2018
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53a" & question_lookup$`2018_option` == 1] <- "a"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53b" & question_lookup$`2018_option` == 1] <- "b"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53c" & question_lookup$`2018_option` == 1] <- "c"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53d" & question_lookup$`2018_option` == 1] <- "d"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53e" & question_lookup$`2018_option` == 1] <- "e"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53f" & question_lookup$`2018_option` == 1] <- "f"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53g" & question_lookup$`2018_option` == 1] <- "g"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53h" & question_lookup$`2018_option` == 1] <- "h"
question_lookup$`2018_option`[question_lookup$`2018_question` == "q53i" & question_lookup$`2018_option` == 1] <- "i"
#Amend 2018 question (`2018_option`) for q53 (`2018_question`) to ensure 2024 match up with historic_2018
question_lookup <- question_lookup %>%
  mutate(`2018_question` = if_else((`2018_question` == "q53a" & `2018_option` == "a") | (`2018_question` == "q53b" & `2018_option` == "b") | (`2018_question` == "q53c" & `2018_option` == "c") |
                                   (`2018_question` == "q53d" & `2018_option` == "d") | (`2018_question` == "q53e" & `2018_option` == "e") | (`2018_question` == "q53f" & `2018_option` == "f") |
                                   (`2018_question` == "q53g" & `2018_option` == "g") | (`2018_question` == "q53h" & `2018_option` == "h") | (`2018_question` == "q53i" & `2018_option` == "i") |
                                   (`2018_question` == "q53j" & `2018_option` == "j"),"q53",`2018_question`))

#Amend 2018 question for q46 (`2018_question`) to ensure 2024 match up with historic_2018
output <- output %>%
  mutate(`2018_question` = if_else((`2018_question` == "q53a") | (`2018_question` == "q53b") | (`2018_question` == "q53c") | (`2018_question` == "q53d") | 
                                   (`2018_question` == "q53e") | (`2018_question` == "q53f") | (`2018_question` == "q53g") | (`2018_question` == "q53h") | 
                                   (`2018_question` == "q53i") | (`2018_question` == "q53j" ),"q53",`2018_question`))

output <- output %>% 
  left_join(question_lookup,by = c("question","response_option","2018_question","2015_question"))

#Amend the 2018 response_option to "-" for q61 to not match on the 2018 "At least one disability / impairment / condition" measure which is not in 2024
output <- output %>%
  mutate(`2018_option` = if_else(question == "q61","-",`2018_option`))

#Where response_text_dashboard = NA, this should equal response_text_analysis
output$check = 0
output <- output %>%
  mutate(check = if_else(is.na(response_text_dashboard),1,check))
table(output$check)
output <- output %>%
  mutate(response_text_dashboard = if_else(is.na(response_text_dashboard),response_text_analysis,response_text_dashboard))

##2018####
historic_2018 <- readRDS(paste0(data_path_2018,"output_2018.rds"))
ls(historic_2018)
historic_2018 <- historic_2018 %>% 
  select(question,level,report_area_name,report_area,response_option,response_text_analysis,n_includedresponses,n_response,wgt_percent,wgt_percent_low,wgt_percent_upp)%>%
  rename_with(~paste(., "2018",sep = "_"))
ls(historic_2018)

##Update report area codes S08000018, S08000021, S08000023 & S08000027. ####
#These have changed in between 2018 and 2024.  This applied to both 'level' = 'NHS board of treatment' & 'NHS board of residence'.
table(historic_2018$report_area_2018)
historic_2018$report_area_2018[historic_2018$report_area_2018 == "S08000018"] <- "S08000029" #NHS Fife
historic_2018$report_area_2018[historic_2018$report_area_2018 == "S08000021"] <- "S08000031" #NHS Greater Glasgow & Clyde
historic_2018$report_area_2018[historic_2018$report_area_2018 == "S08000023"] <- "S08000032" #NHS Lanarkshire
historic_2018$report_area_2018[historic_2018$report_area_2018 == "S08000027"] <- "S08000030" #NHS Tayside
table(historic_2018$report_area_2018)

##Join 2018 output onto 2024 output####

output2 <- output %>% 
  left_join(historic_2018,by = c(`2018_question` = "question_2018","level" = "level_2018","report_area"= "report_area_2018",
                                 `2018_option` = "response_option_2018"),suffix=c("_2024","")) 
                    

##2015####
historic_2015 <- readRDS(paste0(data_path_2015,"output_2015.rds"))
ls(historic_2015)
historic_2015 <- historic_2015 %>% 
  select(question,level,report_area_name,report_area,response_option,response_text_analysis,n_includedresponses,n_response,wgt_percent,wgt_percent_low,wgt_percent_upp)%>%
  rename_with(~paste(., "2015",sep = "_"))
ls(historic_2015)

##Update report area codes S08000018, S08000021, S08000023 & S08000027. ####
#These have changed in between 2015 and 2024.  This applied to both 'level' = 'NHS board of treatment' & 'NHS board of residence'.
table(historic_2015$report_area_2015)
historic_2015$report_area_2015[historic_2015$report_area_2015 == "S08000018"] <- "S08000029" #NHS Fife
historic_2015$report_area_2015[historic_2015$report_area_2015 == "S08000021"] <- "S08000031" #NHS Greater Glasgow & Clyde
historic_2015$report_area_2015[historic_2015$report_area_2015 == "S08000023"] <- "S08000032" #NHS Lanarkshire
historic_2015$report_area_2015[historic_2015$report_area_2015 == "S08000027"] <- "S08000030" #NHS Tayside
table(historic_2015$report_area_2015)

##Join 2015 output onto 2024 & 2018 output####
output3 <- output2 %>% 
  left_join(historic_2015,by = c(`2015_question` = "question_2015","level" = "level_2015","report_area"= "report_area_2015",
                                 `2015_option` = "response_option_2015"),suffix=c("_2024","")) 

ls(output3)
output3 <- output3 %>%
  select(-percent,-`2018_question`,-`2015_question`,-report_area_name_2018,-response_text_analysis_2018, -report_area_name_2015,-response_text_analysis_2015)
ls(output3)
output3 <- output3 %>%
  select(question,question_text,response_option,response_text_analysis,response_text_dashboard,topic,question_type,level,report_area,report_area_name,
         n_includedresponses,n_wgt_includedresponses,n_response,n_wgt_response,wgt_percent,`wgt_percent_low`,`wgt_percent_upp`,
         n_includedresponses_2018,n_response_2018,wgt_percent_2018,wgt_percent_low_2018,wgt_percent_upp_2018,comparability_2018,
         n_includedresponses_2015,n_response_2015,wgt_percent_2015,wgt_percent_low_2015,wgt_percent_upp_2015,comparability_2015)


#Correct CIs so that all in the range (0,1)
sum(output3$wgt_percent_upp > 1,na.rm = TRUE)
table(round(output3$wgt_percent_upp[output3$wgt_percent_upp > 1],2))
output3 <- output3 %>% 
  mutate(across(matches("_low"),function(x) if_else(x < 0, x*0,x)),
         across(matches("_upp"),function(x) if_else(x > 1, x/x,x)))
#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"output_2024.rds")) 
all.equal(hist.file,output3)  

##save out####
saveRDS(output3, paste0(analysis_output_path,"output_2024.rds"))
write.xlsx(output3,paste0(analysis_output_path,"output_2024.xlsx"))

rm(historic_2015,historic_2018,output,output2,output3,hist.file)

#Cancer group output####
##Read in 2024 cancer_group output.rds#### 
cancer_group_output <- readRDS(paste0(analysis_output_path,"cancer_group_output.rds"))
#Amend 2018 question for q46 (`2018_question`) to ensure 2024 match up with historic_2018
cancer_group_output <- cancer_group_output %>%
  mutate(`2018_question` = if_else((`2018_question` == "q53a") | (`2018_question` == "q53b") | (`2018_question` == "q53c") | (`2018_question` == "q53d") | 
                                     (`2018_question` == "q53e") | (`2018_question` == "q53f") | (`2018_question` == "q53g") | (`2018_question` == "q53h") | 
                                     (`2018_question` == "q53i") | (`2018_question` == "q53j" ),"q53",`2018_question`))

cancer_group_output <- cancer_group_output %>% 
  left_join(question_lookup,by = c("question","response_option","2018_question","2015_question"))
ls(cancer_group_output)

#Amend the 2018 response_option to "-" for q61 to not match on the 2018 "At least one disability / impairment / condition" measure which is not in 2024
cancer_group_output <- cancer_group_output %>%
  mutate(`2018_option` = if_else(question == "q61","-",`2018_option`))

#Where response_text_dashboard = NA, this should equal response_text_analysis
cancer_group_output$check = 0
cancer_group_output <- cancer_group_output %>%
  mutate(check = if_else(is.na(response_text_dashboard),1,check))
table(cancer_group_output$check)
cancer_group_output <- cancer_group_output %>%
  mutate(response_text_dashboard = if_else(is.na(response_text_dashboard),response_text_analysis,response_text_dashboard))

##2018####
cancer_group_historic_2018 <- readRDS(paste0(data_path_2018,"cancer_group_output.rds"))
#cancer_group_historic_2018 <- read.xlsx(paste0(data_path_2018,"cancer_group_output_2018.xlsx"))
ls(cancer_group_historic_2018)
cancer_group_historic_2018 <- cancer_group_historic_2018 %>% 
  select(question,level,report_area,response_option,response_text_analysis,n_includedresponses,n_response,wgt_percent,wgt_percent_low,wgt_percent_upp)%>%
  rename_with(~paste(., "2018",sep = "_"))
ls(cancer_group_historic_2018)

##Join 2018 tumour output onto 2024 tumour output####
cancer_group_output2 <- cancer_group_output %>% 
  left_join(cancer_group_historic_2018,by = c(`2018_question` = "question_2018","level" = "level_2018","report_area"= "report_area_2018",
                                              `2018_option` = "response_option_2018"),suffix=c("_2024","")) 

##2015####
cancer_group_historic_2015 <- readRDS(paste0(data_path_2015,"cancer_group_output.rds"))
ls(cancer_group_historic_2015)
cancer_group_historic_2015 <- cancer_group_historic_2015 %>% 
  select(question,level,report_area,response_option,response_text_analysis,n_includedresponses,n_response,wgt_percent,wgt_percent_low,wgt_percent_upp)%>%
  rename_with(~paste(., "2015",sep = "_"))
ls(cancer_group_historic_2015)

##Join 2015 cancer_group onto 2024 & 2018 cancer_group output####
cancer_group_output3 <- cancer_group_output2 %>% 
  left_join(cancer_group_historic_2015,by = c(`2015_question` = "question_2015","level" = "level_2015","report_area"= "report_area_2015",
                                              `2015_option` = "response_option_2015"),suffix=c("_2024","")) 

ls(cancer_group_output3)
cancer_group_output3 <- cancer_group_output3 %>%
  select(-`2018_question`,-`2015_question`,-response_text_analysis_2018, -response_text_analysis_2015)
ls(cancer_group_output3)
cancer_group_output3 <- cancer_group_output3 %>%
  select(question,question_text,response_option,response_text_analysis,response_text_dashboard,topic,question_type,level,report_area,
         n_includedresponses,n_wgt_includedresponses,n_response,n_wgt_response,wgt_percent,wgt_percent_low,wgt_percent_upp,
         n_includedresponses_2018,n_response_2018,wgt_percent_2018,wgt_percent_low_2018,wgt_percent_upp_2018,comparability_2018,
         n_includedresponses_2015,n_response_2015,wgt_percent_2015,wgt_percent_low_2015,wgt_percent_upp_2015,comparability_2015) 

#check if the same as before
hist.file <- readRDS(paste0(analysis_output_path,"cancer_group_output_2024.rds")) 
all.equal(hist.file,cancer_group_output3)  
  
##save out####
saveRDS(cancer_group_output3, paste0(analysis_output_path,"cancer_group_output_2024.rds"))
write.xlsx(cancer_group_output3,paste0(analysis_output_path,"cancer_group_output_2024.xlsx"))

rm(cancer_group_historic_2015,cancer_group_historic_2018,cancer_group_output,cancer_group_output2,cancer_group_output3,hist.file,question_lookup)
