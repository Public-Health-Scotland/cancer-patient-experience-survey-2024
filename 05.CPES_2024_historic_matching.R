# Name of file: 05.CPES_2024_historic_matching.R
# 
# Original author(s): Martin Leitch
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  output analyses at all levels of reporting.
# 
# Approximate run time: tbd
# 
# Approximate memory usage: tbd
# 
# *****************************************

#Inputs: 
#analysis_output_path,"output.rds"
#data_path_2018,"output.rds"
#data_path_2015,"output.rds"
##analysis_output_path,"tumour_output.rds"
#data_path_2018,"tumour_output.rds"
#data_path_2015,"tumour_output.rds"

#Outputs: 
#analysis_output_path,"output_2024.xlsx"
#analysis_output_path,"output_2024.rds"
#analysis_output_path,"tumour_output_2024.xlsx"
#analysis_output_path,"tumour_output_2024.rds"

#Join historical results for 2018 and 2015 on to the 2024 results####
##Read in historic data for 2018 and 2015 and then match onto the 2024 results files ("output" & "tumour_output")
##Need to do separately for output and tumour output equivalent files.

##Read in 2024 output.rds####
output <- readRDS(paste0(analysis_output_path,"output.rds"))

##2018####
historic_2018 <- readRDS(paste0(data_path_2018,"output.rds"))
ls(historic_2018)
historic_2018 <- historic_2018 %>% 
  select(question,level,report_area_name,report_area,response_option,response_text_analysis,n_includedresponses,wgt_percent,'_low','_upp')%>%
  rename(question_2018 = question) %>%
  rename(level_2018 = level) %>%
  rename(report_area_name_2018 = report_area_name) %>%
  rename(report_area_2018 = report_area) %>%
  rename(response_option_2018 = response_option) %>%
  rename(response_text_analysis_2018 = response_text_analysis) %>%
  rename(n_includedresponses_2018 = n_includedresponses) %>%
  rename(wgt_percent_2018 = wgt_percent) %>%
  rename('_low_2018' = '_low') %>%
  rename( '_upp_2018' = '_upp')
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
                                 response_option = "response_option_2018"),suffix=c("_2024","")) 

##2015####
historic_2015 <- readRDS(paste0(data_path_2015,"output.rds"))
ls(historic_2015)
historic_2015 <- historic_2015 %>% 
  select(question,level,report_area_name,report_area,response_option,response_text_analysis,n_includedresponses,wgt_percent,'_low','_upp')%>%
  rename(question_2015 = question) %>%
  rename(level_2015 = level) %>%
  rename(report_area_name_2015 = report_area_name) %>%
  rename(report_area_2015 = report_area) %>%
  rename(response_option_2015 = response_option) %>%
  rename(response_text_analysis_2015 = response_text_analysis) %>%
  rename(n_includedresponses_2015 = n_includedresponses) %>%
  rename(wgt_percent_2015 = wgt_percent) %>%
  rename('_low_2015' = '_low') %>%
  rename( '_upp_2015' = '_upp')
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
                                 response_option = "response_option_2015"),suffix=c("_2024","")) 

#Temporary - eventually remove
rm(historic_2018)
rm(historic_2015)
rm(output)
rm(output2)
rm(output3)



