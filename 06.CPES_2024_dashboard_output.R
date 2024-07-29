# Name of file: 06.CPES_2024_dashboard_output.R
# 
# Original author(s): Martin Leitch
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  output analyses at all levels of reporting.
# 
# Approximate run time: <1min
# 
# Approximate memory usage: 598 MiB
# 
# *****************************************

#Inputs: 
#analysis_output_path,"output_2024.rds"
#analysis_output_path,"cancer_group_output_2024.rds"

#Outputs: 
#analysis_output_path,"output_2024_dashboard.xlsx"
#analysis_output_path,"output_2024_dashboard.rds"
#analysis_output_path,"cancer_group_output_2024_dashboard.xlsx"
#analysis_output_path,"cancer_group_output_2024_dashboard.rds"

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Geographical dashboard####
##Read in output_2024.rds####
geography <- readRDS(paste0(analysis_output_path,"output_2024.rds"))
ls(geography)

#remove 2018 and 2015 data that is incomparable with 2024 in the dashboard
geography$remove_2018[geography$comparability_2018 != "Dashboard"] <- 1
geography <- geography %>% 
  mutate(n_includedresponses_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ n_includedresponses_2018),
         n_response_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ n_response_2018),
         wgt_percent_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ wgt_percent_2018),
         wgt_percent_low_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ wgt_percent_low_2018),
         wgt_percent_upp_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ wgt_percent_upp_2018))
geography$remove_2015[geography$comparability_2015 != "Dashboard"] <- 1
geography <- geography %>% 
  mutate(n_includedresponses_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ n_includedresponses_2015),
         n_response_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ n_response_2015),
         wgt_percent_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ wgt_percent_2015),
         wgt_percent_low_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ wgt_percent_low_2015),
         wgt_percent_upp_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ wgt_percent_upp_2015))
geography$remove_2018 <- NULL
geography$comparability_2018 <- NULL
geography$remove_2015 <- NULL
geography$comparability_2015 <- NULL

#Rounding to 2 decimal places 
geography$wgt_percent = round(geography$wgt_percent,2)
geography$wgt_percent_low = round(geography$wgt_percent_low,2)
geography$wgt_percent_upp = round(geography$wgt_percent_upp,2)
geography$wgt_percent_2018 = round(geography$wgt_percent_2018,2)
geography$wgt_percent_low_2018 = round(geography$wgt_percent_low_2018,2)
geography$wgt_percent_upp_2018 = round(geography$wgt_percent_upp_2018,2)
geography$wgt_percent_2015 = round(geography$wgt_percent_2015,2)
geography$wgt_percent_low_2015 = round(geography$wgt_percent_low_2015,2)
geography$wgt_percent_upp_2015 = round(geography$wgt_percent_upp_2015,2)

#remove areas that are not published - less than 50 responses OVERALL. 
geography$remove <- 0
geography$remove <- ifelse(geography$n_response < 50,1,0)
geography$wgt_percent[geography$remove == 1] <- NA
geography$wgt_percent_low[geography$remove == 1] <- NA
geography$wgt_percent_upp[geography$remove == 1] <- NA
geography$n_includedresponses_2018[geography$suppress == 1] <- NA
geography$wgt_percent_2018[geography$remove == 1] <- NA
geography$wgt_percent_low_2018[geography$remove == 1] <- NA
geography$wgt_percent_upp_2018[geography$remove == 1] <- NA
geography$n_includedresponses_2015[geography$suppress == 1] <- NA
geography$wgt_percent_2015[geography$remove == 1] <- NA
geography$wgt_percent_low_2015[geography$remove == 1] <- NA
geography$wgt_percent_upp_2015[geography$remove == 1] <- NA
#also remove N_IncludedResponses?
geography$remove <- NULL

#Suppress questions with responses less than 20.
geography$suppress <- 0
geography$suppress <- ifelse(geography$n_includedresponses < 20,1,0)
geography$wgt_percent[geography$suppress == 1] <- NA
geography$wgt_percent_low[geography$suppress == 1] <- NA
geography$wgt_percent_upp[geography$suppress == 1] <- NA
geography$n_includedresponses_2018[geography$suppress == 1] <- NA
geography$wgt_percent_2018[geography$suppress == 1] <- NA
geography$wgt_percent_low_2018[geography$suppress == 1] <- NA
geography$wgt_percent_upp_2018[geography$suppress == 1] <- NA
geography$n_includedresponses_2015[geography$suppress == 1] <- NA
geography$wgt_percent_2015[geography$suppress == 1] <- NA
geography$wgt_percent_low_2015[geography$suppress == 1] <- NA
geography$wgt_percent_upp_2015[geography$suppress == 1] <- NA
#also suppress N_IncludedResponses? 
geography$suppress <- NULL

geography <- geography %>%
  select(question,question_text,response_option,response_text_analysis,question_type,level,report_area,report_area_name,
         n_includedresponses,n_wgt_includedresponses,n_response,n_wgt_response,wgt_percent,`wgt_percent_low`,`wgt_percent_upp`,
         n_response_2018,wgt_percent_2018,wgt_percent,wgt_percent_low_2018,wgt_percent_upp_2018,
         n_response_2015,wgt_percent_2015,wgt_percent,wgt_percent_low_2015,wgt_percent_upp_2015)

##save out####
saveRDS(geography, paste0(analysis_output_path,"dashboard_output_2024.rds"))
write.xlsx(geography,paste0(analysis_output_path,"dashboard_output_2024.xlsx"))

#Cancer group dashboard####
##Read in tumour_output_2024.rds####
cancer_group_output <- readRDS(paste0(analysis_output_path,"cancer_group_output_2024.rds"))
ls(cancer_group_output)

#remove 2018 and 2015 data that is incomparable with 2024 in the dashboard
cancer_group_output$remove_2018[cancer_group_output$comparability_2018 != "Dashboard"] <- 1
cancer_group_output <- cancer_group_output %>% 
  mutate(n_includedresponses_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ n_includedresponses_2018),
         n_response_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ n_response_2018),
         wgt_percent_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ wgt_percent_2018),
         wgt_percent_low_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ wgt_percent_low_2018),
         wgt_percent_upp_2018 = case_when(remove_2018 == 1 ~ NA, TRUE ~ wgt_percent_upp_2018))
cancer_group_output$remove_2015[cancer_group_output$comparability_2015 != "Dashboard"] <- 1
cancer_group_output <- cancer_group_output %>% 
  mutate(n_includedresponses_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ n_includedresponses_2015),
         n_response_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ n_response_2015),
         wgt_percent_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ wgt_percent_2015),
         wgt_percent_low_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ wgt_percent_low_2015),
         wgt_percent_upp_2015 = case_when(remove_2015 == 1 ~ NA, TRUE ~ wgt_percent_upp_2015))
cancer_group_output$remove_2018 <- NULL
cancer_group_output$comparability_2018 <- NULL
cancer_group_output$remove_2015 <- NULL
cancer_group_output$comparability_2015 <- NULL

#Rounding to 2 decimal places 
cancer_group_output$wgt_percent = round(cancer_group_output$wgt_percent,2)
cancer_group_output$wgt_percent_low = round(cancer_group_output$wgt_percent_low,2)
cancer_group_output$wgt_percent_upp = round(cancer_group_output$wgt_percent_upp,2)
cancer_group_output$wgt_percent_2018 = round(cancer_group_output$wgt_percent_2018,2)
cancer_group_output$wgt_percent_low_2018 = round(cancer_group_output$wgt_percent_low_2018,2)
cancer_group_output$wgt_percent_upp_2018 = round(cancer_group_output$wgt_percent_upp_2018,2)
cancer_group_output$wgt_percent_2015 = round(cancer_group_output$wgt_percent_2015,2)
cancer_group_output$wgt_percent_low_2015 = round(cancer_group_output$wgt_percent_low_2015,2)
cancer_group_output$wgt_percent_upp_2015 = round(cancer_group_output$wgt_percent_upp_2015,2)

#remove areas that are not published - less than 50 responses OVERALL. 
cancer_group_output$remove <- 0
cancer_group_output$remove <- ifelse(cancer_group_output$n_response < 50,1,0)
cancer_group_output$wgt_percent[cancer_group_output$remove == 1] <- NA
cancer_group_output$`wgt_percent_low`[cancer_group_output$remove == 1] <- NA
cancer_group_output$`wgt_percent_upp`[cancer_group_output$remove == 1] <- NA
cancer_group_output$wgt_percent_2018[cancer_group_output$remove == 1] <- NA
cancer_group_output$`wgt_percent_low_2018`[cancer_group_output$remove == 1] <- NA
cancer_group_output$`wgt_percent_upp_2018`[cancer_group_output$remove == 1] <- NA
cancer_group_output$wgt_percent_2015[cancer_group_output$remove == 1] <- NA
cancer_group_output$`wgt_percent_low_2015`[cancer_group_output$remove == 1] <- NA
cancer_group_output$`wgt_percent_upp_2015`[cancer_group_output$remove == 1] <- NA
#also remove N_IncludedResponses? NO
cancer_group_output$remove <- NULL

#Suppress questions with responses less than 20.
cancer_group_output$suppress <- 0
cancer_group_output$suppress <- ifelse(cancer_group_output$n_includedresponses < 20,1,0)
cancer_group_output$wgt_percent[cancer_group_output$suppress == 1] <- NA
cancer_group_output$`wgt_percent_low`[cancer_group_output$suppress == 1] <- NA
cancer_group_output$`wgt_percent_upp`[cancer_group_output$suppress == 1] <- NA
cancer_group_output$wgt_percent_2018[cancer_group_output$suppress == 1] <- NA
cancer_group_output$`wgt_percent_low_2018`[cancer_group_output$suppress == 1] <- NA
cancer_group_output$`wgt_percent_upp_2018`[cancer_group_output$suppress == 1] <- NA
cancer_group_output$wgt_percent_2015[cancer_group_output$suppress == 1] <- NA
cancer_group_output$`wgt_percent_low_2015`[cancer_group_output$suppress == 1] <- NA
cancer_group_output$`v_upp_2015`[cancer_group_output$suppress == 1] <- NA
#also suppress N_IncludedResponses? NO
cancer_group_output$suppress <- NULL

cancer_group_output <- cancer_group_output %>%
  select(question,question_text,response_option,response_text_analysis,question_type,level,report_area,
         n_includedresponses,n_wgt_includedresponses,n_response,n_wgt_response,wgt_percent,`wgt_percent_low`,`wgt_percent_upp`,
         n_response_2018,wgt_percent_2018,wgt_percent_low_2018,wgt_percent_upp_2018,
         n_response_2015,wgt_percent_2015,wgt_percent_low_2015,wgt_percent_upp_2015)

##save out####
saveRDS(cancer_group_output, paste0(analysis_output_path,"cancer_group_dashboard_output_2024.rds"))
write.xlsx(cancer_group_output,paste0(analysis_output_path,"cancer_group_dashboard_output_2024.xlsx"))