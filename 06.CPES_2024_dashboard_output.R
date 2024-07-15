# Name of file: 06.CPES_2024_dashboard_output.R
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
#analysis_output_path,"output_2024.rds"
#analysis_output_path,"tumour_output_2024.rds"
#lookup_path,"question_lookup.rds"

#Outputs: 
#analysis_output_path,"output_2024_dashboard.xlsx"
#analysis_output_path,"output_2024_dashboard.rds"
#analysis_output_path,"tumour_output_2024_dashboard.xlsx"
#analysis_output_path,"tumour_output_2024_dashboard.rds"

#Join historical results for 2018 and 2015 on to the 2024 results####
##Read in historic data for 2018 and 2015 and then match onto the 2024 results files ("output" & "tumour_output")
##Need to do separately for output and tumour output equivalent files.

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")
source("00.CPES_2024_functions.R")

#Geographical dashboard####
##Read in output_2024.rds####
geography <- readRDS(paste0(analysis_output_path,"output_2024.rds"))
ls(geography)

#Remove n_response (2024) 2018 & 2015 as they are not presented in Tableau.
geography$n_response <- NULL
geography$n_response_2018 <- NULL
geography$n_response_2015 <- NULL

#Rounding to 2 decimal places 
geography$wgt_percent = round(geography$wgt_percent,2)
geography$wgt_percent_2018 = round(geography$wgt_percent_2018,2)
geography$wgt_percent_2015 = round(geography$wgt_percent_2015,2)

#remove areas that are not published - less than 50 responses OVERALL. 
geography$remove <- 0
geography$remove <- ifelse(geography$n_response < 50,1,0)
geography$wgt_percent[geography$remove == 1] <- NA
geography$`_low`[geography$remove == 1] <- NA
geography$`_upp`[geography$remove == 1] <- NA
geography$wgt_percent_2018[geography$remove == 1] <- NA
geography$`_low_2018`[geography$remove == 1] <- NA
geography$`_upp_2018`[geography$remove == 1] <- NA
geography$wgt_percent_2015[geography$remove == 1] <- NA
geography$`_low_2015`[geography$remove == 1] <- NA
geography$`_upp_2015`[geography$remove == 1] <- NA
#also remove N_IncludedResponses? NO
geography$remove <- NULL

#Suppress questions with responses less than 20.
#Recode "wgt_percent","`_low`","`_upp",
#"wgt_percent_2018", "`_low_2018`","`_upp_2018`","wgt_percent_2015", "`_low_2015`","`_upp_2015`", =sysmis NA
geography$suppress <- 0
geography$suppress <- ifelse(geography$n_includedresponses < 20,1,0)
geography$wgt_percent[geography$suppress == 1] <- NA
geography$`_low`[geography$suppress == 1] <- NA
geography$`_upp`[geography$suppress == 1] <- NA
geography$wgt_percent_2018[geography$suppress == 1] <- NA
geography$`_low_2018`[geography$suppress == 1] <- NA
geography$`_upp_2018`[geography$suppress == 1] <- NA
geography$wgt_percent_2015[geography$suppress == 1] <- NA
geography$`_low_2015`[geography$suppress == 1] <- NA
geography$`_upp_2015`[geography$suppress == 1] <- NA
#also suppress N_IncludedResponses? NO
geography$suppress <- NULL

#Suppress questions with responses less than 20.
#Recode N_Response_2024 Wgt_Percent_Response_2024 N_Response_2022 Wgt_Percent_Response_2022 N_Response_2020 Wgt_Percent_Response_2020 
#       N_Response_2018 Wgt_Percent_Response_2018 N_Response_2016 Wgt_Percent_Response_2016 =sysmis NA
geography$suppress <- 0
geography$suppress <- ifelse(about_the_resp$N_IncludedResponses < 20,1,0)
geography$N_Response_2024[about_the_resp$suppress == 1] <- NA
geography$Percent_Response_2024[about_the_resp$suppress == 1] <- NA
#also suppress N_IncludedResponses? NO
geography$suppress <- NULL

geography <- geography %>%
  select(question,question_text,response_option,response_text_analysis,question_type,level,report_area,report_area_name,
         n_includedresponses,n_wgt_includedresponses,n_response,n_wgt_response,wgt_percent,`_low`,`_upp`,
         n_includedresponses_2018,wgt_percent_2018,`_low_2018`,`_upp_2018`,
         n_includedresponses_2015,wgt_percent_2015,`_low_2015`,`_upp_2015`,)

##save out####
saveRDS(geography, paste0(analysis_output_path,"draft_dashboard_output.rds"))
write.xlsx(geography,paste0(analysis_output_path,"draft_dashboard_output.xlsx"))

#Cancer group dashboard####
##Read in tumour_output_2024.rds####
group_output <- readRDS(paste0(analysis_output_path,"tumour_output_2024.rds"))