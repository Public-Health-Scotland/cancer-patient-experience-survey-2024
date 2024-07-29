###set vectors of report areas
report_areas <- c("scotland","network_of_tx","network_of_residence_tx","board_of_tx" ,"board_of_residence_tx")
report_area_wt <- c("nat_wt","nett_wt","netr_wt","hbt_wt","hbr_wt")
sg_variables <- c("sex","smr01_sex_label","age_group_chi","simd2020v2_sc_quintile_smr01","ur6_2020_smr01","ur6_2020_name_smr01",
                  "hscp2019","hscp2019name","board_of_tx","board_of_treatment","board_of_residence_tx","board_of_residence",
                  "network_of_tx","network_of_residence_tx","smr01_locname","cancer_group_smr06","smr06_stage",
                  "smr06_method_1st_detection","smr06_method_1st_detection_description","nat_wt","nett_wt","netr_wt","hbt_wt","hbr_wt")


###set vectors of questions numbers
questions <- readRDS(paste0(lookup_path,"questions.rds"))
allocation_questions <- readRDS(paste0(lookup_path,"allocation_questions.rds"))

#age groups

two_age_bands <- function(age) {
  two_age_bands <- character(length(age))
  two_age_bands <- case_when(age >=16 & age <= 64 ~ "16-64",
                             age >=65 & age <= 150 ~ "65 plus",
                             TRUE ~ "Dummy")
  return(two_age_bands)}

three_age_bands <- function(age) {
  three_age_bands <- character(length(age))
  three_age_bands <- case_when(age >=16 & age <= 64 ~ "16-64",
                               age >=65 & age <= 74 ~ "65-74",
                               age >=75 & age <= 150 ~ "75 plus",
                               TRUE ~ "Dummy")
  return(three_age_bands)}

four_age_bands <- function(age) {
  three_age_bands <- character(length(age))
  three_age_bands <- case_when(age >=16 & age <= 54 ~ "16-54",
                               age >=55 & age <= 64 ~ "55-64",
                               age >=65 & age <= 74 ~ "65-74",
                               age >=75 & age <= 150 ~ "75 plus",
                               TRUE ~ "Dummy")
  return(three_age_bands)}

five_age_bands <- function(age) {
  five_age_bands <- character(length(age))
  five_age_bands <- case_when(age >=16 & age <= 44 ~ "16-54",
                              age >=45 & age <= 54 ~ "45-54",
                              age >=55 & age <= 64 ~ "55-64",
                              age >=65 & age <= 74 ~ "65-74",
                              age >=75 & age <= 150 ~ "75 plus",
                              TRUE ~ "Dummy")
  return(five_age_bands)}

seven_age_bands <- function(age) {
  seven_age_band <- character(length(age))
  seven_age_band <- case_when(age >=17 & age <= 24 ~ "16-24",
                              age >=25 & age <= 34 ~ "25-34",
                              age >=35 & age <= 44 ~ "35-44",
                              age >=45 & age <= 54 ~ "45-54",
                              age >=55 & age <= 64 ~ "55-64",
                              age >=65 & age <= 74 ~ "65-74",
                              age >=75 & age <= 150 ~ "75 plus",
                              TRUE ~ "Dummy")
  return(seven_age_band)}
#calculate Confidence Intervals
#https://tidy-survey-r.github.io/tidy-survey-book/c10-sample-designs-replicate-weights.html#common-sampling-designs
#se(^p) =√(^p(1−^p)/(n-1))
#CI = estimate±t∗df×SE

add_CIs_proportion <- function(df,p,n) {
  df <- df %>% 
    mutate(se = sqrt((({{p}}*(1-{{p}}))/({{n}} - 1))),
           t = qt(0.95,df={{n}}-1)) %>% 
    mutate(wgt_percent_low = {{p}} - t*se,
           wgt_percent_upp = {{p}} + t*se) %>% 
    mutate(wgt_percent_low = if_else(wgt_percent_low < 0,0,wgt_percent_low),
           wgt_percent_upp = if_else(wgt_percent_upp > 1,1,wgt_percent_upp),
           wgt_percent_low = if_else(wgt_percent == 1,1,wgt_percent_low),
           wgt_percent_upp = if_else(wgt_percent == 1,1,wgt_percent_upp))}

#define census_date
census_date = as.Date("2024-01-18")

