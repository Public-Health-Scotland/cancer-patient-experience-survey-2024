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
#fix cancer networks names as can't work with numeric field. This doesn't belong here, but will move later.
responses_longer <- responses_longer %>% 
  mutate(network_of_tx = as.character(haven::as_factor(network_of_tx)),
         network_of_residence_tx = as.character(haven::as_factor(network_of_residence_tx)),
         no_wt = 1)
table(responses_longer$response_option)

#read in lookups
hb_names <- read.csv(paste0(lookup_path,"SMRA.ANALYSIS.HEALTH_BOARD.csv"))%>% 
  select(GRO_HB9_2019,DESCRIPTION) %>% 
  rename(hb_code = GRO_HB9_2019) %>% 
  rename(hb_name = DESCRIPTION) %>% 
  filter(hb_code != "")

#haven::print_labels(x= responses_longer$board_of_tx)
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds")) %>% select(-response_value,-response_text) #read in lookup 
responses_longer <- responses_longer %>% 
  left_join(question_lookup,by = c("question","response_option","cancercentreallocation")) %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude")) 

question_lookup <- question_lookup %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude")) %>% 
  select(question,question_text,response_option,response_text_analysis,topic) %>% 
  distinct()

#define the aggregate function.####
aggregate_responses <- function(report_areas,wt) {
  responses_longer <- responses_longer %>%
    mutate(response = 1) %>% 
    as_survey_design(weights = {{wt}}) %>% 
    group_by("report_area" = {{report_areas}},question,response_text_analysis) %>%
    summarise(survey_prop(na.rm = TRUE,vartype = c("ci"),level = 0.95,proportion = TRUE,deff = FALSE),
              n_response = n(),
              n_wgt_response = sum({{wt}})) %>% 
    group_by(report_area,question) %>%
    mutate(n_includedresponses = sum(n_response),
           n_wgt_includedresponses = sum(n_wgt_response))}

#define the expand table function.####
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

df <- aggregate_responses(tumour_group_text,nat_wt)
tmt <- expand_table(df)%>%  mutate(level = "Tumour group")

saveRDS(nat, paste0(analysis_output_path,"nat.rds"))
saveRDS(nett, paste0(analysis_output_path,"nett.rds"))
saveRDS(netr, paste0(analysis_output_path,"netr.rds"))
saveRDS(hbt, paste0(analysis_output_path,"hbt.rds"))
saveRDS(hbr, paste0(analysis_output_path,"hbr.rds"))
saveRDS(cc, paste0(analysis_output_path,"cc.rds"))
saveRDS(tmt, paste0(analysis_output_path,"tmt.rds"))

################

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
  rename(wgt_percent = coef) %>% 
  mutate(percent = n_response / n_includedresponses)
saveRDS(output, paste0(analysis_output_path,"provisional_output.rds"))

#######

nat <- readRDS(paste0(analysis_output_path,"nat.rds"))
nett <- readRDS(paste0(analysis_output_path,"nett.rds"))
netr <- readRDS(paste0(analysis_output_path,"netr.rds"))
hbt <- readRDS(paste0(analysis_output_path,"hbt.rds"))
hbr <- readRDS(paste0(analysis_output_path,"hbr.rds"))
cc <- readRDS(paste0(analysis_output_path,"cc.rds"))
tmt <- readRDS(paste0(analysis_output_path,"tmt.rds"))
output <- readRDS(paste0(analysis_output_path,"provisional_output.rds"))
question_lookup <- readRDS(paste0(lookup_path,"question_lookup.rds"))  #read in lookup again to get response option

question_lookup <- question_lookup %>% 
  filter(!response_text_analysis %in% c(NA,"Exclude")) %>% 
  group_by(question,question_type,response_text_analysis,`2018_question`,`2015_question`,response_value) %>% 
  summarise(response_option = first(response_option))
#`2018_question`,`2018_option`,`2015_question`,`2015_option`

output <- output %>% 
  select(-hb_name) %>% 
  left_join(question_lookup, by = c("question","response_text_analysis","response_option")) %>% 
  arrange(level,report_area,question,response_option) %>%
  filter(!question_type %in% c(NA)) # to remove duplicated negative values 

table(output$report_area_name,useNA = c("always"))

tumour_output <- distinct(bind_rows(nat,tmt)) %>% 
  left_join(question_lookup, by = c("question","response_text_analysis","response_option")) %>% 
  arrange(level,report_area,question,response_option) %>%
  filter(!question_type %in% c(NA)) # to remove duplicated negative values

write.xlsx(output,paste0(analysis_output_path,"output.xlsx"))
saveRDS(output, paste0(analysis_output_path,"output.rds"))
write.xlsx(tumour_output,paste0(analysis_output_path,"tumour_output.xlsx"))
saveRDS(tumour_output, paste0(analysis_output_path,"tumour_output.rds"))