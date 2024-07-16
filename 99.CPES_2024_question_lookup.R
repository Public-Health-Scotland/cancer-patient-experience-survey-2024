# Name of file: 99.CPES_2024_question_lookup.R
# 
# Original author(s): Catriona Haddow
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Create metadata for each question in CPES 2024 survey
# 
# Approximate run time: <1 min
#  
# Approximate memory usage: 1.26 GiB

#Inputs: lookup_path,"CPES_2024_question_mapping.xlsx"

#Outputs:
#"lookups/question_lookup.rds"
#"lookups/questions.rds"
#"lookups/allocation_questions.rds"

source("00.CPES_2024_set_up_packages.R")
source("00.CPES_2024_set_up_file_paths.R")

#Read in document
question_mapping <- read.xlsx(paste0(lookup_path,"CPES_2024_question_mapping.xlsx"),sheet = "2024_CPES_question_mapping")

question_mapping <- question_mapping %>%
  mutate(across(everything(), as.character))%>%
  rename_with(tolower) %>% #all lower case
  mutate(question = paste0("q",questionnumber)) %>% 
  rename(question_text = 'questiontext',
         question_type = question.type,
         response_option = response.options,
         response_text = response.text,
         cancercentreallocation = cancer.centre.allocation) 

table(question_mapping$question[str_detect(question_mapping$cancercentreallocation,"q") == TRUE],
      question_mapping$cancercentreallocation[str_detect(question_mapping$cancercentreallocation,"q") == TRUE])
table(question_mapping$question[str_detect(question_mapping$question_type,"llocation") == TRUE],
      question_mapping$cancercentreallocation[str_detect(question_mapping$question_type,"llocation") == TRUE])

table(question_mapping$response_option,useNA = c("always"))
table(question_mapping$question_type,useNA = c("always"))

#create question lookup
question_lookup <- question_mapping %>%
  select(question,question_type,question_text,response_option,response_text,topic,cancercentreallocation,weighted,pnn,
         `2018_question`,`2018_option`,`2015_question`,`2015_option`) %>%
  rename(response_value = pnn) %>%
  mutate(response_text_analysis = coalesce(response_value,response_text)) # for 04.CPES_2024_create_aggregate_results.R

#Read in addendum mapping document which caters for the design of questions q47, q50 and q51 (Future proofing for codes 01, 02, 03 and 04)
question_mapping_addendum <- read.xlsx(paste0(lookup_path,"CPES_2024_question_mapping.xlsx"),sheet = "Additional_mapping (info-TATA)")
question_lookup <- rbind(question_lookup,question_mapping_addendum)
question_lookup <- question_lookup %>%
  filter(question != 'q47a01' & question !='q47a02' & question !='q47a03' & question != 'q47a04' 
         & question !='q47a05' & question !='q47a06' & question != 'q47a07' &
           question != 'q47b01' & question !='q47b02' & question !='q47b03' & question != 'q47b04' 
         & question !='q47b05' & question !='q47b06' & question != 'q47b07' &
           question != 'q50a' & question !='q50b' & question !='q50c' & question != 'q50d' & question != 'q50e' &
           question != 'q51a' & question !='q51b' & question !='q51c' & question != 'q51d' & question != 'q51e')

#check if the same as before
hist.file <- readRDS(paste0(lookup_path,"question_lookup.rds"))
identical(hist.file,question_lookup)
saveRDS(question_lookup, paste0(lookup_path,"question_lookup.rds"))
write.xlsx(question_lookup, paste0(lookup_path,"question_lookup.xlsx"))

allocation_questions <- unique(question_lookup$question[str_detect(question_mapping$question_type,"llocation") == TRUE])
questions <- unique(question_lookup$question[question_lookup$question_type != "-"])
saveRDS(allocation_questions, paste0(lookup_path,"allocation_questions.rds"))
saveRDS(questions, paste0(lookup_path,"questions.rds"))