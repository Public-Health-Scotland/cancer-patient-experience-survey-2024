# Name of file: 03.CPES_2024_create_responses_longer.R
# 
# Original author(s): Catriona Haddow 
#   
# Written/run on: Posit Workbench - RStudio R 4.1.2
# 
# Description of content:  Read in responses, restructure to be longer, and allocate cancer centre
# 
# Approximate run time: 
# 
# Approximate memory usage: 
#Inputs:

#Outputs: 

source("00.set_up_packages.R")
source("00.set_up_file_paths.R")
source("00.functions.R")

#read in results data####
responses_with_weights <- readRDS(paste0(analysis_output_path,"responses_with_weights.rds")) 

