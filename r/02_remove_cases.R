## Name: 02_remove_cases.R
## Description: Remove cases
## Input file: raw_sim
## Functions: 
## Output file: sim_result

# Packages ----------------------------------------------------------------
library(stringr)
library(magrittr)
library(dplyr)
library(simulacr)

# Source user written scripts ---------------------------------------------
source('r/functions/create_reported_cases_list.R')
source('r/functions/simulate_reported_cases_unique.R')
source('r/functions/remove_n_cases_tidyverse.R')
# scenario_ref <- NULL
if(!exists("scenario_ref")) {
  scenario_ref <- "r_mean_2_11"
  
  # Create_paths ---------------------------------------------
  scenario_path <- file.path("scenarios", scenario_ref)
  data_path <- file.path("data", "processing", scenario_path)
  dir.create(data_path, showWarnings = F)
  outputs_path <- file.path("outputs", scenario_path)
  dir.create(outputs_path, showWarnings = F)
}

# Define simulation settings ----------------------------------------------

seed <- 765

# Take a scenario - an outbreak size
# Then take the specific simulation
# Then filter by the outbreak days? Probs not needed. 
# Then remove cases according to the 3 levels
# data_path <- "data/processing/scenarios/r_mean_1_36"

raw_sims <- list.files(path = data_path, pattern = "raw_sim", 
                       full.names = TRUE)
raw_sims_filenames <- raw_sims

raw_names <- str_extract(raw_sims,
                         pattern = "\\d+[_]\\d+[-]\\d+")


names(raw_sims) <- raw_names

set.seed(4582)
combined_results_list <- list()

for (i in 1:length(raw_sims)) {
      current_name <- names(raw_sims[i])
      print(current_name)
      current_scenario <- qs::qread(raw_sims[[i]])
      num_sims <- length(raw_sims[[i]])
    for (outbreak_days in 365) {
      print(outbreak_days)
      for (prop_ur in c(0.25, 0.5, 0.75)) {
        print(prop_ur)
        sim_results <- lapply(
          current_scenario,
          create_reported_cases_list, 
          prop_ur, 
          outbreak_days = outbreak_days)
        
        csim <- do.call("rbind", sim_results)
        lname <- paste(as.character(prop_ur), 
                       as.character(outbreak_days), 
                       sep = "_")
        combined_results_list[[lname]] <- csim
      }
    }
    
      
  

  combined_results <- do.call("rbind", combined_results_list)
  combined_results$seed <- seed
  timestring <- format(Sys.time(), "%d_%m_%s")

  results_filename <- file.path(data_path, paste0(
                             "sim_results_", 
                             current_name ,
                             "_", 
                             timestring,
                             ".qs"))
  qs::qsave(combined_results, results_filename)
  print(results_filename)
}

