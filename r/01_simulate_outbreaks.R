## Name: 01_simulate_outbreaks.R
## Description: Simulate outbreaks 
## Input file: none
## Functions: 
## Output file: raw_sim

# Packages ----------------------------------------------------------------
library(simulacr)
library(foreach)

# Source user written scripts ---------------------------------------------
source('r/functions/create_raw_simulation_list.R')
source('r/functions/recursive_minimum_outbreak.R')

# Define simulation settings ----------------------------------------------

seed <- 7865
set.seed(seed)
all_simulations <- list()

# Create R distributions --------------------------------------------------
# Will need to run for each of these R distributions for scenarios
# r_dist_list_2_11 <- lapply(1:20, function(x) { rgamma(1000000, rate = 0.95, shape = 2) } )
# r_dist_list_1_67 <- lapply(1:20, function(x) { rgamma(1000000, rate = 1.2, shape = 2) } )
r_dist_list_1_36 <- lapply(1:20, function(x) { rgamma(1000000, rate = 1.475, shape = 2) } )

# SET R DIST LIST --------------------------------------------------------
r_dist_list <- r_dist_list_1_36
r_dist_mean_label <- formatC(mean(r_dist_list[[1]]), digits = 2, format = "f")
print(r_dist_mean_label)

# Define changing values --------------------------------------------------

population_sizes <- c(200, 500, 1000, 2000, 5000, 10000, 15000, 20000)

outbreak_ranges <- c("10-200", "10-500", "10-1000", "10-2000", "10-5000",
                      "100-10000", "100-15000", "100-20000")



all_simulations_list <- list()
## all_simulations_list <- vector("list", nrow(permutations))


# Run simulations ---------------------------------------------------------
paste("start sims:", Sys.time())
foreach(p = 1:length(population_sizes)) %dopar% {
  population_size <- population_sizes[p]
  outbreak_range <- outbreak_ranges[p]
  pop_size_sims <- list()
  print(p)
  print(population_size)
  print(outbreak_range)
    for (i in 1:1000) {
      raw_simulation <- create_raw_simulation_list(
        population_size = population_size,
        r_dist_list = r_dist_list,
        outbreak_range = outbreak_range)
      list_name <- paste(population_size, i, sep = "-")
      pop_size_sims[[list_name]] <- raw_simulation
    }
  print("first run done")

  all_simulations_list[[population_size]] <- pop_size_sims
  timestring <- format(Sys.time()+(60*60*24*2), "%d_%m_%s")
  
  file_path <- paste0(
    "data/processing/scenarios/",
    "r_mean_", gsub("\\.", "_", r_dist_mean_label), "/")
  dir.create(file_path, showWarnings = F)  
  
  all_simulations_list <- pop_size_sims
  file_name <- paste0(
    file_path,
    "raw_sim_",
    population_size, "_",
    outbreak_range, "_",
    timestring, ".qs")

  attr(all_simulations_list, "seed") <- seed
  qs::qsave(all_simulations_list, file = file_name)
  print(file_name)
}

paste("End :", Sys.time())
