## Name: 03_combine_sims.R
## Description: Combine simulations results
## Input file: sim_results
## Functions: 
## Output file: sim_results_combined.qs

# Packages ----------------------------------------------------------------
library(dplyr)


# Source user written scripts ---------------------------------------------

if(!exists("scenario_ref")) {
  scenario_ref <- "r_mean_2_11"
  
  # Create_paths ---------------------------------------------
  scenario_path <- file.path("scenarios", scenario_ref)
  data_path <- file.path("data", "processing", scenario_path)
  dir.create(data_path, showWarnings = F)
  outputs_path <- file.path("outputs", scenario_path)
  dir.create(outputs_path, showWarnings = F)
}

results_files <- list.files(path = data_path, pattern = "^sim_results", 
                            full.names = TRUE)
sim_results_places <- grep("sim_results_combined", 
                           results_files, 
                           invert = TRUE)
results_files <- results_files[sim_results_places]



# Combine into one dataframe
combined_results <- do.call(rbind,
                            lapply(results_files, 
                                   qs::qread))

# Create bin sizes for the outbreaks
outbreak_bins <- c(0, 10, 100, 500, 1000, Inf)
outbreak_bin_names <-
  c("1-9", "10-99", "100-499", "500-999", "1000+")


combined_results$reported_outbreak_bins <- cut(
  combined_results$n_reported, 
  breaks = outbreak_bins,
  labels = outbreak_bin_names)
table(combined_results$reported_outbreak_bins, combined_results$true_value)


# Coverage
combined_results <- combined_results %>% 
  mutate(covered =
           true_value >= reporting_conf_low & true_value <= reporting_conf_high,
         over_est = true_value > reporting_conf_high,
         under_est = true_value < reporting_conf_low
  )

# Bias and Bias squared (For Monte carlo standard errors)
combined_results <- combined_results %>% mutate(
  bias = estimated_reporting_probability - true_value,
  bias_sq = bias^2)



# Create a grouping varible for the categorises
combined_results$catid <- paste0(combined_results$true_value,
                                 "_",
                                 combined_results$reported_outbreak_bins)

# Check the variable
table(combined_results$catid) %>% min

# Sample 2000 from each category.
set.seed(2620)
df_results <- combined_results %>% 
  filter(n_known_epilink > 0,
         n_reported > 10) %>% 
  group_by(catid) %>% 
  sample_n(4000, replace = T) %>% 
  mutate(reported_outbreak_bins = factor(reported_outbreak_bins))


# Add some extra stuff that can go in the combine results
sum_df <- df_results %>% 
  group_by(true_value, reported_outbreak_bins) %>% 
  summarise(average_point_estimate = mean(estimated_reporting_probability),
            sum_bias_sq = sum(bias_sq),
            mse = mean(bias_sq))

df_results <- left_join(df_results, sum_df)

df_results <- df_results %>% 
  mutate(spread = estimated_reporting_probability - average_point_estimate,
         spread_sq = spread^2,
         mse_num = (bias_sq - mse)^2,
         
  ) %>% 
  ungroup()


filename <- file.path(data_path, "sim_results_combined.qs")

qs::qsave(df_results, filename)
print(paste("Saved to: ", filename))
