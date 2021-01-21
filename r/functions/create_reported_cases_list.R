## Creates outbreak simulations with unknown contacts 
## - fixed distributions for R, infectious period, and incubation period
##    - inc period and inf period from WHO Ebola 9 month paper
## - params for population size, proportion, and a list of R distributions.

## Creates outbreak simulations with unknown contacts 
## - fixed distributions for R, infectious period, and incubation period
## - params for population size, proportion, and a list of R distributions.

create_reported_cases_list <- function(outbreak_list_object, proportion_under_reporting,
                                       outbreak_days = 365) {
  
  meta_data <- outbreak_list_object$meta_data
  sim <- outbreak_list_object$sim
  sim <- sim[sim$date_onset <= outbreak_days,] 
  
  # Extract contact data
  
  epicontacts_list <- simulate_reported_cases_unique(sim, proportion_under_reporting)
  n_total_outbreak <- nrow(sim)
  reported <- epicontacts_list$contacts
  
  if (length(reported) > 0) {
    n_known_epilink <- sum(!is.na(reported$from))
    n_unknown_epilink <- sum(is.na(reported$from))
    n_reported <- nrow(reported)
    reporting_probability <- n_known_epilink / n_reported 
    se_reporting_probability = 
      sqrt(reporting_probability * (1 - reporting_probability) / n_reported)
    conf <- binom.test(n_known_epilink,  (n_reported), conf.level = 0.95)$conf.int
    
    # Return df 
    df <- data.frame(
      proportion_under_reporting = proportion_under_reporting,
      true_value = 1 - proportion_under_reporting,
      n_reported = n_reported,
      n_known_epilink = n_known_epilink,
      n_unknown_epilink = n_unknown_epilink,
      n_total_outbreak = n_total_outbreak,
      estimated_reporting_probability = reporting_probability,
      se_reporting_probability = se_reporting_probability,
      reporting_conf_low = conf[1],
      reporting_conf_high = conf[2],
      proportion_known_epilink = n_known_epilink / n_reported,
      population_size = meta_data$population_size,
      outbreak_days = outbreak_days,
      r_dist_rate = meta_data$r_dist_rate,
      r_dist_shape = meta_data$r_dist_shape,
      r_dist_method = meta_data$r_dist_method,
      r_dist_min = meta_data$r_dist_min,
      r_dist_25 = meta_data$r_dist_25,
      r_dist_median = meta_data$r_dist_median,
      r_dist_mean = meta_data$r_dist_mean,
      r_dist_75 = meta_data$r_dist_75,
      r_dist_max = meta_data$r_dist_max, 
      inf_mu = outbreak_list_object$meta_data$inf_mu,
      inf_sd = outbreak_list_object$meta_data$inf_sd,
      inc_mu = outbreak_list_object$meta_data$inc_mu,
      inc_sd = outbreak_list_object$meta_data$inf_sd
    )
    
    return(df)
  } else {
    data.frame()
  }
}
