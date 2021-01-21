## Creates outbreak simulations with unknown contacts 
    ## - fixed distributions for R, infectious period, and incubation period
    ## - params for population size, proportion, and a list of R distributions.

create_raw_simulation_list <- function(
  population_size, r_dist_list, outbreak_range) {
  
  
  ## Create Infectious Period Distribution (FIXED)
  inf_mu <- 5 # mean in days
  inf_sd <- 4.7 # standard deviation in days
  infectious_period <- make_disc_gamma(inf_mu, inf_sd)
  ## Create Incubation Period Distribution (FIXED)
  inc_mu <- 9.7 # mean in days
  inc_sd <- 5.5 # standard deviation in days
  incubation <- make_disc_gamma(inc_mu, inc_sd)
  ## Create Reproduction Number Distribution (PARAMETER)
  r_dist <- sample(r_dist_list, 1)[[1]]
  if (r_dist[1] < 1) { r_dist <- sample(r_dist_list, 1)[[1]] }
  
  outbreak_range <- stringr::str_split(outbreak_range, pattern = "-")[[1]]
  # Run simulation
  sim <- recursive_minimum_outbreak(
    min_outbreak_size = outbreak_range[1],
    max_outbreak_size = outbreak_range[2],
    duration = 365, #in days
    population_size = population_size,
    dist_r = r_dist,
    dist_incubation = incubation,
    dist_infectious_period = infectious_period
  )

  meta_data <- data.frame(
    population_size = c(population_size),
    ## r dist data
    r_dist_method = c("gamma"),
    r_dist_rate = c(1.2),
    r_dist_shape = c(2),
    r_dist_min = c(as.numeric(quantile(r_dist, 0))),
    r_dist_25 = c(as.numeric(quantile(r_dist, 0.25))),
    r_dist_median = c(median(r_dist)),
    r_dist_mean = c(mean(r_dist)),
    r_dist_75 = c(as.numeric(quantile(r_dist, 0.75))),
    r_dist_max = c(as.numeric(quantile(r_dist, 1))),
    ## infectious data
    inf_mu = inf_mu, 
    inf_sd = inf_sd,
    inf_shape = infectious_period$parameters$shape,
    inf_scale = infectious_period$parameters$scale,
    ## incubation data
    inc_mu = inc_mu,
    inc_sd = inc_sd,
    inc_shape = incubation$parameters$shape,
    inc_scale = incubation$parameters$scale
  )
  sim_data = list(sim = sim, meta_data = meta_data)
  return(sim_data)
}
