## Ensure a minimum number for each outbreak simulation if this
## number isn't hit then keep running then simulate new data


recursive_minimum_outbreak <- function(min_outbreak_size, max_outbreak_size, 
                                       duration, population_size, dist_r,
                                       dist_incubation, dist_infectious_period){
  sim <- simulacr::simulate_outbreak(
    duration = duration, #in days
    population_size = population_size,
    R_values = dist_r,
    dist_incubation = dist_incubation,
    dist_infectious_period = dist_infectious_period
  )
  if (nrow(sim) < min_outbreak_size | nrow(sim) > max_outbreak_size) {
    sim <- recursive_minimum_outbreak(min_outbreak_size, max_outbreak_size, duration, population_size,
                                      dist_r, dist_incubation, dist_infectious_period)
  }
  return(sim)
}
