## This creates an epicontact object of reported cases by removing the given proportion
## of cases and removes those case ids as sources.
##   - Uses tryCatch to fix duplication errors and return appropriate object
## 

simulate_reported_cases_unique <- function(outbreak_obj, prop_under_reporting) {
  ## Calculate number of missing cases
  outbreak_total <- nrow(outbreak_obj)
  if (outbreak_total > 0) {
    n_missing_cases <- round(outbreak_total) * prop_under_reporting
    ## Remove nodes from outbreak simulation to represent missing contacts, 
      ##  remove source ids that match missing contact ids, return outbreak object
    reported_cases <- remove_n_cases_tidyverse(outbreak_obj, n_missing_cases)
    
    
    # Convert to epicontacts object
    if (length(reported_cases) > 0) {
      reported_outbreak_obj <- tryCatch({
        as.epicontacts(reported_cases)
      }, error = function(e){
        ## rename duplicates if necessary
        dups <- reported_cases[duplicated(reported_cases$id), "id"]
        reported_cases <- mutate(
          reported_cases, id = if_else(id %in% dups, paste0(id, as.character(R)), id))    
        class(reported_cases) <- c("outbreak", class(reported_cases))
        attr(reported_cases, "has_contacts") <- FALSE
        as.epicontacts(reported_cases)
      })
      
      return(reported_outbreak_obj)
    }
  }
}
