






remove_n_cases_tidyverse <- function(outbreak_obj, n_remove) {
  if(n_remove > 0) {
    missing_ids <- sample(outbreak_obj$id, size = n_remove, replace = FALSE)
    
    known_outbreak_obj <- outbreak_obj %>%
      filter(!(id %in% missing_ids)) %>%
      mutate(source = if_else(source %in% missing_ids, NA_character_, source))
    
    class(known_outbreak_obj) <- c("outbreak", class(known_outbreak_obj))
    attr(known_outbreak_obj, "has_contacts") <- FALSE
    return(known_outbreak_obj)
  } else {
    return(outbreak_obj)
  }
}