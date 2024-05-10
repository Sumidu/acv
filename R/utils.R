get_scholar_profile <- function(update_pubs = FALSE)
{
  scholar_cache <- here::here("data", "scholarcache.RDS")
  
  if (!file.exists(scholar_cache) | update_pubs) {
    
    #message("Updating Scholar Cache")
    # Update the cache if asked
    scholar_profile <- scholar::get_profile("K6EVDoYAAAAJ")
    readr::write_rds(scholar_profile, scholar_cache)
    
  } else {
    # return cache
    scholar_profile <- readr::read_rds(scholar_cache)
  }
  scholar_profile
}

get_scholar_publications <- function(update_pubs = FALSE)
{
  pub_cache <- here::here("data", "pubcache.RDS")
  
  if (!file.exists(pub_cache) | update_pubs) {
    
    # Update the cache if asked
    publications <- scholar::get_publications("K6EVDoYAAAAJ")
    readr::write_rds(publications, pub_cache)
    
  } else {
    # return cache
    publications <- readr::read_rds(pub_cache)
  }
  publications
}