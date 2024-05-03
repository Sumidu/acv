library(tidyverse)
library(glue)
library(rvest)
# This script will scrape the CORE database for a given conference name.

conf_name <- "ACM RecSys"
base_url <- "https://portal.core.edu.au/conf-ranks/"
dbase <- "CORE2023"
page <- 1
  
full_url <- glue("{base_url}?search={URLencode(conf_name)}&by=all&source={dbase}&sort=atitle&page={page}")

rvest::read_html(full_url) %>% 
  html_nodes("table") %>% 
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()


pubfile <- here::here("data","final_publications.rds")
publications <- read_rds(pubfile) %>% filter(type == "Conference") |> 
  mutate(searchtitle = str_to_lower(paste(journal, number))) |> 
  select(searchtitle, title) |> View()
  mutate(searchtitle = str_remove_all(searchtitle, "…")) |>
  mutate(searchtitle = str_replace_all(searchtitle, " [0-9]*-[0-9]*$", "")) |> 
  mutate(searchtitle = str_replace_all(searchtitle, "^proceedings of the [0-9]*", "")) |> 
  mutate(searchtitle = str_replace_all(searchtitle, "conference", "")) |> 
  mutate(searchtitle = str_squish(searchtitle)) |>
  count(searchtitle) |> arrange(desc(n))
publications

for(pub in publications$searchtitle){
  print(pub)
  conf_name <- pub
  full_url <- glue("{base_url}?search={URLencode(conf_name)}&by=all&source={dbase}&sort=atitle&page={page}")
  
  tbls <- rvest::read_html(full_url) %>% 
    html_nodes("table") %>% 
    html_table()
  
  if(length(tbls) > 0){
    tbls[[1]] %>% 
    as_tibble() |> print()
  } else
    print("No results")
  
}

# character for end of string match regex
