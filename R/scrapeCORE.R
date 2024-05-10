library(tidyverse)
library(glue)
# This script will scrape the CORE database for a given conference name.

core <- read_csv(here::here("data","core.csv"), col_names = c("id", "Title", "Acronym", "Source",  "Rank", "Note", "Primary-for", "Comments", "Average-Rating")) |> 
  arrange(id) |> 
  select(-Note, -`Primary-for`, -Comments, -`Average-Rating`)



pubfile <- here::here("data","final_publications.rds")
publications <- read_rds(pubfile) %>% filter(type == "Conference") |> 
  mutate(searchtitle = str_to_lower(paste(journal, number))) |> 
  select(searchtitle, title) |> 
  mutate(searchtitle = str_remove_all(searchtitle, "…")) |>
  mutate(searchtitle = str_replace_all(searchtitle, " [0-9]*-[0-9]*$", "")) |> 
  mutate(searchtitle = str_replace_all(searchtitle, "^proceedings of the [0-9]*", "")) |> 
  mutate(searchtitle = str_replace_all(searchtitle, "conference", "")) |> 
  mutate(searchtitle = str_squish(searchtitle)) |>
  count(searchtitle) |> arrange(desc(n))
publications



matches <- tibble()
for(pub in publications$searchtitle){
  print(pub)
  conf_name <- pub
  
  #conf_name <- publications$searchtitle[2]
  
  closest_match <- core$Title[
    stringdist::amatch(conf_name, core$Title, maxDist = Inf)
  ]
  
  matches <- bind_rows(matches, tibble(my_name = conf_name, core_name = closest_match))

}


matches
# character for end of string match regex
