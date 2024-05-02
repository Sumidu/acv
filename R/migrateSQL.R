
library(tidyverse)
library(DBI)
library(RSQLite)
library(RPostgres)

host_name <- "aws-0-eu-central-1.pooler.supabase.com"
db_user <- Sys.getenv("SUPABASEUSER")
db_password <- Sys.getenv("SUPABASEPW")

target <- dbConnect(RPostgres::Postgres(), 
                dbname = "postgres", host = host_name, port = 5432, 
                user = db_user, 
                password = db_password,
                gssencmode="disable"
)

db <- dbConnect(RSQLite::SQLite(), here("data", "my_data.db"))
#list all tables
tables <- dbListTables(db) %>% 
  tibble(tbl_name = .) %>% 
  filter(!str_detect(tbl_name, "sqlite")) 

dbBegin(target)
for(tab in tables$tbl_name) {
  cat("Copying ", tab, "\n")
  tbl <- tbl(db, tab) |> collect()
  cat("Rows: ", nrow(tbl), "\n")
  #copy_to(target, tbl, tab, overwrite = TRUE)
  
  if(tab == "teaching_evaluations"){
    tbl <- tbl %>% mutate(id = row_number()) %>% 
      select(id, everything())
  }
  dbWriteTable(target, tab, tbl, overwrite = TRUE)  
  dbExecute(target, glue::glue("ALTER TABLE {tab} ADD PRIMARY KEY (id);"))
  
}
#dbExecute(target, glue::glue("ALTER TABLE teaching_evaluations ADD PRIMARY KEY (teaching_id);"))
dbCommit(target)
dbDisconnect(db)
dbListTables(target)
dbDisconnect(target)

