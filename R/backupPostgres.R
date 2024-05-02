
# This script loads data from a SQLite database and migrates it to a Postgres database.
# any database changes are lost during transfer
if(FALSE){
  
library(tidyverse)
library(DBI)
library(RSQLite)
library(RPostgres)
library(here)

host_name <- "aws-0-eu-central-1.pooler.supabase.com"
db_user <- Sys.getenv("SUPABASEUSER")
db_password <- Sys.getenv("SUPABASEPW")

db <- dbConnect(RPostgres::Postgres(), 
                dbname = "postgres", host = host_name, port = 5432, 
                user = db_user, 
                password = db_password,
                gssencmode="disable"
)

target <- dbConnect(RSQLite::SQLite(), here("data", "backups", glue::glue("backup-{today()}.db")))
#list all tables
tables <- dbListTables(db) %>% 
  tibble(tbl_name = .) %>% 
  filter(!str_detect(tbl_name, "pg_stat")) 

#dbBegin(target)
for(tab in tables$tbl_name) {
  cat("Copying ", tab, "\n")
  tbl <- tbl(db, tab) |> collect()
  cat("Rows: ", nrow(tbl), "\n")
  #copy_to(target, tbl, tab, overwrite = TRUE)
  
  dbWriteTable(target, tab, tbl, overwrite = TRUE)  
  #dbExecute(target, glue::glue("ALTER TABLE {tab} ADD PRIMARY KEY (id);"))
  
}

dbDisconnect(db)
dbListTables(target)
dbDisconnect(target)

}
