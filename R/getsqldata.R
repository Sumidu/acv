# this is a file that will move all data to a sqlite database.

library(here)
library(DBI)
library(RSQLite)
library(tidyverse)
# Create a connection to the database
db <- dbConnect(RSQLite::SQLite(), here("data", "my_data.db"))

grants <- read_csv(here("data", "download", "awardsandgrants.csv")) |> 
  filter(str_starts(award, "Drittmittel")) |> 
  mutate(name = str_remove(award, "Drittmittel: ")) |> 
  mutate(total_amount = str_match(award, "([0-9]+.*) Euro")[,1]) |> 
  mutate(own_amount = str_match(award, "([0-9]+.*) Euro")[,1]) |> 
  mutate(id = row_number()) |> 
  select(id, name, year = date, total_amount, own_amount, funder = with)


# Function to copy a dataframe to SQLite and add a primary key
copy_to_sqlite_with_pk <- function(con, df, table_name, pk_column) {
  # Copy the data frame to SQLite
  copy_to(con, df, table_name, temporary = FALSE, overwrite = TRUE)
  
  # Build SQL to create a new table with the primary key
  col_definitions <- sapply(names(df), function(col_name) {
    col_type <- class(df[[col_name]])
    type_sql <- switch(col_type,
                       factor = "TEXT",
                       character = "TEXT",
                       integer = "INTEGER",
                       numeric = "REAL",
                       logical = "BOOLEAN",
                       stop("Unsupported column type: ", col_type))
    
    if (col_name == pk_column) {
      return(sprintf("%s %s PRIMARY KEY", col_name, type_sql))
    } else {
      return(sprintf("%s %s", col_name, type_sql))
    }
  })
  
  create_table_sql <- sprintf("CREATE TABLE %s_new (%s);", table_name, paste(col_definitions, collapse = ", "))
  cat(create_table_sql, "\n")
  dbExecute(con, create_table_sql)
  
  # Insert data from the old table to the new table
  cols <- paste(names(df), collapse = ", ")
  insert_sql <- sprintf("INSERT INTO %s_new (%s) SELECT %s FROM %s;", table_name, cols, cols, table_name)
  dbExecute(con, insert_sql)
  
  # Drop the old table and rename the new table
  dbExecute(con, sprintf("DROP TABLE %s;", table_name))
  dbExecute(con, sprintf("ALTER TABLE %s_new RENAME TO %s;", table_name, table_name))
}

copy_to_sqlite_with_pk(db, grants, "grants", "id")




dbDisconnect(db)
