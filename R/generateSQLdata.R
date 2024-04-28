# this is a file that will move all data to a sqlite database.

library(here)
library(glue)
library(DBI)
library(RSQLite)
library(tidyverse)

#' Converts half-filled CSV entries to usable tables for the cv package
#'
#' @param a CSV dataframe
#' @param lang the language folder to use (defaults to "en")
#'
#' @return a filled and correctly formatted dataframe for printing
#' @export
#'
#' @examples
#' reviews %>% fix_fill_and_dates(lang = "en")
fix_fill_and_dates <- function(x, lang = "en") {
  x %>% 
    fill(Start, End, What, With, Where, Tag) %>%  
    mutate(Start = mdy(Start)) %>% 
    mutate(End = mdy(End, quiet = TRUE)) %>% 
    mutate(When = case_when(
      is.na(End) ~ glue("{year(Start)}--{today_str(lang)}") %>% as.character(),
      year(Start) == year(End) ~ year(End) %>% as.character(),
      TRUE ~ glue("{year(Start)}--{year(End)}") %>% as.character()
    ))
}

#' Creates a today string depending on the language given
#'
#' @param lang the language to choose
#'
#' @return the term for today
#' @export
#'
#' @examples today_str(lang = "de") # returns Heute
today_str <- function(lang = "en") {
  if (lang == "de") {
    return("heute")
  }
  return("present")
}


get_db_source <- function(){
  db <- dbConnect(RSQLite::SQLite(), here("data", "my_data.db"))
  db
}


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


convert_grants <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  grants <- read_csv(here("data", "de", "awardsandgrants.csv")) |> 
    filter(str_starts(award, "Drittmittel")) |> 
    mutate(name = str_remove(award, "Drittmittel: ")) |> 
    mutate(total_amount = str_match(award, "([0-9]+.*) Euro")[,1]) |> 
    mutate(own_amount = str_match(award, "([0-9]+.*) Euro")[,1]) |> 
    arrange(date) |> 
    mutate(id = row_number()) |> 
    select(id, name, year = date, total_amount, own_amount, funder = with)
  
  cat("Grants need manual adjustment.")
  copy_to_sqlite_with_pk(db, grants, "grants", "id")
  dbDisconnect(db)
}

convert_awards <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  awards <- read_csv(here("data", "de", "awardsandgrants.csv")) |> 
    filter(!str_starts(award, "Drittmittel")) |> 
    arrange(date) |> 
    mutate(id = row_number()) |> 
    select(id, name = award, year = date, award_source = with)
  
  copy_to_sqlite_with_pk(db, awards, "awards", "id")
  dbDisconnect(db)
}

convert_jobs <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  jobs <- read_csv(here("data", "de", "jobs.csv")) |> 
    filter(Tag == "research") |> 
    mutate(active = End == "present") |> 
    mutate(Start = mdy(Start)) |>
    rowwise() |>
    mutate(End = as_date(ifelse(End == "present", today(), mdy(End)))) |>
    ungroup() |>
    arrange(Start) |> 
    mutate(Start = as.character(Start)) |>
    mutate(End = as.character(End)) |> 
    mutate(id = row_number()) |> 
    select(id, start = Start, end = End, name = What, with = With, location = Where, description = Why, active)
  
  copy_to_sqlite_with_pk(db, jobs, "jobs", "id")
  dbDisconnect(db)
}

convert_education <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  education <- read_csv(here("data", "de", "jobs.csv")) |> 
    filter(Tag == "education") |> 
    mutate(active = End == "present") |> 
    mutate(Start = mdy(Start)) |>
    rowwise() |>
    mutate(End = as_date(ifelse(End == "present", today(), mdy(End)))) |>
    ungroup() |>
    arrange(Start) |> 
    mutate(Start = as.character(Start)) |>
    mutate(End = as.character(End)) |> 
    mutate(id = row_number()) |> 
    select(id, start = Start, end = End, name = What, with = With, location = Where, description = Why)
  
  copy_to_sqlite_with_pk(db, education, "education", "id")
  dbDisconnect(db)
}

convert_degrees <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  degrees <- read_csv(here("data", "de", "jobs.csv")) |> 
    filter(Tag == "degree") |> 
    mutate(active = End == "present") |> 
    mutate(Start = mdy(Start)) |>
    rowwise() |>
    mutate(End = as_date(ifelse(End == "present", today(), mdy(End)))) |>
    ungroup() |>
    arrange(Start) |> 
    mutate(Start = as.character(Start)) |>
    mutate(End = as.character(End)) |> 
    mutate(id = row_number()) |> 
    select(id, start = Start, end = End, name = What, with = With, location = Where, description = Why)
  
  copy_to_sqlite_with_pk(db, degrees, "degrees", "id")
  dbDisconnect(db)
}


convert_memberships <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  memberships <- read_csv(here("data", "de", "memberships.csv")) |> 
    mutate(active = TRUE) |> 
    mutate(Start = mdy("1/1/2013")) |>
    mutate(Start = as.character(Start)) |>
    mutate(End = "2050-01-01") |> 
    mutate(type = "Mitglied") |> 
    mutate(id = row_number()) |> 
    select(id, start = Start, end = End, name = Membership, type, active)
  
  copy_to_sqlite_with_pk(db, memberships, "memberships", "id")
  dbDisconnect(db)
}

convert_talks <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  talks <- read_csv(here("data", "de", "presentations.csv")) |> 
    filter(Tag == "talk") |>
    mutate(talk_date = mdy(When)) |>
    arrange(talk_date) |>
    mutate(talk_date = as.character(talk_date)) |>
    mutate(id = row_number()) |> 
    select(id, talk_date, title = Title, location = Location, conference = Conference)
  
  copy_to_sqlite_with_pk(db, talks, "talks", "id")
  dbDisconnect(db)
}

convert_lectures <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  lectures <- read_csv(here("data", "de", "presentations.csv")) |> 
    filter(Tag == "lecture") |>
    mutate(lecture_date = mdy(When)) |>
    arrange(lecture_date) |>
    mutate(lecture_date = as.character(lecture_date)) |>
    mutate(id = row_number()) |> 
    select(id, lecture_date, title = Title, location = Location, conference = Conference)
  
  copy_to_sqlite_with_pk(db, lectures, "lectures", "id")
  dbDisconnect(db)
}

convert_reviews <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  reviews <- read_csv(here("data", "de", "reviews.csv")) |> 
    tidyr::fill(Type) |> 
    filter(Type == "adhoc") |>
    mutate(times = 1) |> 
    mutate(last_reviewed = as.character(today())) |> 
    mutate(review_type = ifelse(str_detect(Name, "Konferenz"), "conference", "journal")) |> 
    mutate(id = row_number()) |> 
    select(id, name = Name, times, last_reviewed, review_type) |> 
    filter(!str_detect(name, "etc."))
  
  copy_to_sqlite_with_pk(db, reviews, "reviews", "id")
  dbDisconnect(db)
}


convert_conference_committees <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  committees <- read_csv(here("data", "de", "reviews.csv")) |> 
    tidyr::fill(Type) |> 
    filter(Type != "adhoc") |>
    mutate(times = 1) |> 
    mutate(last_reviewed = as.character(today())) |> 
    mutate(id = row_number()) |> 
    select(id, name = Name, times, last_reviewed, position_type = Type) |> 
    filter(!str_detect(name, "etc."))
  
  copy_to_sqlite_with_pk(db, committees, "committees", "id")
  dbDisconnect(db)
}

convert_service <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  services <- read_csv(here("data", "de", "service.csv")) |> 
    filter(Tag == "service") |>
    mutate(active = End == "present") |> 
    mutate(Start = mdy(Start)) |>
    rowwise() |>
    mutate(End = as_date(ifelse(End == "present", today(), mdy(End)))) |>
    ungroup() |>
    arrange(Start) |> 
    mutate(Start = as.character(Start)) |>
    mutate(End = as.character(End)) |> 
    mutate(id = row_number()) |> 
    select(id, name = What, start = Start, end = End, details = With, location = Where, role = Why, active) 
  
  copy_to_sqlite_with_pk(db, services, "services", "id")
  dbDisconnect(db)
}

convert_consulting <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  consulting <- read_csv(here("data", "de", "service.csv")) |> 
    filter(Tag != "service") |>
    mutate(active = End == "present") |> 
    mutate(Start = mdy(Start)) |>
    rowwise() |>
    mutate(End = as_date(ifelse(End == "present", today(), mdy(End)))) |>
    ungroup() |>
    arrange(Start) |> 
    mutate(Start = as.character(Start)) |>
    mutate(End = as.character(End)) |> 
    mutate(id = row_number()) |> 
    select(id, name = What, start = Start, end = End, client = With, location = Where, role = Why, active) 
  
  copy_to_sqlite_with_pk(db, consulting, "consulting", "id")
  dbDisconnect(db)
}


convert_software <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  software <- read_csv(here("data", "de", "software.csv")) |> 
    arrange(when) |> 
    mutate(start = as.character(when)) |> 
    mutate(id = row_number()) |> 
    select(id, role = what, start, details = why, url = where) 
  
  copy_to_sqlite_with_pk(db, software, "software", "id")
  dbDisconnect(db)
}


convert_teaching <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  teaching <- read_csv(here("data", "de", "teaching.csv")) |> 
    mutate(active = End == "present") |> 
    mutate(Start = mdy(Start)) |>
    rowwise() |>
    mutate(End = as_date(ifelse(End == "present", today(), mdy(End)))) |>
    ungroup() |>
    arrange(Start) |> 
    mutate(Start = as.character(Start)) |>
    mutate(End = as.character(End)) |> 
    mutate(id = row_number()) |> 
    mutate(credits = "4 ECTS") |> 
    mutate(sws = "3 SWS") |> 
    select(id, name = What, start = Start, end = End, frequency = With, students = Where, credits, sws, details = Why, active) 
  
  copy_to_sqlite_with_pk(db, teaching, "teaching", "id")
  dbDisconnect(db)
}

convert_theses <- function() {
  # Create a connection to the database
  db <- get_db_source()
  
  theses <- read_csv(here("data", "de", "theses.csv")) |> 
    arrange(year) |> 
    mutate(id = row_number()) |> 
    mutate(note = "") |> 
    select(id, title = name, year, degree, subject, author, note) 
  
  copy_to_sqlite_with_pk(db, theses, "theses", "id")
  dbDisconnect(db)
}

generate_teaching_evalution <- function() {
  
  db <- get_db_source()
  
  teaching <- tbl(db, "teaching") |> collect()
  
  # Uni Lübeck ----
  
  t_id <- teaching |> filter(str_detect(name, "Interaktive Systeme")) |> pull(id)
  evals <- tribble(
    ~teaching_id, ~year, ~semester_type, ~students, ~rating,
    t_id, 2021, "winter", 50, 1.91,
    t_id, 2022, "summer", 47, 2.63,
    t_id, 2022, "winter", 83, 3.04,
    t_id, 2023, "summer", 72, 2.55,
    t_id, 2023, "winter", 65, 1.88
    )
    
  t_id <- teaching |> filter(str_detect(name, "Forschungsthemen")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2022, "summer", 15, 3.0,
      )
    )
  
  t_id <- teaching |> filter(str_detect(name, "Mensch-Technik")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2022, "winter", 80, 2.09,
      t_id, 2023, "winter", 87, 2.9
      )
    )
  
  t_id <- teaching |> filter(str_detect(name, "Prozess")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2022, "winter", 31, 2.33,
      t_id, 2023, "winter", 19, 1.5
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Software Ergonomie")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2022, "summer", 50, 2.36
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Statistik und Methoden")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2023, "summer", 65, 2.53
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Usability und UX")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2022, "winter", 55, 2.61,
      t_id, 2023, "winter", 49, 2.57
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Responsible")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2023, "winter", 40, 2.14
    )
  )
  
  # RWTH Evaluations ----
  
  t_id <- teaching |> filter(str_detect(name, "Bachelor-Kolloquium")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2017, "summer", 15, 1.3
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Usability, User Diversity und Technologie-Akzeptanz")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2015, "winter", 10, 1.7,
      t_id, 2016, "winter", 26, 1.5,
      t_id, 2017, "winter", 19, 1.3,
      t_id, 2018, "winter", 16, 1.4,
      t_id, 2019, "winter", 15, 1.2
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Sozialwissenschaftliche Forschungsmethoden für Fort")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2015, "winter", 28, 2.4,
      t_id, 2017, "winter", 28, 1.6,
      t_id, 2018, "winter", 28, 1.5,
      t_id, 2020, "winter", 28, 1.6
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Sozialwissenschaftliche Forschungsmethoden in der A")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2016, "winter", 28, 2.0,
      t_id, 2017, "winter", 28, 1.3,
      t_id, 2018, "winter", 28, 1.7,
      t_id, 2020, "winter", 28, 1.9
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Anwendungsseminar")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2017, "summer", 22, 1.9,
      t_id, 2018, "summer", 22, 1.5,
      t_id, 2020, "summer", 22, 1.7,
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Visuelle Empfehlungssysteme")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2016, "summer", 18, 2.2
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Informationsvisualisierung und Sprache")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2017, "summer", 17, 1.7
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Informationsvisualisierung und Sprache")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2016, "summer", 15, 1.6,
      t_id, 2017, "summer", 17, 1.7
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Methoden der Technikkommunikation")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2016, "summer", 12, 1.4,
      t_id, 2017, "summer", 17, 1.7
    )
  )
  
  t_id <- teaching |> filter(str_detect(name, "Korpuslinguistik")) |> pull(id)
  evals <- evals |> bind_rows(
    tribble(
      ~teaching_id, ~year, ~semester_type, ~students, ~rating,
      t_id, 2021, "winter", 27, 1.6
      
    )
  )
  
  
  
  # Overview ----

  teaching_evaluations <- evals |> 
    mutate(id = row_number()) |> 
    select(id, teaching_id, year, semester_type, students, rating)
  
  teaching_evaluations |> 
    mutate(fullsem = paste0(year, " ", semester_type)) |>
    ggplot() +
    aes(y = rating, x = fullsem, group = c(fullsem), alpha = 0.6) +
    geom_boxplot() +
    geom_point()+
    scale_y_continuous(limits = c(1, 5)) +
    coord_flip() +
    labs(x = "Semester", y = "Rating", title = "Teaching Evaluations") +
    # hide legend
    theme(legend.position = "none")
    
  copy_to_sqlite_with_pk(db, evals, "teaching_evaluations", "id")
  dbDisconnect(db)
}





if(FALSE){
  convert_grants()
  convert_awards()
  convert_jobs()
  convert_education()
  convert_degrees()
  convert_talks()
  convert_lectures()
  convert_reviews()
  convert_conference_committees()
  convert_service()
  convert_consulting()
  convert_teaching()
  convert_theses()
  generate_teaching_evalution()
}
