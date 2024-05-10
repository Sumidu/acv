library(shiny)
library(DT)
library(DBI)
library(pool)
library(RPostgres)
options(shiny.maxRequestSize = 100*1024^2)  # Set max file size to 100 MB

db_user <- Sys.getenv("SUPABASEUSER")
db_password <- Sys.getenv("SUPABASEPW")

# Establish database connection
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "postgres",
  host = "aws-0-eu-central-1.pooler.supabase.com",
  port = 5432,
  user = db_user,
  password = db_password,
  gssencmode="disable",
  sslmode = 'require'  # Adjust as necessary
)

ui <- fluidPage(
  titlePanel("CSV to PostgreSQL Database"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      uiOutput("columnSelector"),
      textInput("tableName", "Enter new table name"),
      selectInput("primaryKey", "Select primary key", choices = NULL),
      actionButton("upload", "Upload to Database")
    ),
    mainPanel(
      DTOutput("dataPreview")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal()  # Holds the original data
  
  # Reactive to hold renamed data
  renamedData <- reactive({
    df <- data()
    if (is.null(df)) {
      return(data.frame())
    }
    # Apply renaming if any
    new_names <- sapply(names(df), function(colname) {
      input_name <- paste0("newname_", colname)
      if (!is.null(input[[input_name]])) {
        return(str_to_lower(input[[input_name]]))
      } else {
        return(colname)
      }
    })
    names(df) <- new_names
    df
  })
  
  observe({
    file <- input$file
    if (is.null(file)) {
      return()
    }
    df <- read.csv(file$datapath)
    data(df)  # Store original data
    
    # Setup for column renaming UI
    output$columnSelector <- renderUI({
      fluidRow(
        lapply(names(df), function(colname) {
          textInput(paste0("newname_", colname), label = sprintf("Rename %s to:", colname), value = colname)
        })
      )
    })
    
    # Setup primary key selector with initial column names
    updateSelectInput(session, "primaryKey", choices = names(df), selected = names(df)[1])
  })
  
  # Observe changes in renaming inputs to update primary key choices
  observe({
    if (!is.null(renamedData())) {
      updateSelectInput(session, "primaryKey", choices = names(renamedData()), selected = input$primaryKey)
    }
  })
  
  output$dataPreview <- renderDT({
    datatable(renamedData(), options = list(scrollX = TRUE), editable = TRUE)
  })
  
  observeEvent(input$upload, {
    req(input$tableName)
    df <- renamedData()  # Use renamed data
    
    # Validate primary key existence
    pk <- input$primaryKey
    if (!(pk %in% names(df))) {
      showNotification("Primary key is not valid.", type = "error")
      return()
    }
    
    # Wrap database operations in a transaction using pool
    poolWithTransaction(pool, {
      sql <- sprintf("CREATE TABLE IF NOT EXISTS %s (%s, PRIMARY KEY (%s))",
                     input$tableName,
                     paste(names(df), "TEXT", collapse = ", "),
                     pk)
      dbExecute(pool, sql)
      dbWriteTable(pool, input$tableName, df, append = TRUE, row.names = FALSE)
    })
    
    showNotification(paste("Table", input$tableName, "successfully created and data uploaded."), type = "message")
  })
  
}


shinyApp(ui, server)