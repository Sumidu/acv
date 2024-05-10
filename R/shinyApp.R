library(shiny)
library(DT)
library(DBI)
library(RPostgres)
library(pool)


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
  titlePanel("Database ID Matcher"),
  sidebarLayout(
    sidebarPanel(
      selectInput("table1", "Select Table 1", choices = NULL),
      uiOutput("columnSelect1"),
      selectInput("table2", "Select Table 2", choices = NULL),
      uiOutput("columnSelect2"),
      actionButton("add", "Add Match"),
      actionButton("save", "Save to Database")
    ),
    mainPanel(
      DTOutput("table1Display"),
      DTOutput("table2Display"),
      DTOutput("conversionTable")
    )
  )
)

server <- function(input, output, session) {
  observe({
    db_tables <- dbListTables(pool)
    updateSelectInput(session, "table1", choices = db_tables)
    updateSelectInput(session, "table2", choices = db_tables)
  })
  
  data1 <- reactive({
    req(input$table1)
    dbReadTable(pool, input$table1)
  })
  
  data2 <- reactive({
    req(input$table2)
    dbReadTable(pool, input$table2)
  })
  
  output$columnSelect1 <- renderUI({
    req(data1())
    selectInput("selectedColumn1", "Select ID column for File 1",
                choices = names(data1()), selected = names(data1())[1])
  })
  
  output$columnSelect2 <- renderUI({
    req(data2())
    selectInput("selectedColumn2", "Select ID column for File 2",
                choices = names(data2()), selected = names(data2())[1])
  })
  
  output$table1Display <- renderDT({
    datatable(data1(), selection = 'single')
  })
  
  output$table2Display <- renderDT({
    datatable(data2(), selection = 'single')
  })
  
  matches <- reactiveVal(data.frame(ID1 = character(), ID2 = character()))
  output$conversionTable <- renderDT({
    datatable(matches())
  })
  
  observeEvent(input$add, {
    sel1 <- input$table1Display_rows_selected
    sel2 <- input$table2Display_rows_selected
    if (length(sel1) == 1 && length(sel2) == 1) {
      new_row <- data.frame(
        ID1 = data1()[sel1, input$selectedColumn1],
        ID2 = data2()[sel2, input$selectedColumn2]
      )
      matches(rbind(matches(), new_row))
    }
  })
  
  observeEvent(input$save, {
    req(nrow(matches()) > 0)
    # Function to extract consonants from a string
    extract_consonants <- function(name) {
      gsub("[aeiouAEIOU]", "", name)  # Remove vowels
    }
    # Create a table name by extracting consonants and combining them
    tbl_name <- paste0("matches_", extract_consonants(input$table1), "_", extract_consonants(input$table2))
    # Attempt to write to the database
    tryCatch({
      dbWriteTable(pool, tbl_name, matches(), append = TRUE, row.names = FALSE)
      # Success notification
      showNotification(paste("Data successfully saved to table:", tbl_name), type = "message")
    }, error = function(e) {
      # Error notification
      showNotification(paste("Failed to save data:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)