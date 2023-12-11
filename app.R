#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(DBI)
library(pool)
library(DT)
library(bslib)
library(tidyverse)
library(glue)

# Connect to the MySQL database (replace with your database details)
db <- config::get("azuredb")
pool <- dbPool(
    drv = RMariaDB::MariaDB(), 
    dbname = db$database,
    host = db$server,
    port = db$port,
    username = db$uid,
    password = db$pwd,
    ssl.ca = db$ssl_ca,
    sslmode = "require"
)
tbl <- db$table
onStop(function() {
    poolClose(pool)
})
data <- read_csv("ade.csv",col_names = F)
# Define UI
ui <- page_sidebar(
  title = "Phenotype of ADR",
  sidebar = sidebar(
    #textInput("query", label = "Enter An ADR:", placeholder = "e.g., Rhabdomyolysis"),
    selectizeInput('query', choices = NULL, label = "Enter An ADR:"),
    actionButton("submit", "Search")
  ),
  card(
    card_header(
      class = "bg-dark",
      "Definition"
    ),
    DTOutput("datatable")
  ),
  theme = bs_theme(bootswatch = "yeti")
)


# Define server logic
server <- function(input, output, session) {
  updateSelectizeInput(session, 'query', choices = data$X1, server = TRUE)
    observeEvent(input$submit, {
        # Execute the SQL query
        #query <- "select * from phenosen where term = 'abdominal injury'"
        query <- glue_sql("
                        SELECT *
                        FROM {`tbl`}
                        WHERE {`tbl`}.term = {input$query}
  ", .con = pool)
        query_result <- dbGetQuery(pool, query)
        query_result$original_sentence<- query_result$original_sentence %>%map( ~.x[!.x == 00]) %>%
          map_chr(rawToChar)
        df_reactive <- reactive({
            query_result %>% select(PMID = pmid, ADR =term, term_id,start=term_start_position,
                                          end =term_end_position, Definition = original_sentence) %>%
                mutate(PMID=paste0(
                    "<a href='https://pubmed.ncbi.nlm.nih.gov/",
                    PMID,
                    "/' target='_blank'>",
                    PMID,
                    "</a>"
                ))%>%
                mutate(Definition = map_chr(Definition,~gsub(str_sub(Definition,start=start,end=end),
         paste0("<span style='background-color: #FFFF00'>",str_sub(Definition,start=start,end=end),"</span>"),.x,useBytes = T)))
        })
        # Display the table with highlighted words
        output$datatable <- renderDT({
            datatable(df_reactive() %>% as_tibble() %>% select(PMID, ADR, Definition), 
                      escape = F, 
                      options = list(searching = FALSE, paging = TRUE,
                                     lengthMenu = c(5, 10, 20, 50)))
        })
    })
}

# Run the Shiny app
shinyApp(ui, server)
