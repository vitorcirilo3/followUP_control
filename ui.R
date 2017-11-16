library(ggplot2)

fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  
  titlePanel("Follow-UP ITV"),

  div(id="main_div",
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("status",
                       # Create a new Row in the UI for selectInputs
                       fluidRow(
                         column(4,
                                dateInput("date", "date",format = "yyyy-mm-dd"),
                                selectInput("flow_cell", "Flow Cell", c("teste1","teste2")),
                                selectInput("application", "Application", c("teste1","teste2")),
                                actionButton("run","Run")
                         ),
                         column(4,
                                selectInput("assay", "Assay", c("teste1","teste2")),
                                numericInput("leaght_reads", "Lenght Reads", c("teste1","teste2")),
                                textInput("adapter", "Adapter", "CTGTCTCTTATACACATCT")
                         ),
                         column(4,
                                selectInput("source", "Source", c("teste1","teste2")),
                                textInput("path", "Path", ""),
                                fileInput("table1", "input table")

                         )
                         
                       ),
                       br(),br(),
                       # Create a new row for the table.
                       fluidRow(
                         DT::dataTableOutput("table")
                       )),
              tabPanel("about")
  )
  )
  
)