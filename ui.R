library(ggplot2)
library(shinydashboard)
library(shinyjs)

fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
    tags$style("#erro{color: red;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
    ),
    tags$style("#done{color: green;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
    )
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
                                textInput("genome", "Genome", "")
                         ),
                         column(4,
                                selectInput("assay", "Assay", c("teste1","teste2")),
                                numericInput("length_reads", "Length Reads", c("teste1","teste2")),
                                textInput("adapter", "Adapter", "CTGTCTCTTATACACATCT"),
                                textInput("folder_name", "File", "")
                         ),
                         column(4,
                                selectInput("source", "Source", c("teste1","teste2")),
                                textInput("path", "Path", ""),
                                fileInput("datafile", "input table"),
                                actionButton("run","Run")

                         )
                         
                       ),
                       br(),
                       div(id = "div_erro",
                       verbatimTextOutput("erro")),
                       div(id = "div_done",
                       verbatimTextOutput("done")),
                       br(),
                       # Create a new row for the table.
                       fluidRow(
                         rHandsontableOutput('table') %>%
                           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       )),
              tabPanel("about")
  )
  )
  
)