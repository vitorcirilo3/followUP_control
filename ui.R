library(ggplot2)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(mongolite)
library(shinyTable)
library(shiny)
library(rhandsontable)
library(compare)

rm(list=ls())

list_path <- system("ls -d /bio/share_bio/illumina/*/", intern = TRUE)

fluidPage(
  list_path2 <- vector(),
  for(i in 1:length(list_path)){
    aux <- strsplit(list_path[[i]],"/")
    aux <- aux[[1]]
    list_path2[i] <- aux[length(aux)]
  },
  
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
                                dateInput("date", "date",format = "yy-mm-dd"),
                                selectInput("flow_cell", "Flow Cell", c("teste1","teste2")),
                                selectInput("genome", "Genome", c("teste1","teste2")),
                                actionButton("run","Run")
                         ),
                         column(4,
                                selectInput("assay", "Assay", c("teste1","teste2")),
                                selectInput("application", "Application", c("teste1","teste2")),
                                numericInput("length_reads", "Length Reads", c("teste1","teste2")),
                                textInput("adapter", "Adapter", "CTGTCTCTTATACACATCT")
                         ),
                         column(4,
                                selectInput("source", "Source", c("teste1","teste2")),
                                #selectInput("folder_name", "File", c(list_path2)),
                                uiOutput("factor1"),
                                fileInput("datafile", "input table")

                         )
                         
                       ),
                       br(),
                       verbatimTextOutput("date_msg"),
                       div(id = "div_erro",
                       verbatimTextOutput("erro")),
                       div(id = "div_done",
                       verbatimTextOutput("done")),
                       actionButton("save","save"),
                       br(),br(),
                       # Create a new row for the table.
                       fluidRow(
                         rHandsontableOutput('table') %>%
                           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       )),
              tabPanel("about",
                       includeMarkdown(file.path("text", "about_blind.md")))
  )
  )
  
)
