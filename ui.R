library(ggplot2)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(mongolite)
library(shiny)
library(rhandsontable)
library(plotly)




rm(list=ls())

list_path <- system("ls -d /bio/share_bio/illumina/*/", intern = TRUE)
# file_names <- read.csv("www/names.csv", sep="\t")
# genome_names <- file_names[file_names$type == 'genome',]
# genome_names$names <- as.character(genome_names$names)
# application_names <- file_names[file_names$type == 'application',]
# application_names$names <- as.character(application_names$names)
# chemistry_names <- file_names[file_names$type == 'chemistry',]
# chemistry_names$names <- as.character(chemistry_names$names)

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
    ),
    tags$style("#update{color: green;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
    )
    ,
    tags$style("#txt_update_category{color: green;
                                 font-size: 14px;
                                 font-style: italic;
                                 }"
    ),
    tags$style(HTML('#div_status, #div_category{
                    border-bottom: 2px solid red;
    }')),
    tags$style(HTML('#run, #bt_add_category{background-color:#009183; color:white; width:300px}')),
    tags$style(HTML('#bt_update_category{background-color:#009183; color:white}')),
    tags$style(HTML('#save{background-color:#009183; color:white}'))
  ),
  
  titlePanel("Follow-UP ITV"),

  div(id="main_div",
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(type = "tabs",
              tabPanel("Overview status",
                       fluidRow(
                         column(4,
                                br(),
                                h3(textOutput("result1")),
                                br(),
                                plotlyOutput("output_plot1")
                         ),
                         column(4,
                                br(),
                                uiOutput("factor7"),
                                plotlyOutput("output_plot2")
                         )
                       #verbatimTextOutput("result1")
                       )),
              tabPanel("Status",
                       # Create a new Row in the UI for selectInputs
                       div(id="div_status",
                       fluidRow(
                         column(4,
                                br(),
                                dateInput("date", "Date",format = "yy-mm-dd"),
                                #selectInput("flow_cell", "Flow Cell", c("teste1","teste2")),
                                uiOutput("factor2"),
                                #selectInput("genome", "Genome", c(genome_names$names)),
                                uiOutput("factor3"),
                                textInput("adapter", "Adapter", "CTGTCTCTTATACACATCT")
                         ),
                         column(4,
                                br(),
                                #selectInput("assay", "Assay", c("QXTSureSelect")),
                                uiOutput("factor4"),
                                #selectInput("application", "Application", c(application_names$names)),
                                uiOutput("factor5"),
                                fileInput("datafile", "Input table"),
                                actionButton("run","Add")
                         ),
                         column(4,
                                br(),
                                #selectInput("chemistry", "Chemistry", c(chemistry_names$names)),
                                uiOutput("factor6"),
                                uiOutput("factor1"),
                                numericInput("length_reads", "Length Reads", c("teste1","teste2"))

                         )
                         
                       )),
                       verbatimTextOutput("date_msg"),
                       div(id = "div_erro",
                       verbatimTextOutput("erro")),
                       div(id = "div_done",
                            verbatimTextOutput("done")),
                       div(id = "div_update",
                           verbatimTextOutput("update")),
                       br(),
                       actionButton("save","Update Table"),
                       br(),br(),
                       # Create a new row for the table.
                       fluidRow(
                         rHandsontableOutput('table') %>%
                           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       )),
              tabPanel("Category",
                       div(id="div_category",
                       fluidRow(
                         column(2,
                                br(),
                                textInput("category_name", "Category name"),
                                selectInput("category_type", "Category type", c("flow_cell","genome","assay","application","chemistry"))
                         ),
                         column(2,
                                br(),br(),br(),br(),br(),br(),
                                actionButton("bt_add_category","Add")
                         )),
                       br()),
                       div(id = "div_update_category",
                           verbatimTextOutput("txt_update_category"),
                       br(),
                       actionButton("bt_update_category","Update")),
                       br(),
                       fluidRow(
                         rHandsontableOutput('table_categories') %>%
                           hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
                       ))#,
              #tabPanel("about",
              #includeMarkdown(file.path("/srv/shiny-server/followUP_control/text", "about_blind.md")))
  )
  )
  
)
