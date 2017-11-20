# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(mongolite)
library(shinyTable)
library(shiny)
library(rhandsontable)

function(input, output) {
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  collection <- "followUp_itv"
  dbname <- "status_proc"
  conect_mongo <- "mongodb://localhost:27017"
  mongo = mongo(collection = collection, db = dbname, url = conect_mongo,
                verbose = FALSE, options = ssl_options())


  
  # Filter data based on selections
  # output$table <- DT::renderDataTable({
  #   
  #   data <-  mongo$find(fields = '{"source":"","path":""}')
  #   
  #   data <- data[,-1]
  #   
  #   #data <- mpg
  #   # if (input$man != "All") {
  #   #   data <- data[data$manufacturer == input$man,]
  #   # }
  #   # if (input$cyl != "All") {
  #   #   data <- data[data$cyl == input$cyl,]
  #   # }
  #   # if (input$trans != "All") {
  #   #   data <- data[data$trans == input$trans,]
  #   # }
  #   DT::datatable(data, options = list(lengthChange = T,
  #                                      lengthMenu = c(10, 25, 50) ), rownames= FALSE
  #   )
  #   
  # })
  
  data <-  mongo$find(fields =  '{"_id" : 0}')
  
  if(length(data) != 0){
    data$status <- "ok"
    data$status[1] <- "seq"
    
    #data <- data.frame(matrix(ncol = 7, nrow = 2))
    #colnames(data) <- c("status","source","file_name","ITV_number","genome","runner","path")
    #data[,1] <- "ok4"
    
    output$table <- renderRHandsontable(
      rhandsontable(data, search = TRUE, height = 700) %>%
        hot_cols(columnSorting = TRUE, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (value == 'seq') {
              td.style.background = 'pink';
             } else if (value == 'ok') {
              td.style.background = 'lightgreen';
             }
           }") %>%
        hot_context_menu(
          customOpts = list(useShinyjs(),
            search = list(name = "Search",
                          callback = htmlwidgets::JS(
                            "function (key, options) {
                         var srch = prompt('Search criteria');

                         this.search.query(srch);
                         this.render();
                       }"))))
    )
  }
  
  
  

  
   
  
  
  observeEvent(input$run, {
    
    hide(id = "div_erro")
    hide(id = "div_done")
    
    if(is.na(input$length_reads) == T | is.na(input$genome) == T | is.na(input$path) == T | is.na(input$folder_name) == T | is.na(input$adapter) == T | is.null(input$datafile) == T |
       input$length_reads == "" | input$length_reads <= 0 | is.na(input$genome) == "" | is.na(input$path) == "" | is.na(input$folder_name) == "" | is.na(input$adapter) == "" | is.null(input$datafile) == ""){
      show(id = "div_erro")
      output$erro <- renderPrint({ print("missing values in input or negative length")}) 
    }else{
      print(input$length_reads)
      inFile <- input$datafile
      file <- read.csv(inFile$datapath)
      #print(test)
      
      #file <- read.csv("data.csv", sep=',')
      df <- as.data.frame(sapply(file,gsub,pattern='"',replacement=""))
      df <- data.frame(do.call('rbind', strsplit(as.character(df[,1]),',',fixed=TRUE)))
      aux_df <- data.frame(do.call('rbind', strsplit(as.character(df$X4),'-',fixed=TRUE)))
      df <- cbind(df,aux_df)
      aux_df <- 0
      aux_df <- data.frame(do.call('rbind', strsplit(as.character(df$X5),'-',fixed=TRUE)))
      df <- cbind(df,aux_df)
      df <- df[,-4:-5]
      colnames(df) <- c("Sample ID","Project","Well","index_1","index_12","index_2","index_22")
      
      
      matrix_final <- matrix(nrow=nrow(df), ncol = 7)
      colnames(matrix_final) <- c("status","source","file_name","ITV_number","genome","runner","path")
      matrix_final <- as.data.frame(matrix_final)
      
      
      matrix_partial <- matrix(nrow=nrow(df), ncol = 10)
      colnames(matrix_partial) <- c("Sample_ID","Sample_Name","Sample_Plate","Sample_Well","I7_Index_ID","index","I5_Index_ID","index2","Sample_Project","Description")
      matrix_partial <- as.data.frame(matrix_partial)
      
      aux <- input$date
      aux_date <- data.frame(do.call('rbind', strsplit(as.character(aux),'-',fixed=TRUE)))
      aux_date$X1 <- as.character(aux_date$X1)
      date_id <- paste0(substrRight(aux_date$X1, 2),aux_date$X2,aux_date$X3)
      
      
      
      for(i in 1:nrow(matrix_partial)){
        matrix_partial$Sample_ID[i] <- as.character(df$`Sample ID`[i])
        matrix_partial$I7_Index_ID[i] <- as.character(df$`index_1`[i])
        matrix_partial$index[i] <- as.character(df$index_12[i])
        matrix_partial$I5_Index_ID[i] <- as.character(df$index_2[i])
        matrix_partial$index2[i] <- as.character(df$index_22[i])
        matrix_partial$Description[i] <- as.character(df$Project[i])
        matrix_partial$Sample_Project[i] <- paste0(date_id,"_",input$flow_cell)
        matrix_partial$Sample_Name[i] <- ""
        matrix_partial$Sample_Plate[i] <- ""
        matrix_partial$Sample_Well[i] <- ""
        
        
        matrix_final$itv_number[i] <- as.character(df$`Sample ID`[i])
        matrix_final$path[i] <- as.character(input$path)
        matrix_final$source[i] <- input$source
        matrix_final$status[i] <- "-"
        matrix_final$genome[i] <- input$genome
        matrix_final$runner[i] <- "-"
        matrix_final$path[i] <- input$path
        matrix_final$file_name[i] <- input$folder_name
      }
      
      # Start writing to an output file
      sink('analysis-output.txt')
      
      # Do some stuff here
      cat("[Header]\n")
      cat("IEMFileVersion,4\n")
      cat(paste0("Date,",input$date,"\n"))
      cat(paste0("Workflow,GenerateFASTQ\n"))
      cat(paste0("Application,NextSeq FASTQ Only\n"))
      cat(paste0("Assay,QXTSureSelect\n"))
      cat(paste0("Description\n"))
      cat(paste0("Chemistry,Amplicon\n"))
      cat(paste0("\n"))

      cat("[Reads]\n")
      cat(paste0(input$length_reads,"\n"))
      cat(paste0(input$length_reads,"\n"))
      cat(paste0("\n"))

      cat("[Settings]\n")
      cat(paste0("Adapter,",input$adapter,"\n"))
      cat(paste0("\n"))

      cat("[Data]\n")
      
      #cat(matrix_partial)

  
      
      # Stop writing to the file
      sink()
      
      
      # Append to the file
      sink('analysis-output.txt', append=TRUE)
      sink()
      
      mongo$insert(matrix_final)
      
      data <-  mongo$find(fields =  '{"_id" : 0}')
      output$table <- renderRHandsontable(
        rhandsontable(data, search = TRUE, height = 700) %>%
          hot_cols(columnSorting = TRUE, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             if (value == 'seq') {
              td.style.background = 'pink';
             } else if (value == 'ok') {
              td.style.background = 'lightgreen';
             }
           }") %>%
          hot_context_menu(
            customOpts = list(
              search = list(name = "Search",
                            callback = htmlwidgets::JS(
                              "function (key, options) {
                         var srch = prompt('Search criteria');

                         this.search.query(srch);
                         this.render();
                       }"))))
      )
      
      show(id = "div_done")
      output$done <- renderPrint({ print("added with success!")}) 
      
    }

  })

  
}