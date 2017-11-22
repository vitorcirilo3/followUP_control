# Load the ggplot2 package which provides
# the 'mpg' dataset.

unique.rows <- function (df1, df2) { 
  # Returns any rows of df1 that are not in df2 
  out <- NULL 
  for (i in 1:nrow(df1)) { 
    found <- FALSE 
    for (j in 1:nrow(df2)) { 
      if (all(df1[i,] == df2[j,])) { 
        found <- TRUE 
        break 
      } 
    } 
    if (!found) out <- rbind(out, df1[i,]) 
  } 
  out 
} 

function(input, output) {
  
  # Create the object with no values
  values <- reactiveValues()
  
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
  
  #data <-  mongo$find(fields =  '{"_id" : 0}')
  data <-  mongo$find(fields =  '{"_id":1, "status":1, "file_name":1,"itv_number":1,"genome":1,"runner":1,"path":1 }')
  values$data <- data
  values$database <- data
  
  if(length(data) != 0){

    #data <- data.frame(matrix(ncol = 7, nrow = 2))
    #colnames(data) <- c("status","source","file_name","ITV_number","genome","runner","path")
    #data[,1] <- "ok4"
    
    
    #values$data <- data
    #data <- data[,-1]
    
    output$table <- renderRHandsontable(
      rhandsontable(values$data, search = TRUE, height = 700) %>%
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

                         this.search.query(srcinput$folder_nameh);
                         this.render();
                       }"))))
    )
  }
  
  
  

  
   
  
  
  observeEvent(input$run, {
    
    hide(id = "div_erro")
    hide(id = "div_done")
    
    if(is.na(input$length_reads) == T | is.na(input$genome) == T | is.na(input$folder_name) == T | is.na(input$adapter) == T | is.null(input$datafile) == T |
       input$length_reads == "" | input$length_reads <= 0 | is.na(input$genome) == "" | is.na(input$folder_name) == "" | is.na(input$adapter) == "" | is.null(input$datafile) == ""){
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
      
      path_base <- paste0("/home/vitorcirilo/Documentos/",input$folder_name,"/")
      
      for(i in 1:nrow(matrix_partial)){
        matrix_partial$Sample_ID[i] <- as.character(df$`Sample ID`[i])
        matrix_partial$I7_Index_ID[i] <- as.character(df$`index_1`[i])
        matrix_partial$index[i] <- as.character(df$index_12[i])
        matrix_partial$I5_Index_ID[i] <- as.character(df$index_2[i])
        matrix_partial$index2[i] <- as.character(df$index_22[i])
        matrix_partial$Description[i] <- as.character(df$Project[i])
        matrix_partial$Sample_Project[i] <- paste0(date_id,"_",input$flow_cell)
        matrix_partial$Sample_Name[i] <- ""
        matrix_partial$Sample_Plate[i] <- ",,"
        matrix_partial$Sample_Well[i] <- ""
        
        
        matrix_final$itv_number[i] <- as.character(df$`Sample ID`[i])
        matrix_final$path[i] <- as.character(path_base)
        matrix_final$source[i] <- input$source
        matrix_final$status[i] <- "-"
        matrix_final$genome[i] <- input$genome
        matrix_final$runner[i] <- "-"
        matrix_final$file_name[i] <- input$folder_name
      }
      
      
      print(paste0(path_base,'analysis-output.txt'))
      # Start writing to an output file
      sink(paste0(path_base,'analysis-output.txt'))
      
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

      #matrix_partial <- matrix(c(2,5,6,4), nrow=5, ncol = 5)
      #matrix_partial <- as.data.frame(matrix_partial)
      s <- do.call(paste, as.data.frame(matrix_partial, stringsAsFactors=FALSE))
      s <- gsub('\\s+', ',', s)
      
      
      aux_matrix <- matrix(1,nrow=nrow(matrix_partial), ncol= 1)
      aux_matrix <- as.data.frame(aux_matrix)
      for(i in 1:nrow(matrix_partial)){
        aux_matrix[i,1] <- s[i]
      }
      
      colnames(aux_matrix) <- "Sample_ID,Sample_Name,Sample_Plate,Sample_Well,I7_Index_ID,index,I5_Index_ID,index2,Sample_Project,Description"
      
      # Stop writing to the file
      sink()
      
      
      # Append to the file
      sink(paste0(path_base,'analysis-output.txt'), append=TRUE)
      sink()
      
      
      write.table(aux_matrix, file=paste0(path_base,'analysis-output.txt'), sep=",", row.names=FALSE, append=TRUE, quote = FALSE)
      

      
      #values$matrix_final <- matrix_final
      mongo$insert(matrix_final)
      
      #data <-  mongo$find(fields =  '{"_id" : 0}')
      data <-  mongo$find(fields =  '{"_id":1, "status":1, "file_name":1,"itv_number":1,"genome":1,"runner":1,"path":1 }')
      values$data <- data
      values$database <- data
      #data <- data[,-1]
      
      
      output$table <- renderRHandsontable(
        rhandsontable(values$data, search = TRUE, height = 700) %>%
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
  
  
  observeEvent(input$save, { 
    
    df <- hot_to_r(input$table)
    aux_update <- unique.rows(df,values$database)
    
    print(aux_update$`_id`[1])
    print(aux_update$file_name[1])
    
    if(is.null(aux_update) == F){
      for(i in 1:nrow(aux_update)){
        #mongo$update('{"_id": {"$oid":"5a155e3596d02e0ebd07ac5e"}}', '{"$set":{"file_name":"hahaha"}}')
        query <- paste0('{"_id": {"$oid":"',aux_update$`_id`[i],'"}}')
        updater <- paste0('{"$set":{"file_name":"',aux_update$file_name[i],'"}}')
        mongo$update(query, updater)
      }
    }

    

    })

  
}