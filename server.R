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
  
  collection2 <- "followUp_itv_categories"
  dbname2 <- "status_proc"
  conect_mongo2 <- "mongodb://localhost:27017"
  mongo2 = mongo(collection = collection2, db = dbname2, url = conect_mongo2,
                verbose = FALSE, options = ssl_options())
  
  names_flowcell <-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "flow_cell"}')
  if(nrow(names_flowcell) == 0){
    names_flowcell <- ""
  }else{
    names_flowcell <- names_flowcell[,1]
    names_flowcell <- as.character(names_flowcell)
  }
  
  output$factor2 <- renderUI({
    selectInput("flow_cell", "Flow cell",c(names_flowcell))
  })
  
  
  names_genome<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "genome"}')
  if(nrow(names_genome) == 0){
    names_genome<- ""
  }else{
    names_genome <- names_genome[,1]
    names_genome <- as.character(names_genome)
  }
  output$factor3 <- renderUI({
    selectInput("genome", "Genome",c(names_genome))
  })
  
  output$factor7 <- renderUI({
    selectInput("genome2", "Genome",c(names_genome))
  })
  
  
  names_assay <-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "assay"}')
  if(nrow(names_assay) == 0){
    names_assay <- ""
  }else{
    names_assay <- names_assay[,1]
    names_assay <- as.character(names_assay)
  }
  output$factor4 <- renderUI({
    selectInput("assay", "Assay",c(names_assay))
  })
  
  
  names_application<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "application"}')
  if(nrow(names_application) == 0){
    names_application <- ""
  }else{
    names_application <- names_application[,1]
    names_application <- as.character(names_application)
  }
  
  output$factor5 <- renderUI({
    selectInput("application", "Application",c(names_application))
  })
  
  names_chemistry<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "chemistry"}')
  if(nrow(names_chemistry) == 0){
    names_chemistry <- ""
  }else{
    names_chemistry <- names_chemistry[,1]
    names_chemistry<- as.character(names_chemistry)
  }
  
  output$factor6 <- renderUI({
    selectInput("chemistry", "Chemistry",c(names_chemistry))
  })


  
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
  data <-  mongo$find(fields =  '{"_id":1, "status":1, "illumina_path":1,"itv_number":1,"genome":1,"runner":1,"followup":1,"tt_reads":1,"tt_base":1,"size_file":1 }')
  values$data <- data
  values$database <- data
  values$status <- data$status
  values$genome <- data$genome
  
  ### plot 1 ####
  output$output_plot1 <- renderPlotly({
    
    data_plot1 <- matrix(ncol=2, nrow = nrow(values$data))
    colnames(data_plot1) <- c("genome","status")
    data_plot1 <- as.data.frame(data_plot1)
    data_plot1$status <- values$status
    data_plot1$genome <- values$genome
    
    output_plot1 <- data_plot1 %>% 
      count(genome)
    
    output_plot1 <- as.data.frame(output_plot1)
    
    output$result1 <- renderText({paste0("Total of samples: ",nrow(values$database))}) 
    
    p <- plot_ly(output_plot1, labels = ~genome, values = ~n, type = 'pie', textposition = 'inside',
                 textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF')) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    ggplotly(p)
    })
  
  
  ### plot 2 ####
  output$output_plot2 <- renderPlotly({
    data_plot2 <- matrix(ncol=2, nrow = nrow(values$data))
    colnames(data_plot2) <- c("genome","status")
    data_plot2 <- as.data.frame(data_plot2)
    data_plot2$status <- values$status
    data_plot2$genome <- values$genome
    
    data_plot2 <- data_plot2[data_plot2$genome == input$genome2,]

    output_plot2 <- data_plot2 %>% 
      count(status)
    
    output_plot2 <- as.data.frame(output_plot2)
    
    p2 <- plot_ly(output_plot2, labels = ~status, values = ~n, type = 'pie', textposition = 'inside',
                 textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF')) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    ggplotly(p2)
  })
  
  

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
  
  #data <-  mongo$find(fields =  '{"_id" : 0}')
  data2 <-  mongo2$find(fields =  '{"_id":1, "category_name":1, "category_type":1}')
  values$data_category <- data2
  values$database_category <- data2
  

  
  if(length(data2) != 0){
    
    #data <- data.frame(matrix(ncol = 7, nrow = 2))
    #colnames(data) <- c("status","source","file_name","ITV_number","genome","runner","path")
    #data[,1] <- "ok4"
    
    
    #values$data <- data
    #data <- data[,-1]
    
    output$table_categories <- renderRHandsontable(
      rhandsontable(values$data_category, search = TRUE, height = 700) %>%
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
  
  
  

  
  output$date_msg <- renderPrint({ 
    newdate<- strptime(as.character(input$date), "%Y-%m-%d")
    system_command <- paste0("ls -d /bio/share_bio/illumina/",format(newdate, "%y%m%d"),"*/")
    list_path <- system(system_command, intern = TRUE)
    list_path2 <- vector()

    if(length(list_path) != 0){
      for(i in 1:length(list_path)){
        aux <- strsplit(list_path[[i]],"/")
        aux <- aux[[1]]
        list_path2[i] <- aux[length(aux)]
      }
    }else{
      system_command <- paste0("ls -d /bio/share_bio/illumina/*/")

      list_path <- system(system_command, intern = TRUE)
      for(i in 1:length(list_path)){
        aux <- strsplit(list_path[[i]],"/")
        aux <- aux[[1]]
        list_path2[i] <- aux[length(aux)]
      }
    }

    output$factor1 <- renderUI({
      selectInput("folder_name", "File",c(list_path2))
    })
    }) 
  
  #### add item ####
  observeEvent(input$run, {
    
    hide(id = "div_erro")
    hide(id = "div_done")
    hide(id = "div_update")
    
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
      
      
      matrix_final <- matrix(nrow=nrow(df), ncol = 9)
      colnames(matrix_final) <- c("status","illumina_path","ITV_number","genome","runner","followup","tt_reads","tt_base","size_file")
      matrix_final <- as.data.frame(matrix_final)
      
      
      matrix_partial <- matrix(nrow=nrow(df), ncol = 10)
      colnames(matrix_partial) <- c("Sample_ID","Sample_Name","Sample_Plate","Sample_Well","I7_Index_ID","index","I5_Index_ID","index2","Sample_Project","Description")
      matrix_partial <- as.data.frame(matrix_partial)
      
      aux <- input$date
      aux_date <- data.frame(do.call('rbind', strsplit(as.character(aux),'-',fixed=TRUE)))
      aux_date$X1 <- as.character(aux_date$X1)
      date_id <- paste0(substrRight(aux_date$X1, 2),aux_date$X2,aux_date$X3)
      
      path_base <- paste0("/bio/share_bio/illumina/",input$folder_name,"/")
      
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
        matrix_final$followup[i] <- as.character("/bio/share_bio/projects/...")
        matrix_final$status[i] <- "-"
        matrix_final$genome[i] <- input$genome
        matrix_final$runner[i] <- "-"
        matrix_final$tt_reads[i] <- "-"
        matrix_final$tt_base[i] <- "-"
        matrix_final$size_file[i] <- "-"
        matrix_final$illumina_path[i] <- input$folder_name 
        
      }
      
      #system_command <- paste0("cp /www/demultiplex.pbs /",input$folder_name,"/demultiplex.pbs")
      system_command <- paste0("cp www/demultiplex.pbs /bio/share_bio/illumina/",input$folder_name,"/demultiplex.pbs")
      system(system_command)
      system_command <- paste0("qsub /bio/share_bio/illumina/",input$folder_name,"/demultiplex.pbs")
      system(system_command)
      
      print(paste0(path_base,'SampleSheet.csv'))
      # Start writing to an output file
      sink(paste0(path_base,'SampleSheet.csv'))
      
      # Do some stuff here
      cat("[Header]\n")
      cat("IEMFileVersion,4\n")
      cat(paste0("Date,",input$date,"\n"))
      cat(paste0("Workflow,GenerateFASTQ\n"))
      cat(paste0("Application,",input$application," FASTQ Only\n"))
      cat(paste0("Assay,",input$assay,"\n"))
      cat(paste0("Description\n"))
      cat(paste0("Chemistry,",input$chemistry,"\n"))
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
      sink(paste0(path_base,'SampleSheet.csv'), append=TRUE)
      sink()
      
      
      write.table(aux_matrix, file=paste0(path_base,'SampleSheet.csv'), sep=",", row.names=FALSE, append=TRUE, quote = FALSE)
      

      
      #values$matrix_final <- matrix_final
      mongo$insert(matrix_final)
      
      #data <-  mongo$find(fields =  '{"_id" : 0}')
      data <-  mongo$find(fields =  '{"_id":1, "status":1, "illumina_path":1,"itv_number":1,"genome":1,"runner":1,"followup":1 }')
      values$data <- data
      values$database <- data
      values$status <- data$status
      values$genome <- data$genome
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
  
  
  #### save category ####
  observeEvent(input$bt_add_category, {
    
    hide("div_update_category")
    
      matrix_category <- matrix(nrow=1, ncol = 2)
      colnames(matrix_category) <- c("category_name","category_type")
      matrix_category <- as.data.frame(matrix_category)
      matrix_category$category_name <- input$category_name
      matrix_category$category_type <- input$category_type
      
      mongo2$insert(matrix_category)
      
      #data <-  mongo$find(fields =  '{"_id" : 0}')
      data2 <-  mongo2$find(fields =  '{"_id":1, "category_name":1, "category_type":1}')
      values$data_category <- data2
      values$database_category <- data2
      
      if(length(data2) != 0){
        
        #data <- data.frame(matrix(ncol = 7, nrow = 2))
        #colnames(data) <- c("status","source","file_name","ITV_number","genome","runner","path")
        #data[,1] <- "ok4"
        
        
        #values$data <- data
        #data <- data[,-1]
        
        output$table_categories <- renderRHandsontable(
          rhandsontable(values$data_category, search = TRUE, height = 700) %>%
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
      
      show(id = "div_update_category")
      output$txt_update_category <- renderPrint({ print("categories table update with success!")})
      
      names_flowcell <-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "flow_cell"}')
      if(nrow(names_flowcell) == 0){
        names_flowcell <- ""
      }else{
        names_flowcell <- names_flowcell[,1]
        names_flowcell <- as.character(names_flowcell)
      }
      
      output$factor2 <- renderUI({
        selectInput("flow_cell", "Flow cell",c(names_flowcell))
      })
      
      
      names_genome<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "genome"}')
      if(nrow(names_genome) == 0){
        names_genome<- ""
      }else{
        names_genome <- names_genome[,1]
        names_genome <- as.character(names_genome)
      }
      output$factor3 <- renderUI({
        selectInput("genome", "Genome",c(names_genome))
      })
      
      output$factor7 <- renderUI({
        selectInput("genome2", "Genome",c(names_genome))
      })
      
      
      names_assay <-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "assay"}')
      if(nrow(names_assay) == 0){
        names_assay <- ""
      }else{
        names_assay <- names_assay[,1]
        names_assay <- as.character(names_assay)
      }
      output$factor4 <- renderUI({
        selectInput("assay", "Assay",c(names_assay))
      })
      
      
      names_application<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "application"}')
      if(nrow(names_application) == 0){
        names_application <- ""
      }else{
        names_application <- names_application[,1]
        names_application <- as.character(names_application)
      }
      
      output$factor5 <- renderUI({
        selectInput("application", "Application",c(names_application))
      })
      
      names_chemistry<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "chemistry"}')
      if(nrow(names_chemistry) == 0){
        names_chemistry <- ""
      }else{
        names_chemistry <- names_chemistry[,1]
        names_chemistry<- as.character(names_chemistry)
      }
      
      output$factor6 <- renderUI({
        selectInput("chemistry", "Chemistry",c(names_chemistry))
      })
    
        
  })
  
  #### update category ####
  observeEvent(input$bt_update_category, {
    
    hide("div_update_category")
    
    if(length(input$table_categories$params$data) >= nrow(values$database_category)){
      if(is.null(input$"table_categories") == F){
        df <- hot_to_r(input$table_categories)
        aux_update <- unique.rows(df,values$database_category)
      }else{
        aux_update <- NULL
      }
      
      if(is.null(aux_update) == F){
        for(i in 1:nrow(aux_update)){
          #mongo$update('{"_id": {"$oid":"5a155e3596d02e0ebd07ac5e"}}', '{"$set":{"file_name":"hahaha"}}')
          query <- paste0('{"_id": {"$oid":"',aux_update$`_id`[i],'"}}')
          updater <- paste0('{"$set":{"category_name":"',aux_update$category_name[i],'"}}')
          mongo2$update(query, updater)
        }
      }
    }else{
      df <- hot_to_r(input$table_categories)
      aux_update <- unique.rows(values$database_category,df)
      
      for(i in 1:nrow(aux_update)){
        #mongo$update('{"_id": {"$oid":"5a155e3596d02e0ebd07ac5e"}}', '{"$set":{"file_name":"hahaha"}}')
        query <- paste0('{"_id": {"$oid":"',aux_update$`_id`[i],'"}}')
        updater <- paste0('{"$set":{"category_name":"',aux_update$category_name[i],'"}}')
        mongo2$remove(query)
      }
    }
    


    #data <-  mongo$find(fields =  '{"_id" : 0}')
    data2 <-  mongo2$find(fields =  '{"_id":1, "category_name":1, "category_type":1}')
    values$data_category <- data2
    values$database_category <- data2

    if(length(data2) != 0){

      #data <- data.frame(matrix(ncol = 7, nrow = 2))
      #colnames(data) <- c("status","source","file_name","ITV_number","genome","runner","path")
      #data[,1] <- "ok4"


      #values$data <- data
      #data <- data[,-1]

      output$table_categories <- renderRHandsontable(
        rhandsontable(values$data_category, search = TRUE, height = 700) %>%
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

    show(id = "div_update_category")
    output$txt_update_category <- renderPrint({ print("categories table update with success!")})
    
    names_flowcell <-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "flow_cell"}')
    if(nrow(names_flowcell) == 0){
      names_flowcell <- ""
    }else{
      names_flowcell <- names_flowcell[,1]
      names_flowcell <- as.character(names_flowcell)
    }
    
    output$factor2 <- renderUI({
      selectInput("flow_cell", "Flow cell",c(names_flowcell))
    })
    
    
    names_genome<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "genome"}')
    if(nrow(names_genome) == 0){
      names_genome<- ""
    }else{
      names_genome <- names_genome[,1]
      names_genome <- as.character(names_genome)
    }
    output$factor3 <- renderUI({
      selectInput("genome", "Genome",c(names_genome))
    })
    
    output$factor7 <- renderUI({
      selectInput("genome2", "Genome",c(names_genome))
    })
    
    names_assay <-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "assay"}')
    if(nrow(names_assay) == 0){
      names_assay <- ""
    }else{
      names_assay <- names_assay[,1]
      names_assay <- as.character(names_assay)
    }
    output$factor4 <- renderUI({
      selectInput("assay", "Assay",c(names_assay))
    })
    
    
    names_application<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "application"}')
    if(nrow(names_application) == 0){
      names_application <- ""
    }else{
      names_application <- names_application[,1]
      names_application <- as.character(names_application)
    }
    
    output$factor5 <- renderUI({
      selectInput("application", "Application",c(names_application))
    })
    
    names_chemistry<-  mongo2$find(fields = '{"_id":0, "category_name":1}', query = '{"category_type" : "chemistry"}')
    if(nrow(names_chemistry) == 0){
      names_chemistry <- ""
    }else{
      names_chemistry <- names_chemistry[,1]
      names_chemistry<- as.character(names_chemistry)
    }
    
    output$factor6 <- renderUI({
      selectInput("chemistry", "Chemistry",c(names_chemistry))
    })
    
    
    
  })
  
  #### save changes ####
  observeEvent(input$save, { 
    
    hide(id = "div_erro")
    hide(id = "div_done")
    hide(id = "div_update")
    
    if(length(input$table$params$data) == nrow(values$database)){
      df <- hot_to_r(input$table)
      aux_update <- unique.rows(df,values$database)
      
      if(is.null(aux_update) == F){
        for(i in 1:nrow(aux_update)){
          #mongo$update('{"_id": {"$oid":"5a155e3596d02e0ebd07ac5e"}}', '{"$set":{"file_name":"hahaha"}}')
          query <- paste0('{"_id": {"$oid":"',aux_update$`_id`[i],'"}}')
          updater <- paste0('{"$set":{"status":"',aux_update$status[i],'","runner":"',aux_update$runner[i],'","followup":"',aux_update$followup[i],'"}}')
          mongo$update(query, updater)
        }
        
        show(id = "div_update")
        output$update <- renderPrint({ print("values were updade with success!")}) 
      }
      
      data <-  mongo$find(fields =  '{"_id":1, "status":1, "illumina_path":1,"itv_number":1,"genome":1,"runner":1,"followup":1,"tt_reads":1,"tt_base":1,"size_file":1 }')
      values$data <- data
      values$database <- data
      values$status <- data$status
      values$genome <- data$genome
    }else if(length(input$table$params$data) < nrow(values$database)){
      
      df <- hot_to_r(input$table)
      aux_update <- unique.rows(values$database,df)
      
      if(is.null(aux_update) == F){
        for(i in 1:nrow(aux_update)){
          #mongo$update('{"_id": {"$oid":"5a155e3596d02e0ebd07ac5e"}}', '{"$set":{"file_name":"hahaha"}}')
          query <- paste0('{"_id": {"$oid":"',aux_update$`_id`[i],'"}}')
          mongo$remove(query)
        }
        
        show(id = "div_update")
        output$update <- renderPrint({ print("Itens were deleted with success!")}) 
      }
      
      data <-  mongo$find(fields =  '{"_id":1, "status":1, "illumina_path":1,"itv_number":1,"genome":1,"runner":1,"followup":1,"tt_reads":1,"tt_base":1,"size_file":1 }')
      values$data <- data
      values$database <- data
      values$status <- data$status
      values$genome <- data$genome
      
    }else{
      show(id = "div_erro")
      output$erro <- renderPrint({ print("One or more lines were add. You can add only using the add button. To realize the update of table you need delete these lines")}) 
    }
    })

  
}