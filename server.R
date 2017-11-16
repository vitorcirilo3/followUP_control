# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(mongolite)

function(input, output) {
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  collection <- "folloup_itv"
  dbname <- "status_proc"
  conect_mongo <- "mongodb://localhost:27017"
  mongo = mongo(collection = collection, db = dbname, url = conect_mongo,
                verbose = FALSE, options = ssl_options())


  
  # Filter data based on selections
  output$table <- DT::renderDataTable({
    
    data <-  mongo$find(fields = '{"source":"","path":""}')
    
    data <- data[,-1]
    
    #data <- mpg
    # if (input$man != "All") {
    #   data <- data[data$manufacturer == input$man,]
    # }
    # if (input$cyl != "All") {
    #   data <- data[data$cyl == input$cyl,]
    # }
    # if (input$trans != "All") {
    #   data <- data[data$trans == input$trans,]
    # }
    DT::datatable(data, options = list(lengthChange = T,
                                       lengthMenu = c(10, 25, 50) ), rownames= FALSE
    )
    
  })
  
  observeEvent(input$run, {

    
    file <- read.csv("data.csv", sep=',')
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
    colnames(matrix_final) <- c("source","path","itv_number","genome","status","runner","follow_up","tt_reads","tt_bases")
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
      
      
      matrix_final$itv_number <- as.character(df$`Sample ID`[i])
      matrix_final$path <- as.character(input$path)
      matrix_final$source <- input$source
    }
    
    
    mongo$insert(matrix_final)
    

  })

  
}