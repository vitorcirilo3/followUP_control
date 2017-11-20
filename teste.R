library(shiny)
library(rhandsontable)

ui=fluidPage(
  rHandsontableOutput('table'),
  verbatimTextOutput('selected')
)

server=function(input,output,session)({
  DF = data.frame(val = 1:10,
                  bool = TRUE,
                  big = LETTERS[1:10],
                  small = factor(letters[1:10]),
                  dt = seq(from = Sys.Date(), by = "days", length.out = 10),
                  stringsAsFactors = FALSE)
  
  
  
  output$table=renderRHandsontable(
    rhandsontable(DF, search = TRUE, width = 550, height = 300) %>%
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
}) # end server
shinyApp(ui = ui, server = server)

