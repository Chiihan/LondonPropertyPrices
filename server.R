library(shiny)
library(xgboost)
library(data.table)
daModel = xgb.load('bstMed')
central = 0
#qa = predict(daModel,t(as.matrix(xAppTest)))
shinyClassifiaz = as.character(c("1-100,000", "100,000-200,000","200,000-300,000","300,000-400,000","400,000-500,000",     
                                 "500,000-600,000",      "600,000-700,000",      "700,000-800,000",      "800,000-900,000",      "900,000-1,000,000",     
                                 "1-1.25 million",   "1.25 - 1.5 million", "1.5 - 1.75 million", "1.75 - 2 million",   "2 - 10 million",     
                                 "10 - 100 million"))
centralBors = as.vector(c(6,7,12,13,14,19,20,22,23,25,28,30,31,32))
paraText =  "This shiny app uses random forest gradient boosting to provide predictions on 
    the selling price of a house in London. Enter details on the left and press submit and 
the model will provide a approximate selling price. Built from around 9800 examples from Zoopla's 
house listing API combined with London borough and train station spaital data."

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  
  NewData <- reactive({ 
if ((input$borough %in% centralBors) == TRUE) {
  central = 1
}
else {
  central = 0
}
    matrix(data = c(as.numeric(input$borough),input$dist2stn,as.numeric(input$prop_type),input$num_bedrooms,
              input$num_bathrooms, input$num_recepts,0),nrow=7)
  })
  

  output$text3 <- renderText({ 
    gg <- shinyClassifiaz[max.col(t(predict(daModel,t(NewData()))))]
    save(gg,file ='lol.rdata')
    cc <-   paste('This property would be worth around £', gg, '.', sep = '')
    
  }) 
                       
                             
   output$logo <- renderUI('<a href="http://www.zoopla.co.uk/"><img src="http://www.zoopla.co.uk/static/images/mashery/powered-by-zoopla-150x73.png" width="150" height="73" title="Property information powered by Zoopla" alt="Property information powered by Zoopla" border="0"></a>')
})

#runApp("censusVis", display.mode = "showcase")
