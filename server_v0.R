#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(reshape2)
library(ggplot2)
source('global.R')
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  observe({  # will 'obseve' the button press
    if(input$go) {
      print("rename column name")
      colnames(data)[which(names(data)==input$xvarID)] <- "date"
    }
    
  })  
    selectedVars <- eventReactive(input$go, {input$selectVar2PlotID})
    data2use <- eventReactive(input$go,{ subset(data,date>=as.Date(as.character(input$dateRangeID[1])) 
                                                & date <= as.Date(as.character(input$dateRangeID[2])),
                                                select = c('date',selectedVars()))
      })
     normalizedData2Use <-  reactive({
       if(input$normMethodID == 'minmax') {
          print('minmax selected!')
          n <- dim(data2use())[2] 
         test <- as.data.frame(data2use()[,2:n])
         test <- as.data.frame(apply(test,2, function(x) (x - min(x))/(max(x)-min(x)) ))
         test$date <- data2use()$date 
     }
      })
    xymelt <-eventReactive(input$go, {melt(data2use(),id.vars = 'date')}) 
    observeEvent(input$go, {write.csv(xymelt(), file = './www/test.csv')})
    if(length(selectedVars)<1) {
      stop ("Please select at lease one variable!!")
    }
    
    #output$textID <- renderText(class(input$normMethodID))
    output$textID <- renderText(str(normalizedData2Use()))
    #output$textID <- renderText(str(data2use()[,2:dim(data2use())[2]]))
    output$plot <- renderPlot({
       # ggplot(data = data2use(), aes(x = date, y = selectedVars())) +
        #  geom_line()
      ggplot(xymelt(), aes(x = date, y = value, color=variable)) + geom_line() +
        geom_point(data = xymelt(),aes(x = date, y = value, color=variable))
        
         
    })
   
   output$table <- renderDataTable(data2use())
})
