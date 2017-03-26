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
  rv <- reactiveValues()
  rv$data <- NULL
  rv$startDate <- NULL
  rv$endDate <- NULL
  observe({  # will 'obseve' the button press
    if(input$go) {
      print("rename x column name to 'date'")
      colnames(data)[which(names(data)==input$xvarID)] <- "date"
      data$date <- as.Date(data$date)
      startDate <- data$date[1]
      endDate <- data$date[length(data$date)]
#      data$date <- data[,which(names(data)==input$xvarID)]
      # get selected variable name list
      print("get selected variable from selectizeInput")
      selectedVars <- input$selectVar2PlotID
      if(length(selectedVars)<1) {
        stop ("Please select at lease one variable!!")
      }
      # subset data using the date range and selected variables list
      data2use <- subset(data,date>=as.Date(as.character(input$dateRangeID[1])) 
                         & date <= as.Date(as.character(input$dateRangeID[2])),
                         select = c('date',selectedVars))
      # Normalize data2use if minmax chosen
      if(input$normMethodID == 'minmax') {
        print('minmax normalization selected!')
        n <- dim(data2use)[2] 
        data2use[selectedVars] <- lapply(data2use[selectedVars], function(x) (x - min(x))/(max(x)-min(x)) )
      }
      rv$data <- data2use
      rv$startDate <- startDate
      rv$endDate <- endDate
      return(rv)
    }
    
  })  

    xymelt <-eventReactive(input$go, {melt(rv$data,id.vars = 'date')}) 
    #observeEvent(input$go, {write.csv(xymelt(), file = './www/test.csv')})
    
    
#    output$textID <- renderText(str(rv$data))
    output$textID <- renderText(paste0('debug',as.character(rv$startDate)))
    #output$textID <- renderText(class(input$normMethodID))
    #output$textID <- renderText(str(normalizedData2Use()))
    #output$textID <- renderText(str(data2use()[,2:dim(data2use())[2]]))
    output$dateID <- renderUI(dateRangeInput('dateRangeID',paste0("Date Range:",rv$startDate,' to ',rv$endDate), start = rv$startDate, 
                   end = rv$endDate, min = rv$startDate, max = rv$endDate))
    output$plot <- renderPlot({
       # ggplot(data = data2use(), aes(x = date, y = selectedVars())) +
        #  geom_line()
      ggplot(xymelt(), aes(x = date, y = value, color=variable)) + geom_line() +
        geom_point(data = xymelt(),aes(x = date, y = value, color=variable))
        
         
    })
   
   output$table <- renderDataTable(rv$data)
})
