#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# data <- read.csv("BTC_USD.csv")
data <- read.csv("BTC_USD.csv")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$oneWeek <- renderPlot({
        plotData <- data[(nrow(data)-6):nrow(data),]
        plot(plotData$Close, type = "l")
    })
    
    output$oneYear <- renderPlot({
        plotData <- data[(nrow(data)-355):nrow(data),]
        plot(plotData$Close, type = "l")
    })

})
