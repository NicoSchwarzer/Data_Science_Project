
### Server File for the Shiny Web-Application ###

# load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

# load data
data <- read.csv("topic_df.csv")
# test_data <- read.csv("toy_data.csv")

# modify data
data$date <- as.Date(data$date)
data$topic <- as.character(data$topic)

## Set up the server function ##
# i.e. make plots that will be shown #

shinyServer(function(input, output) {
  
  # Topics Plot
  output$topics_plot <- renderPlot({
    data %>%
      filter(topic %in% input$topics_displayed) %>%
      ggplot(aes(x = date, y = appearance)) + 
      geom_line(aes(color = topic)) +
      geom_smooth(method = "lm", aes(color = topic), alpha = 0.2, size = 0.1)
  })
  
  
  # Topics Plot
  output$topics_plot2 <- renderPlot({
    data %>%
      filter(topic %in% input$topics_displayed2) %>%
      ggplot(aes(x = date, y = appearance)) + 
      geom_line(aes(color = topic)) +
      geom_smooth(method = "lm", aes(color = topic), alpha = 0.2, size = 0.1)
  })
  
  
  # make interactive data
  working_data <- reactive({
    if (input$refresh_data < 1){
      test_data <- read.csv("toy_data.csv")
    } else {
      test_data <- read.csv("toy_data.csv")
    }
    data.frame(test_data)
  })
  
  
  # BTC Test Plot
  output$test_plot <- renderPlot({
    plot(working_data()$Date, working_data()$Close,
         type = "l")
  })
  
  
  ## for complexity / length plots ##
  
  
  
  output$plot_complexity_1 <- renderPlotly({
    
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre, ])
    ggplotly(
      ggplot(data= plotData , aes(x=dates,  y= Mean_Song_Length) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab("Words per song") + 
        ggtitle("Number of words per song - over time")+
        stat_smooth(method=lm, colour = "black") + 
        labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=16,face="bold")  ,
              axis.text=element_text(size= 15, face="bold"),
              plot.subtitle = element_text(size = 15, hjust = 0.5),
              legend.text = element_text(size=14), 
              legend.title = element_text(size=15 , face="bold",  hjust = 0.5))
    )
  })
  output$plot_complexity_2 <- renderPlotly({
    
    plotData <- data.frame(df_lengths_genres_dates_2[df_lengths_genres_dates$genre %in% input$Genre, ])
    ggplotly(
      ggplot(data= plotData , aes(x=dates,  y= Mean_Song_Length) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab("Words per song") + 
        ggtitle("Mean Number of words  - without stopwords or onomatopoetics")+
        stat_smooth(method=lm, colour = "black") + 
        labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=16,face="bold")  ,
              axis.text=element_text(size= 15, face="bold"),
              plot.subtitle = element_text(size = 15, hjust = 0.5),
              legend.text = element_text(size=14), 
              legend.title = element_text(size=15 , face="bold",  hjust = 0.5))
    )
  })
  
  
})



