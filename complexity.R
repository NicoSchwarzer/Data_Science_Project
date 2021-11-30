#####################################
## Length and complexity of lyrics ##
#####################################



## use in ggplot 
#scale_color_brewer(palette = "Blues") + 
#  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
#        axis.title=element_text(size=16,face="bold")  ,
#        axis.text=element_text(size= 15, face="bold"),
#        plot.subtitle = element_text(size = 15, hjust = 0.5),
#        legend.text = element_text(size=14), 
#        legend.title = element_text(size=15 , face="bold",  hjust = 0.5))
#)



############################
### Complexity of genres ###
############################


setwd("C:/Users/Nico/Documents/Uni/3. Sem/DS Projekt/Code_and_Data")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("pryr")) install.packages("pryr")
if (!require("plotly")) install.packages("plotly")
if (!require("tm")) install.packages("tm")

library(tidyverse)
library(tidytext)
library(pryr)
library(lubridate)
library(plotly)
library(tm)
library(shiny)
library(shinythemes)




## toy dataset here - to be changed when run on remote machine!
df_lyrics <- read_csv("base_data_cleaned.csv")

glimpse(df_lyrics)

########################################
## length of songs per genre and year ## 
########################################


# this would return mean length of genres overall   
#  df_lengths_genres <- df_lyrics %>%
#  mutate(len = sapply(strsplit(lyrics, " "), length)) %>%   ### function for length of words 
#  group_by(genre) %>%
#  summarise(mean_len = as.numeric(mean(len)))



#df_lyrics[is.na(df_lyrics$lyrics) == T, ]

# mean length per genre and date/year
df_lengths_genres_dates <- df_lyrics %>%
  filter(is.na(lyrics) == F) %>%
  mutate(len = sapply(strsplit(lyrics, " "), length)) %>%   
  filter(len <= 1000) %>%
  filter(len > 20) %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  group_by(genre, dates) %>%
  mutate(dates = as.Date(dates)) %>%
  summarise(Mean_Song_Length =  ceiling(mean(len))) %>%
  #  summarise(Song_Length_over_Duration =  ceiling(mean(len))) %>%
  filter(genre != "unknown genre")



#########################################
## without stop words and onomopoetics ## 
#########################################

## onomopoetics dervied from 
#common_words <- df_lyrics[nrow(df_lyrics)-1000:nrow(df_lyrics),] %>%
#     unnest_tokens(output = "word", input = lyrics, token = "words") %>%
#    count(word, sort = T) #%>%
#common_words$word[1:1000]  



onomopoetics <- c("ah","ooh","shimmy","uh","wah","oop","na","nah","bop","whoa","ya","mm","mmh","mmm","mmmh","o","oh","oo","ole","ola","hip","hipp","yo","jo","ee","eeh","da","dah","da-da","dada","ho","hoh","wo","woo", "uh", "du","da", "duh", "dah", "wow")

## stopwords from base R
sw <- stop_words$word


df_lengths_genres_dates_2  <- df_lyrics%>%
  filter(is.na(lyrics) == F)%>%
  filter(genre != "unknown genre") %>%
  mutate(len1 = sapply(strsplit(lyrics, " "), length)) %>%   
  filter(len1 <= 1000) %>%
  filter(len1 > 20)  %>%
  filter(validUTF8(lyrics) == T)

xx <- df_lengths_genres_dates_2[, c("lyrics", "combination")]
df_uniques <- distinct(xx)
rm(xx)

df_uniques <- df_uniques %>%
  mutate(ly2 =  removeWords(lyrics,sw)) %>%
  mutate(ly =  removeWords(ly2,onomopoetics))
  
for (i in 1:nrow(df_uniques)) {
 a <- strsplit(df_uniques$ly[i], " ")
 df_uniques$len[i] <-  length(a[[1]][a[[1]]!= ""])
}


df_lengths_genres_dates_2 <- left_join(df_lengths_genres_dates_2, df_uniques, by = "combination")

df_lengths_genres_dates_2 <- df_lengths_genres_dates_2 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  group_by(genre, dates) %>%
  mutate(dates = as.Date(dates)) %>%
  summarise(Mean_Song_Length =  ceiling(mean(len)))



## other list  correlation of len & duration !












##############################################################################












ui <- navbarPage(
  
  # Title of whole Application
  title = "From the Beatles to the Killers - A Chronology",
  
  # make pages fluid
  fluid = TRUE,
  
  # optimize navbar for smaller displays
  collapsible = TRUE,
  
  # set other theme
  theme = shinythemes::shinytheme("flatly"),
  
  # play with background colors
  #tags$head(tags$style(css)),
  

tabPanel("Length and complexity of lyrics",
         
         fluidPage(
           
           tags$h1("Length of Lyrics"),
           
           tags$h3("Development of Song Lyrics Length across Time"),
           tags$h4("When analyzing the length of songs lyrics across time, it becomes very much apparent that across genres the avergae number of words almost doubled from 1960 until today. This is quite striking! Once can select genres to see the development of lyrics belonging to that genre."),
           
           
           fluidRow(width=12,
                    column(width=12,
                           selectInput("Genre", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "dance"), selectize = TRUE, width = NULL, size = NULL),
                    )),
           plotlyOutput("plot_complexity_1") ,
           
           tags$h3("Just more 'oh-s' and 'ah-s' in today's songs ?"),
           tags$h4("Going further, one might analyze if the increase in lyrics length is powered by a more freuqnet use of onomatopoeic words ('Uh', 'Ah', 'La-La'). These have been handpicked and dropped from the lyrics. Also, as is common in NLP tasks, common stopwords of the english language have been removed as well. The english stopwords provided in BaseR were utilized for this.
                   While the overall number of words drops (as expected), the rising trend is still present "),
           
           plotlyOutput("plot_complexity_2") ,
           
         )
)
)





## )  end of tab panel


server <- function(input, output) {
  
  
  output$plot_complexity_1 <- renderPlotly({
    
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre, ])
    ggplotly(
      ggplot(data= plotData , aes(x=dates,  y= Mean_Song_Length) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab("Mean number of words per song") + 
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
        ylab("Mean number of words per song - wirhout stopwords and onomatopoetics") + 
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
  
  
}



shinyApp(ui, server)









### complexity -> # of different word stems 
## repetitions in sentences 
##  # of 'oh, ah'
