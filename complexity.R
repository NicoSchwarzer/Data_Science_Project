#########################################################
## Introdcutory page & Length and complexity of lyrics ##
#########################################################


## use in ggplot 
#scale_color_brewer(palette = "Blues") + 
#theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
#      axis.title=element_text(size=14,face="bold")  ,
#      axis.text=element_text(size= 13, face="bold"),
#      plot.subtitle = element_text(size = 13, hjust = 0.5),
#      legend.text = element_text(size=12), 
#      legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
#)


## use of following stop words and "Lautmalerien" possible
#onomopoetics <- c("ah","ooh","shimmy","uh","wah","oop","na","nah","bop","whoa","ya","mm","mmh","mmm","mmmh","o","oh","oo","ole","ola","hip","hipp","yo","jo","ee","eeh","da","dah","da-da","dada","ho","hoh","wo","woo", "uh", "du","da", "duh", "dah", "wow")

## stopwords from base R
#sw <- stop_words$word

## functions ## 


sentiment_mathing <- function(word) {
  
  out <- sentiments_afinn$value[sentiments_afinn$word == word]
  return(out)
}



############################
### Complexity of genres ###
############################


setwd("C:/Users/Nico/Documents/Uni/3. Sem/DS Projekt/Code_and_Data")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("pryr")) install.packages("pryr")
if (!require("plotly")) install.packages("plotly")
if (!require("tm")) install.packages("tm")
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("SnowballC")) install.packages("SnowballC")


library(tidyverse)
library(tidytext)
library(pryr)
library(lubridate)
library(plotly)
library(tm)
library(shiny)
library(shinythemes)
library(SnowballC)




## toy dataset here - to be changed when run on remote machine!
df_lyrics <- read_csv("base_data_cleaned.csv")


#glimpse(df_lyrics)



########################################
## length of songs per genre and year w/ & wo/ stopwrods and oomatopoetics ##
## Mean Tempo & Mean Duration // Unique words (before & after stemming ) ####
## Adverbs & unique adverbs 
########################################


# mean length per genre and date/year
df_lengths_genres_dates_1 <- df_lyrics %>%
  filter(is.na(lyrics) == F) %>%
  mutate(len = sapply(strsplit(lyrics, " "), length)) %>%   
  filter(len <= 1000) %>%
  filter(len > 20) %>%
  filter(validUTF8(lyrics) == T) %>%
  select("dates", "genre", "len", "tempo", "duration", "combination", "lyrics")



## onomopoetics dervied from 
#common_words <- df_lyrics[nrow(df_lyrics)-1000:nrow(df_lyrics),] %>%
#     unnest_tokens(output = "word", input = lyrics, token = "words") %>%
#    count(word, sort = T) #%>%
#common_words$word[1:1000]  


onomopoetics <- c("ah","ooh","shimmy","uh","wah","oop","na","nah","bop","whoa","ya","mm","mmh","mmm","mmmh","o","oh","oo","ole","ola","hip","hipp","yo","jo","ee","eeh","da","dah","da-da","dada","ho","hoh","wo","woo", "uh", "du","da", "duh", "dah", "wow")

## stopwords from base R
sw <- stop_words$word


## getting adverbs 
adverbs <- tidytext::nma_words$word[tidytext::nma_words$modifier == "adverb"]

## sentiment indices 
sentiments_afinn <- get_sentiments("afinn")



sentiments_afinn_negative <- sentiments_afinn[sentiments_afinn$value <0, ]
sentiments_afinn_positive <- sentiments_afinn[sentiments_afinn$value > 0, ]



## important now - > len refers to overall lyrics length while len1 refers to to reduced length!
xx <- df_lengths_genres_dates_1[, c("lyrics", "combination", "tempo", "duration", "genre", "len")]
df_uniques <- distinct(xx)
rm(xx)

# removing stopwords and onomatopoetics
df_uniques <- df_uniques %>%
  mutate(ly2 =  removeWords(lyrics,sw)) %>%
  mutate(ly =  removeWords(ly2,onomopoetics)) 



for (i in 1:nrow(df_uniques)) {
  
 a <- strsplit(df_uniques$ly[i], " ")
 b <- a[[1]][a[[1]] %in% sentiments_afinn$word] # for sentiment matching 
 # positive words (length and most common ones)
 pos <- b[b %in% sentiments_afinn_positive$word]
 pos_list <- sort(pos[is.na(pos) == F], decreasing=T)[1:3]
 df_uniques$pos_1[i] <- pos_list[1]
 df_uniques$pos_2[i] <- pos_list[2]
 df_uniques$pos_3[i] <- pos_list[3]
 df_uniques$pos_len[i] <- length(pos)
 # positive words (length and most common ones)
 neg <- b[b %in% sentiments_afinn_negative$word]
 neg_list <- sort(neg[is.na(neg) == F], decreasing=T)[1:3]
 df_uniques$neg_1[i] <- neg_list[1]
 df_uniques$neg_2[i] <- neg_list[2]
 df_uniques$neg_3[i] <- neg_list[3]
 df_uniques$neg_len[i] <- length(neg)
  df_uniques$len1[i] <-  length(a[[1]][a[[1]]!= ""]) # reduced length
 df_uniques$sent_score[i] <- mean(sapply(b, FUN = sentiment_mathing)) # mean sentiment score 
 ## unique words 
 df_uniques$compl[i] <- length(unique(strsplit(df_uniques$lyrics[i], " ")[[1]]))
 df_uniques$compl_stem[i] <- length(unique(wordStem(strsplit(df_uniques$lyrics[i], " ")[[1]], "english")))
 ## adverbs 
 df_uniques$adverbs[i] <- sum(strsplit(df_uniques$lyrics[i], " ")[[1]] %in% adverbs)
 
}

# positive to negative ratio
df_uniques <- df_uniques %>%
  mutate(pos_neg_ratio  =  (pos_len / (pos_len + neg_len))  )



rm(pos)
rm(neg)
rm(pos_list)
rm(neg_list)


df_uniques <- df_uniques[, c("combination", "len", "len1", "duration", "tempo", "genre", "compl", "compl_stem", "adverbs", "sent_score", "pos_1", "pos_2", "pos_3", "pos_len", "neg_1", "neg_2", "neg_3", "neg_len", "pos_neg_ratio")]
df_lengths_genres_dates_2_1 <- left_join(df_lengths_genres_dates_1[,  c("combination", "dates", "genre")], df_uniques[, c("combination", "len", "len1", "compl", "compl_stem", "duration", "tempo", "adverbs", "sent_score", "pos_1", "pos_2", "pos_3", "pos_len", "neg_1", "neg_2", "neg_3", "neg_len", "pos_neg_ratio")], by = "combination")


df_lengths_genres_dates  <- df_lengths_genres_dates_2_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  mutate(compl = compl / len) %>% # depending on song length 
  mutate(compl_stem = compl_stem / len) %>% # depending on song length 
  mutate(adverbs_rel = adverbs / len) %>%
  summarise(Mean_Song_Length =  ceiling(mean(len)), Mean_Red_Song_Length =  ceiling(mean(len1)), Mean_Tempo = mean(tempo, na.rm = T), Mean_Duration = mean(duration, na.rm = T),   Mean_Compl = mean(compl, na.rm = T),  Mean_Compl_Stem = mean(compl_stem, na.rm = T), Num_Adverbs = mean(adverbs, na.rm = T), Rel_Adverbs = mean(adverbs_rel, na.rm = T), Scores = mean(sent_score, na.rm = T), Mean_Pos_Neg = mean(pos_neg_ratio, na.rm = T) ) %>%
  filter(genre != "unknown genre")
  df_lengths_genres_dates$Scores[is.nan(df_lengths_genres_dates$Scores) == T] <- 0



length_duration <-   df_lengths_genres_dates_2_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(Date) %>%
  summarise(Mean_Tempo = mean(tempo, na.rm = T), Mean_Duration = mean(duration, na.rm = T) ) 



########################################
## Development of length and duration ##
########################################

## correlations ## 

corr_table1 <- df_uniques %>%
  filter(genre!= "unknown genre") %>%
  mutate(Rel_Adverbs = (adverbs / len)*100) %>%
  group_by(genre) %>%
  summarise(Corr1 = cor(tempo, len, use = "complete.obs"),  Corr2 = cor(duration, len, use = "complete.obs"), Rel_Adverbs = mean(Rel_Adverbs, na.rm = T), Mean_Pos_Neg = mean(pos_neg_ratio, na.rm = T) )

names(corr_table1) <- c("Genre", "Correlation Lyrics Length with BPM", "Correlation Lyrics Length with Duration", "Percentage of adverbs", "Ratio of positive to negative words")



#################
## Overview DF ##
#################

df_over <- data.frame(  as.character(min(df_lyrics$dates)),  as.character(max(df_lyrics$dates)),  as.character(nrow(df_lyrics)) , as.character(nrow(  df_lyrics[is.na(df_lyrics$lyrics) == F,] )), as.character(nrow(df_uniques)) )
names(df_over) <- c("First date", "Most recent date", "Number of songs scraped", "Songs with available lyrics", "Unique available songs")



##### relevant DFs to be deleted or sent to shiny app 

## deleted
rm(df_lyrics)
rm(df_lengths_genres_dates_1)
rm(df_lengths_genres_dates_2_1)

## sent to APP

#df_over
#df_lengths_genres_dates
#length_duration
#df_uniques
#df_pos_words







###############
## Shiny App ##
###############


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
  
  ## Page 1  - Introductory page 
tabPanel("Introductory Page",
         
         fluidPage(
           
        
           tags$h1("Overview of the Lyrics Analytics Project"),
           
           fluidRow(
             tags$h4("This R-Shiny Web-Application displays the result of a University Project of the M.Sc. Program Data Science in Business and Economics and Tübingen University. It bundles all relevant steps of a Data Science Project - the last of which is an interactive display of the results residing here."),
                ),
             fluidRow(
               tags$h4("The goal of the Project is to gather insights as to how  Song Lyrics have evolved over time using NLP tools. To this aim, we have scraped all hot 100 Billboard charts from 1960 until today and used the Genuis API to retrieve the lyrics. Also, we have gathered genre information as well as information on each song's acoustic features through the Spotify API. Going further, we have trained and implemented a binary classifier to detect of the songs lyrics are in english and made use of the DeepL API if need be for translation."),
                 ),
           fluidRow(
             tags$h4("Obviously, each week, a new hot 100 Chart List is released. To cope with this, we have also implemented a mechanism that performs all above mentioned steps once a week and appends our overall database. As a next step, the lyrics are cleaned and and manipulated for the further analytics tasks. As we gather more lytics each week, all post-processign steps are also re-run on a weekly basis."),
               ),
           fluidRow(
             tags$h3("Overview of available data")),
           fluidRow(
             tags$h4("Here, relevant parameters descriving the scope of the available lyrics data are summarized.")),


           fluidRow( column(12, align="center",  tableOutput("overview_table") ) ), 

)),

tabPanel("Length and Complexity of Lyrics",
         
         fluidPage(
           
           ### Song Length ### 
           
           tags$h1("Length of Lyrics"),
           
           fluidRow(
           tags$h3("Development of Song Lyrics Length across Time")),
           fluidRow(
             tags$h4("When analyzing the length of songs lyrics across time, it becomes very much apparent that the average number of words (quite strikingly) almost doubled from 1960 until today. While this trend is observeable across genres (with the exception of rock-n-roll), one also sees how songs of the latter genre and rock songs contain comparatively few words, while songs belonging to the genres  dance, jazz, reaggae or rnb contain considerably more. Also, for many genres, peaks and drops in the word count are observeable - showing that for some time periods, the charts songs of some genre might be substantially longer or shorter than common for that genre."),
           ),
           fluidRow(
             tags$h4("Going further, one might analyze if the increase in lyrics length is powered by a more freuqnet use of onomatopoeic words ('Uh', 'Ah', 'La-La'). These have been handpicked and dropped from the lyrics. Also, as is common in NLP tasks, common stopwords of the english language have been removed as well. The english stopwords provided in BaseR were utilized for this.
                   While the overall number of words drops (as expected), the rising trend is still present. Also, the per-genre development of lyrics length differs only slightly from the case of no word removals for most genres. An exception is constituted by reggae music with peaks around 1990 and 2005. This reveals that reggae music uses comparatively fewer stopwords and onomatopoetics than other genres. Both results are displayed in the following on a per-genre basis."),
           ),
           
           fluidRow(width=12,
                    column(width=12,
                           selectInput("Genre_compl_1", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "dance"), selectize = TRUE, width = NULL, size = NULL),
                    )),
           fluidRow(width = 12,
                    column(width = 6,
           plotlyOutput("plot_complexity_1")) ,
           
           column(width = 6 , plotlyOutput("plot_complexity_2") ) ),

           
           fluidRow(
             tags$h3("Accounting for changes in tempo and duration")),
           
           fluidRow(
             tags$h4("One might guess that with increasing duration and with an increased number of beats per minute, the number of words rises as well. These relationships are laid out below. The positive correlation of duration and lyrics length seems quite strong while the correlation of BPM and lyrics length is rather feeble - and for some genres even around 0. Hover over the scatter plots to see which songs belong to the points.")
           ),
          fluidRow(
            width = 12,
            column(width = 4,
                   plotlyOutput("relationship_tempo")) ,
            column(width = 4,
                   plotlyOutput("relationship_duration")) ,
            column(width = 4,
                   tableOutput("correlations_table_compl1")) ,
          ),
           fluidRow(
             tags$h4("Given these insights, one might analyze how the BPM and the duration of songs evolved from 1960 to see if this might have caused the increase in words per song. The following plots, however, indicate that neither the average BPM, nore the average song duration rose overall. Still, it's noteworthy how the average duration rises until around 1992 (black dashed line) and falls off afterwards. On the other hand, the mean BPM shot up during that time period and thereby possibly mitigating the effect of shorter songs.")
           ),
           fluidRow(width = 12,
                    column(width = 6,
                           plotOutput("plot_bpm")) ,
                    
                    column(width = 6 , plotOutput("plot_duration") ) ),
           
           
           
           ### Song complexity ###
           
           fluidRow(
             tags$h1("Complexity of lyrics")),
           fluidRow(
             tags$h3("Repetitiveness in lyrics"),
           ),
          fluidRow(
            tags$h4("When considering the complexity of lyrics, a main point of analysis is the repetitiveness of words utilized. Obviously, the fewer unique words used, the less complex a text can be deemed. Also, the variety of words used plays a substantial role. To analyse that, one can map all unique words to their word stems (using R's Snowball library). A complex text should also contain words from a high number ofdifferent word stems. The plots below show the number of unique words and unique word stems relative to the lyrics length and demonstrate that while overall length of song lyrics increased, both the frequency of uniqe words and unique words stems dropped."),
          ),
          fluidRow(width=12,
                   column(width=12,
                          selectInput("Genre_compl_2", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop"), selectize = TRUE, width = NULL, size = NULL),
                   )),
          
          fluidRow(width = 12,
                   column(width = 6,
                          plotlyOutput("plot_complex_un_1")) ,
                   
                   column(width = 6 , plotlyOutput("plot_complex_un_2") ) ),
          fluidRow(
            tags$h3("Tell me what you want? Tell me what you REALLY, REALLY want!"),
          ),
          fluidRow(
            tags$h4("Really wanting something is semantically more complex than only wanting something - because of the use of adverbs! The latter are common measurements for the elaborateness of a language. Hence, we also took a look at how many of the words in each song's lyrics are adverbs. As is exhibited below, the percentage of adverbs of all words lies around 1% with dance, jazz, pop and raeggae music containing slightly more. Also, there seems to be no overall increase or decrease over the last 60 years, but a high fluctuation of the ratio of adverbs in the song lyrics from one year to another."),
          ),
          fluidRow(width = 12,
                   column(width = 4,
                          tableOutput("correlations_table_compl2")) ,
                   
                   column(width = 8 , plotlyOutput("adverb_slider") ) ),
          
          
         )
),


tabPanel("Topic Modelling"),

tabPanel("Sentiment Analysis",

        ### Sentiment Analysis Panel ### 
        
        fluidPage(
          
          ### Song Length ### 
          
          fluidRow(
            tags$h3("Development of General Sentiment across Time")),
          fluidRow(
            tags$h4("Texts, including song lyrics, carry a sentiment through their semantic structure. The latter may thus be analysed across the time and genre dimension for HOT 100 Charts of the past 60 years. In a first step, the average sentiment score per word was computed using the tidytext package and the afinn lexicon of pre-assigned sentiments. The latter assigns a score ranging from -5 (really negative) to +5 (really positive) to each word. As can be seen below, the average sentiment decreased over the last 60 years from an average sentiment score of around one to an average score below 0 - implying that while in the 60's, the mood in the Charts was generally positive while it is somewhat negative in today's charts. Also, it is observeable that jazz lyrics were rather pessimistic until 1990 while after that year, both rnb and dance lyrics contained more negatively than positively connotated words. On the contrary, from the start of the new millenial onwards, jazz, latin and reaggae music lyrics stand out for their optimistic wording.")),
          fluidRow(width=12,
                   column(width=12,
                          selectInput("Genre_sent_1", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "folk"), selectize = TRUE, width = NULL, size = NULL),
                   )),
          
          fluidRow(width = 12,
                     column(width = 12,
                            plotlyOutput("plot_sentiment_1")) ),
          
          fluidRow(
            tags$h3("Ratio of positive to negative expressions in Song Lyrics") ),
          fluidRow(
            tags$h4("Apart from the mean sentiment score of the lyrics which also entails a degree of the positive or respectively negative connotation, one can determine to average ratio of positive to negative words across the genre and time dimension. The latter differs somewhat from one genre to another with e.g. rock-and-roll, reggae and latin song lyrics having a higher ratio than others (see the table below). These findings also conincide with those of the mean sentiment score from above. Still, all genres contain on average more more positive than negative words. This ratio does, however, fluctuate greatly per year and also decreases per time for most genres as is displayed in the slider graph below.")
          ),
          fluidRow(width = 12,
                   column(width = 4,
                          tableOutput("table_pos_neg_score")),
                   column(width = 8,
                          plotlyOutput("plot_pos_neg_score")),
            ),
          fluidRow(
            tags$h3("Most common positive and negative Words in Lyrics") ),
          fluidRow(
            tags$h4("Going further, the question remains what those words driving a positive or a negative sentiment score actually are. To shed some light into this issue, one can choose a Genre / Year Combination to see which words were the top five most common positively and negatively connoteted words for this subset of the data. ")
          ),
          fluidRow(width = 12,
            column( width = 6,
                    selectInput("word_pos_genre", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "folk"), selectize = TRUE, width = NULL, size = NULL),
                    
                               )  ,
                      column( width = 6,
                    
                    
          dateRangeInput("date_range_word", "Choose Date Range:",
                              format = "yyyy",
                              min = min(df_pos_words$Date),
                              max   = max(df_pos_words$Date),
                              start = "2010-01-01",
                              end = "2020-01-01",
                              startview = "year" ) ) ),
          fluidRow(width = 12,
                   column(width = 6,
                          plotOutput("plot_pos_words")),
                   column(width = 6,
                          plotOutput("plot_neg_words")) ),
         ) ) ,
tabPanel("Pronoun Analysis"),
tabPanel("Recommendation")

) ## end of ui



server <- function(input, output) {

  #######################
  ## Introductory Page ## 
  #######################
  
  output$overview_table <- renderTable({df_over})
  

  

  ###########################
  ## Length and complexity ##
  ###########################  
  
  
  output$plot_complexity_1 <- renderPlotly({
    
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre_compl_1, ])
    ggplotly(
      ggplot(data= plotData , aes(x=Date,  y= Mean_Song_Length) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab("Words per song") + 
        ggtitle("Mean Number of words per song")+
        stat_smooth(method=lm, colour = "black") + 
        labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
      )
  })
  output$plot_complexity_2 <- renderPlotly({
    
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre_compl_1, ])
    ggplotly(
      ggplot(data= plotData , aes(x=Date,  y = Mean_Red_Song_Length) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab("Words per song") + 
        ggtitle("No stopwords or onomatopoetics")+
        stat_smooth(method=lm, colour = "black") + 
        labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
    )
  })
  
  output$relationship_tempo <- renderPlotly({
    ggplotly(
    ggplot(df_uniques[df_uniques$tempo > 10,], aes(x = tempo, y = len, text = combination)) + 
      geom_point(color = "steel blue") + 
      xlab("Beats per Minute") + 
      ylab("Words per Song") + 
      ggtitle("BPM vs. Lyrics Length")+
      ylim(0, 800) +
      xlim(0, 220) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5)),
    tooltip = "combination"
  )
  })
  
  
  output$relationship_duration <- renderPlotly({
    ggplotly(
  ggplot(df_uniques[df_uniques$duration > 30,], aes(x = duration, y = len, text = combination)) + 
    geom_point(color = "steel blue") + 
    xlab("Duration (in seconds)") + 
    ylab("Words per Song") + 
    ggtitle("Duration vs. Lyrics Length")+
    ylim(0, 800) +
    xlim(0, 600) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 13, hjust = 0.5),
          legend.text = element_text(size=12), 
          legend.title = element_text(size=13 , face="bold",  hjust = 0.5)),
  tooltip = "combination"
    )
  })
  
  
  
  output$correlations_table_compl1 <- renderTable({corr_table1[,c("Genre", "Correlation Lyrics Length with BPM", "Correlation Lyrics Length with Duration")]})
  
  
  output$plot_bpm <- renderPlot({
    ggplot(length_duration, aes(x = Date, y = Mean_Tempo)) + 
      geom_line(color = "steel blue") + 
      xlab("Time") + 
      ylab("Mean BPM") + 
      ggtitle("Mean Beats per Minute")+
      ylim(115, 128) + 
      geom_vline(xintercept= as.Date("1992-01-01"), linetype="dashed", color = "black") + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
  })
  
  output$plot_duration <- renderPlot({
    ggplot(length_duration, aes(x = Date, y = Mean_Duration)) + 
      geom_line(color = "steel blue") + 
      xlab("Time") + 
      ylab("Mean Duration (in seconds)") + 
      ggtitle("Mean Duration of songs")+
      ylim(100, 280) + 
      geom_vline(xintercept= as.Date("1992-01-01"), linetype="dashed", color = "black") + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
  })
  
  output$plot_complex_un_1 <- renderPlotly({
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre_compl_2, ])
    ggplotly(
      ggplot(data= plotData , aes(x=Date,  y= Mean_Compl ) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab(" # Unique words / Song Length") + 
        ggtitle(" Unique words / length")+
        stat_smooth(method=lm, colour = "black") + 
      #  labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              #plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
    )
  })
  
  output$plot_complex_un_2 <- renderPlotly({
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre_compl_2, ])
    ggplotly(
      ggplot(data= plotData , aes(x=Date,  y= Mean_Compl_Stem ) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab(" # Unique word stems / Song Length") + 
        ggtitle(" Unique word stems / length")+
        stat_smooth(method=lm, colour = "black") + 
       # labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              #plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
    )
  })
  
  output$correlations_table_compl2 <- renderTable({corr_table1[,c("Genre", "Percentage of adverbs")]})
  
  output$adverb_slider <- renderPlotly({
  df_lengths_genres_dates %>%
    select("Date", "genre", "Rel_Adverbs") %>%
    mutate(Rel_Adverbs = Rel_Adverbs * 100) %>%
    group_by(Date, as.factor(genre)) %>%
    mutate(Date = as.factor(substr(Date, 1, 4))) %>%
    plot_ly(x = ~ as.factor(genre),
            y = ~ Rel_Adverbs,
            frame = ~ Date, ids = ~ genre) %>%
    add_bars() %>% 
    layout(showlegend = F, xaxis = list(title = 'Genre'), yaxis = list(title = 'Percantage of adverbs'),  font = (t <- list(
      size = 14,
      color = 'black')))
  })
  
  ########################  
  ## Sentiment Analysis ##
  ########################
  
  output$plot_sentiment_1 <- renderPlotly({
  plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre_sent_1, ])
  ggplotly(
    ggplot(data= plotData , aes(x=Date,  y= Scores) ) +
      geom_line( aes(colour = genre)) + 
      xlab("Time") + 
      ylab("Mean Sentiment score") + 
      ggtitle("Mean Sentiment Score per song")+
      stat_smooth(method=lm, colour = "black") + 
      labs(  subtitle="Overall trend in black") + 
      scale_color_brewer(palette = "Blues") + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
  )
  })
  
  output$table_pos_neg_score <- renderTable({
    corr_table1[,c("Genre", "Ratio of positive to negative words")]
  })
  
  
  output$plot_pos_neg_score <- renderPlotly({
  df_lengths_genres_dates %>%
    select("Date", "genre", "Mean_Pos_Neg") %>%
    group_by(Date, as.factor(genre)) %>%
    mutate(Date = as.factor(substr(Date, 1, 4))) %>%
    plot_ly(x = ~ as.factor(genre),
            y = ~ Mean_Pos_Neg,
            frame = ~ Date, ids = ~ genre) %>%
    add_bars() %>% 
    layout(showlegend = F, xaxis = list(title = 'Genre'), yaxis = list(title = 'Ratio of positive to negative words'),  font = (t <- list(
      size = 14,
      color = 'black')))
  })
  
    
  output$plot_pos_words <- renderPlot({
  df_pos_words <- df_pos_words %>%
    count(pos_1, sort = T)
  
  
  df_pos_words <- df_pos_words[( df_pos_words$Date %in% input$date_range_word) & ( df_pos_words$Date %in% input$date_range_word) & ( is.na(df_pos_words$pos_1) == F),]
  
  #( df_pos_words$Date %in% input$date_range_word)
  if (nrow(df_pos_words) > 5) {
    df_pos_words <- df_pos_words[1:5,]
  }

  ggplot(data=df_pos_words, aes(x=pos_1, y=n)) +
    geom_bar(stat="identity", fill = "steel blue") +
    xlab("Word") + 
    ylab("Frequency") +
    ggtitle("Frequency of most being most positive word")+
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 13, hjust = 0.5),
          legend.text = element_text(size=12), 
          legend.title = element_text(size=13 , face="bold",  hjust = 0.5)) + 
    coord_flip()
  })
  
  output$plot_neg_words <- renderPlot({
    df_pos_words <- df_pos_words %>%
      count(neg_1, sort = T)
    
    
    df_pos_words <- df_pos_words[( df_pos_words$Date %in% input$date_range_word) & ( df_pos_words$Date %in% input$date_range_word) & ( is.na(df_pos_words$neg_1) == F),]
    
    #( df_pos_words$Date %in% input$date_range_word)
    if (nrow(df_pos_words) > 5) {
      df_pos_words <- df_pos_words[1:5,]
    }
    
    ggplot(data=df_pos_words, aes(x=neg_1, y=n)) +
      geom_bar(stat="identity", fill = "steel blue") +
      xlab("Word") + 
      ylab("Frequency") +
      ggtitle("Frequency of most being most negative word")+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5)) + 
      coord_flip()
  })
  
  
    
} ## end of server function 


shinyApp(ui, server)


