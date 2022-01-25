

jscode <- "shinyjs.refresh = function() { history.go(0); }"


sentiment_mathing <- function(word) {
  
  out <- sentiments_afinn$value[sentiments_afinn$word == word]
  return(out)
}


library(tidyverse)
library(tidytext)
library(pryr)
library(lubridate)
library(plotly)
library(tm)
library(shiny)
library(shinythemes)
library(SnowballC)
library(textdata)
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(shinyjs)

df_over <- data.table::fread("df_over.csv")
df_lengths_genres_dates <- data.table::fread("df_lengths_genres_dates.csv")
length_duration <- data.table::fread("length_duration.csv")
df_uniques <- data.table::fread("df_uniques.csv")
df_pos_words <- data.table::fread("df_pos_words.csv")
corr_table1 <- data.table::fread("corr_table1.csv")
df_seasonal_sent_month <- data.table::fread("df_seasonal_sent_month.csv")
df_seasonal_sent_season <- data.table::fread("df_seasonal_sent_season.csv")
we_data <- data.table::fread("we_data.csv")
i_data <- data.table::fread("i_data.csv")
she_data <- data.table::fread("she_data.csv")
he_data <- data.table::fread("he_data.csv")
df_sents_year <- data.table::fread("df_sents_year.csv")
df_sents_genre_year <- data.table::fread("df_sents_genre_year.csv")
df_historical <- data.table::fread("df_historical.csv")
df_topics_genre_year <- data.table::fread("df_topics_genre_year.csv")
df_topics_year <- data.table::fread("df_topics_year.csv")
prep_genre <- data.table::fread("prep_genre.csv")
co_occuring_df <- data.table::fread("co_occuring_df.csv")
topics_sent <- data.table::fread("topics_sent.csv")



server <- function(input, output) {
  
  #######################
  ## Introductory Page ## 
  #######################

  df_over$`First date` <- as.character(df_over$`First date`)
  df_over$`Most recent date` <- as.character(df_over$`Most recent date`)
  df_over$`Number of songs scraped` <- as.integer(df_over$`Number of songs scraped`)
  df_over$`Songs with available lyrics` <- as.integer(df_over$`Songs with available lyrics`)
  df_over$`Unique available songs` <- as.integer(df_over$`Unique available songs`)
  similarity_topics_df <- similarity_topics_df[(similarity_topics_df$genre != "unknown genre"),]
  similarity_topics_df$'First Chart Appearance' <-     as.Date(similarity_topics_df$first_appearance)
  
  output$overview_table <- renderTable({
    df_over
     })
  
  
  ###########################
  ## Length and complexity ##
  ###########################  
  
  observeEvent(input$button_length_1, {
    toggle('text_div')
    output$text_length_1 <- renderText({"At closer inspection, one also sees how songs of the genre rock-and-roll and rock songs contain comparatively few words, while songs belonging to the genres  dance, jazz, reaggae or rnb contain considerably more. Also, for many genres, peaks and drops in the word count are observeable - showing that for some time periods, the charts songs of some genre might be substantially longer or shorter than common for that genre. This might, however, also be attributed to the fact that in some years, only few songs of one genre were present in the Hot 100. Should these few songs be extraordinarily log or short, a peak or drop occurs.                Also when removing stopwords and onomatopoetics, while the overall number of words drops (as expected), the rising trend is still present. The per-genre development of lyrics length differs only slightly from the case of no word removals for most genres. An exception is constituted by reggae music with peaks around 1990 and 2005. This reveals that reggae music uses comparatively fewer stopwords and onomatopoetics than other genres.                  Concerning implementation, the english stopwords provided in BaseR as well as a extensive handpicked lists of onomatopoetics retrieved from our song database were utilized for this."
    })
  })
  

  

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
        geom_point(size = 0.1, color = "steel blue") + 
        xlab("Beats per Minute") + 
        ylab("Words per Song") + 
        ggtitle("BPM vs. Lyrics Length")+
        ylim(0, 800) +
        xlim(0, 220) +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
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
        geom_point(size = 0.1, color = "steel blue") + 
        xlab("Duration (in seconds)") + 
        ylab("Words per Song") + 
        ggtitle("Duration vs. Lyrics Length")+
        ylim(0, 800) +
        xlim(0, 600) +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5)),
      tooltip = "combination"
    )
  })
  
  
  
  output$correlations_table_compl1 <- renderTable({corr_table1[,c("Genre", "Correlation Lyrics Length & BPM", "Correlation Lyrics Length & Duration")]})
  
  
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
        ylab(" # Unique words / Length") + 
        ggtitle(" Unique words / Length")+
        stat_smooth(method=lm, colour = "black") + 
        #  labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
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
        ylab(" # Unique word stems / Length") + 
        ggtitle(" Unique word stems / Length")+
        stat_smooth(method=lm, colour = "black") + 
        # labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              #plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
    )
  })
  
  output$correlations_table_compl2 <- renderTable({corr_table1[,c("Genre", "Percentage of adverbs")]})
  
  output$adverb_slider <- renderPlotly({
    plotData <- data.frame(df_lengths_genres_dates[df_lengths_genres_dates$genre %in% input$Genre_compl_3, ])
    ggplotly(
      ggplot(data= plotData , aes(x=Date,  y= Rel_Adverbs) ) +
        geom_line( aes(colour = genre)) + 
        xlab("Time") + 
        ylab(" Percentage of Adverbs") + 
        ggtitle(" Relative frequency of adverbs")+
        stat_smooth(method=lm, colour = "black") + 
        # labs(  subtitle="Overall trend in black") + 
        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              #plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
    )
  })
  
    
    
  
  #output$adverb_slider <- renderPlotly({
  #  df_lengths_genres_dates %>%
  #    select("Date", "genre", "Rel_Adverbs") %>%
  #    mutate(Rel_Adverbs = Rel_Adverbs * 100) %>%
  #    group_by(Date, as.factor(genre)) %>%
  #    mutate(Date = as.factor(substr(Date, 1, 4))) %>%
  #    plot_ly(x = ~ as.factor(genre),
  #            y = ~ Rel_Adverbs,
  #            frame = ~ Date, ids = ~ genre) %>%
  #    add_bars() %>% 
  #    layout(showlegend = F, xaxis = list(title = 'Genre'), yaxis = list(title = 'Percantage of adverbs'),  font = (t <- list(
  #      size = 14,
  #      color = 'black')))
  #})
  
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
#        scale_color_brewer(palette = "Blues") + 
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
    )
  })
  
  
  observeEvent(input$button_sentiment_1, {
    toggle('text_div_1')
    output$text_sent_1 <- renderText({"We have used a so-called valence shift algorithm though R’s sentimentR package. This algorithm is based on identifying positively and negatively connotated words alongside a magnitude of connotation based on the Syuzhet Sentiment dataframe. Next, for each of these words, the n preceding and proceding words are taken into account. These are tagged as neutral, negators, amplifiers, de-amplifiers or adversarial conjuctions. Based on these, the sentiment score of each polarized word is repeatedly mutlitplied by values which can be set. E.g. negators such as  ‚not‘ cause a multiplication of the sentiment score by a negative number, amplifiers such as ‚very‘ might cause a multiplication of the sentiment score by a positive number larger than one and de-amplifiers such as ‚a bit‘ might cause a multiplication of the sentiment score by a positive number below one.  All hyperparameters of this algorithm were optimized based on a labelled dataset of Lyrics and sentiments resulting in a F1 Score of 0.91."
      })
    output$text_sent_2 <- renderText({"         "})    
    output$text_sent_3 <- renderText({" Consider the following sources:"})
    output$text_sent_4 <- renderText({"Rinker, Tyler (2021), SentimentR, https://github.com/trinker/sentimentr"})
    output$text_sent_5 <- renderText({"Çano, Erion; Morisio, Maurizio (2017). MoodyLyrics: A Sentiment Annotated Lyrics Dataset. In: 2017 International Conference on Intelligent Systems, Metaheuristics & Swarm Intelligence, Hong Kong, March, 2017. pp. 118-124"})
    output$text_sent_6 <- renderText({"Lockers, M. L. (2017), Syuzhet: Extract sentiment and plot arcs from text. Retrieved from https://github.com/mjockers/syuzhet"})
    output$text_sent_7 <- renderText({"Hu, M., & Liu, B. (2004,. Mining opinion features in customer reviews. National Conference on Artificial Intelligence."})
    })

  
  output$plot_sentiment_2 <- renderPlot({
    
    ggplot(data= df_sents_year , aes(x=Date,  y= Scores) ) +
      geom_line(size = 0.8, color = "steel blue") + 
      ggtitle("Sentiment and historical events") + 
      xlab("Time") + 
      ylab("Mean Sentiment score") + 
      ylim(-0.15, 0.7) + 
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=16,face="bold")  ,
            axis.text=element_text(size= 16, face="bold"),
            plot.subtitle = element_text(size = 16, hjust = 0.5)) + 

      # May 1975 - End of Vietnam War
      geom_vline(xintercept =   df_sents_year$Date[186], color = "black", linetype="dotted", size = 1.2)  + 
      geom_text(aes(x=df_sents_year$Date[186], label="1", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +
      
      
      # January 1981 - January 1993 - US Economic Crisis
      geom_vline(xintercept =   df_sents_year$Date[253], color = "black" , linetype="dotted", size = 1.2) + 
      geom_vline(xintercept =   df_sents_year$Date[277], color = "black", linetype="dotted", size = 1.2) + 
      ggplot2::annotate("rect", xmin = df_sents_year$Date[253], xmax = df_sents_year$Date[277], ymin = -Inf, ymax = Inf,
                        alpha = .4) + 
      geom_text(aes(x=df_sents_year$Date[253], label="2", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) + 
      
      
      ## July 1988 - December 1991 - Dissolution of the Soviet Union 
      geom_vline(xintercept =   df_sents_year$Date[356], color = "black", linetype="dotted", size = 1.2) + 
      geom_vline(xintercept =   df_sents_year$Date[385], color = "black", linetype="dotted", size = 1.2) + 
      ggplot2::annotate("rect", xmin = df_sents_year$Date[356], xmax = df_sents_year$Date[385], ymin = -Inf, ymax = Inf,
                        alpha = .4) + 
      geom_text(aes(x=df_sents_year$Date[356], label="3", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) + 
      
      
      # September 2001 - 9/11  
      geom_vline(xintercept =   df_sents_year$Date[501], color = "black", linetype="dotted", size = 1.2) + 
      geom_text(aes(x=df_sents_year$Date[501], label="4", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) + 
      
      
      
      ## March 2003 - Start of second Iraq War
      geom_vline(xintercept =   df_sents_year$Date[519], color = "black", linetype="dotted", size = 1.2)  + 
      geom_text(aes(x=df_sents_year$Date[519], label="5", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) + 
      
      
      ## August 2007 - March 2009 US financial crisis
      geom_vline(xintercept =   df_sents_year$Date[572], color = "black", linetype="dotted", size = 1.2)  + 
      geom_vline(xintercept =   df_sents_year$Date[592], color = "black", linetype="dotted", size = 1.2)  +
      ggplot2::annotate("rect", xmin = df_sents_year$Date[572], xmax = df_sents_year$Date[592], ymin = -Inf, ymax = Inf,
                        alpha = .4) + 
      geom_text(aes(x=df_sents_year$Date[572], label="6", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) + 
      
      
      # March 2020 - First Wave of Covid Pandemic commences
      geom_vline(xintercept =   df_sents_year$Date[723], color = "black", linetype="dotted", size = 1.2) +
      geom_text(aes(x=df_sents_year$Date[723], label="7", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) #+ 
      
      # September 2021 - Second Wave of Covid Pandemic commences
    #  geom_vline(xintercept =   df_sents_year$Date[739], color = "black", linetype="dotted", size = 1.2) + 
    #  geom_text(aes(x=df_sents_year$Date[739], label="8", y=-0.1), colour="steel blue", angle=0, vjust = 1, text=element_text(size=54) , size = 8)
    
  })
  
  
  output$table_hist <- renderTable({df_historical[,"Events"] })
  
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
    
    
    df_pos_words <- df_pos_words[( df_pos_words$Date %in% input$date_range_word) & ( df_pos_words$Date %in% input$date_range_word) & ( is.na(df_pos_words$pos_1) == F),]
    
    df_pos_words <- df_pos_words %>%
      count(pos_1, sort = T)
    
    #( df_pos_words$Date %in% input$date_range_word)
    if (nrow(df_pos_words) > 5) {
      df_pos_words <- df_pos_words[1:5,]
    }
    
    ggplot(data=df_pos_words, aes(x=  reorder(pos_1, n), n)) +
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
    
    
    df_pos_words <- df_pos_words[( df_pos_words$Date %in% input$date_range_word) & ( df_pos_words$Date %in% input$date_range_word) & ( is.na(df_pos_words$neg_1) == F),]
    
    df_pos_words <- df_pos_words %>%
      count(neg_1, sort = T)
    
    
    #( df_pos_words$Date %in% input$date_range_word)
    if (nrow(df_pos_words) > 5) {
      df_pos_words <- df_pos_words[1:5,]
    }
    
    ggplot(data=df_pos_words, aes(x=  reorder(neg_1, n), n)) +
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
  
  output$corr_sent_tempo   <- renderPlotly({
    ggplotly(
      ggplot(df_uniques[df_uniques$tempo > 10,], aes(x = tempo, y = sent, text = combination)) + 
        geom_point(size = 0.1, color = "steel blue") + 
        xlab("Beats per Minute") + 
        ylab("Mean Sentiment Score") + 
        ggtitle("BPM vs. Sentiment")+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5)),
      tooltip = "combination"
    )
  })
  
  output$corr_sent_dance   <- renderPlotly({
    ggplotly(
      ggplot(df_uniques, aes(x = danceability, y = sent, text = combination)) + 
        geom_point(size = 0.1, color = "steel blue") + 
        xlab("Danceability") + 
        ylab("Mean Sentiment Score") + 
        ggtitle("Danceability vs. Sentiment")+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12), 
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5)),
      tooltip = "combination"
    )
  })
  
  output$corr_sent_table <- renderTable({
    corr_table1[, c("Genre", "Correlation Sentiment & BPM" , "Correlation Sentiment & Danceability")]
  })
  
  
  
  output$sent_season <- renderPlot({
    ggplot(df_seasonal_sent_season, aes(x = Season, y = x)) + 
      geom_bar(stat = "identity", fill = "steel blue") + 
      xlab("Months") + 
      ylab("Mean Sentiment Score") + 
      ggtitle("Sentiment over Months")+
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
  })
  
  output$sent_months <- renderPlot({
    df_seasonal_sent_month$Month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") 

    ggplot(df_seasonal_sent_month, aes(x = Month, y = x)) + 
      geom_bar(stat = "identity", fill = "steel blue") + 
      xlab("Months") + 
      ylab("Mean Sentiment Score") + 
      ggtitle("Sentiment over Months")+
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5)) + 
      scale_x_discrete(labels=c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"))
  
    })
  
  
  #####################
  ## Topic Modelling ##
  #####################
  
  output$plot_topics_time <- renderPlot({
    
    df_topics_year2  <- df_topics_year %>%
      gather(key = "Topic", value = "Share", -Date)
    
    ggplot(data= df_topics_year2 , aes(x=Date, y = Share)) + 
      geom_line(aes(color = Topic)) +
      ggtitle("Chart topics across time") + 
      xlab("Time") + 
      ylab("Share of topics in Charts") + 
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
            axis.title=element_text(size=16,face="bold")  ,
            axis.text=element_text(size= 16, face="bold"),
            legend.title = element_text(size= 16, face="bold"),
            legend.text = element_text(size = 14) )
    
  })
  
  
  output$plot_co_topics <- renderPlot({
  
  topic <- input$topic_select
  #topic <- "Love and Romance"
  
  co_occuring_df <- data.frame(co_occuring_df)
  names(co_occuring_df) <- c("Topics", "Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")
  data <- co_occuring_df[, c("Topics", topic)]
  
  data2 <- data[(data$Topics != topic), ]
  
  ggplot(data2[data2$Topics != topic,], aes(x="", y= data2[1:4,2]  , fill = Topics)) +
    geom_bar(stat="identity", width = 1) + 
    coord_polar("y", start=0) + 
    theme_void() + 
    ggtitle("Share of second assigned Topic") + 
    scale_fill_brewer(palette = "Blues") + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          legend.title = element_text(size= 16, face="bold"),
          legend.text = element_text(size = 14) )
  
  
    })
  
  output$plot_genre_topics_time <- renderPlot({
  
  df <- df_topics_genre_year[(df_topics_genre_year$genre == input$topic_genre_select),]  
  
  df <- df %>%
    gather(key = "Topic", value = "Share", -c(Date, genre))
  
  ggplot(data= df , aes(x=Date, y = Share)) + 
    geom_line(aes(color = Topic)) +
    ggtitle(paste0("Chart topics - " , stringr::str_to_title(as.character( input$topic_genre_select)), " Songs" )) + 
    xlab("Time") + 
    ylab("Share of topics in Charts" ) + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title=element_text(size=16,face="bold")  ,
          axis.text=element_text(size= 16, face="bold"),
          legend.title = element_text(size= 16, face="bold"),
          legend.text = element_text(size = 14) )
  
  })

  output$plot_genre_topics <- renderPlot({
  
  prep_genre <- data.frame(prep_genre)
  
  df <- prep_genre[(prep_genre$genre == input$topic_genre_select), ]
  names(df)[2:6] <- c("Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")
  
  df <- df %>%
    gather(key = "Topic", value = "Share", -genre)
  
  
  ggplot(df , aes(x="", y= Share  , fill = Topic )) +
    geom_bar(stat="identity", width = 1) + 
    coord_polar("y", start=0) + 
    theme_void() + 
    ggtitle(paste0("Topics Share - ", stringr::str_to_title(as.character( input$topic_genre_select)), " Songs" )) + 
    scale_fill_brewer(palette = "Blues") + 
    theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          legend.title = element_text(size= 16, face="bold"),
          legend.text = element_text(size = 14) )
  
  
  })
  
  observeEvent(input$button_topic_1, {
    toggle('text_div_3')
    output$text_topic_1 <- renderText({"For hits taks of unsupervised clasification, we used Sentence Transformer, more specifically the pretrained All-mpnet-base-v2 BERT model to obtain semantically meaningful embeddings of all lyrics and of all strings describing the topics. The latter is a Deep Learning Approach which outputs the semantic information on a per-text basis. The closest two topic embeddings (based on cosine-similarity) were deduced. This procedure was manually tested on a high number of songs and was found to function really well."})
    output$text_topic_2 <- renderText({"         "})    
    output$text_topic_3 <- renderText({" Consider the following sources:"})
    output$text_topic_4 <- renderText({"HuggingFace (2020): BERT, URL: https://huggingface.co/docs/transformers/model_doc/bert."})
    output$text_topic_5 <- renderText({"HuggingFace (2021), Model All-mpnet-base-v2, URL: https://huggingface.co/sentence-transformers/all-mpnet-base-v2 Reimers, N., & Gurevych, I. (2019). Sentence-bert: Sentence embeddings using siamese bert-networks. arXiv preprint arXiv:1908.10084."})
    })
  
  
  
  ########################  
  ### Pronoun Analysis ###
  ########################
  
  aux_verbs <- c("be", "have", "will", "could", "would", "should", "shall", "may", "might")
  bi_gram_counts_i <- i_data %>%
    group_by(year) %>%
    summarise(n = sum(n))
  bi_gram_counts_we <- we_data %>%
    group_by(year) %>%
    summarise(n = sum(n))
  bi_gram_counts_she <- she_data %>%
    group_by(year) %>%
    summarise(n = sum(n))
  bi_gram_counts_he <- he_data %>%
    group_by(year) %>%
    summarise(n = sum(n))
  
  
  
  ### Lyrisches Ich
  ## make plot
  output$i_analysis <- renderPlot({
    
    if (input$aux_verbs_i){
      plot_data_i <- i_data %>%
        filter(year == lubridate::year(input$i_year)) %>%
        filter(!verb %in% aux_verbs) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_i$n <- (plot_data_i$n/filter(bi_gram_counts_i, year == lubridate::year(input$i_year))$n) * 100 
      
    } else {
      plot_data_i <- i_data %>%
        filter(year == lubridate::year(input$i_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_i$n <- (plot_data_i$n/filter(bi_gram_counts_i, year == lubridate::year(input$i_year))$n) * 100 
      
    }
    
    plot_data_i %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=14,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank()) +
      labs(y = "Relative Appearance (in %)", x = "")
    
  })
  
  
  ### We are Familyyyy
  ## make plot
  output$we_analysis <- renderPlot({
    
    if (input$aux_verbs_i){
      plot_data_we <- we_data %>%
        filter(year == lubridate::year(input$we_year)) %>%
        filter(!verb %in% aux_verbs) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_we$n <- (plot_data_we$n/filter(bi_gram_counts_we, year == lubridate::year(input$we_year))$n) * 100 
      
    } else {
      plot_data_we <- we_data %>%
        filter(year == lubridate::year(input$we_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_we$n <- (plot_data_we$n/filter(bi_gram_counts_we, year == lubridate::year(input$we_year))$n) * 100 
      
    }
    
    plot_data_we %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=14,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank()) +
      labs(y = "Relative Appearance (in %)", x = "")
  })
  
  
  ### She's got that light around her eyes
  ## make plot
  output$she_analysis <- renderPlot({
    
    if (input$aux_verbs_i){
      plot_data_she <- she_data %>%
        filter(year == lubridate::year(input$she_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        filter(!verb %in% aux_verbs) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_she$n <- (plot_data_she$n/filter(bi_gram_counts_she, year == lubridate::year(input$she_year))$n) * 100 
      
    } else {
      plot_data_she <- she_data %>%
        filter(year == lubridate::year(input$she_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_she$n <- (plot_data_she$n/filter(bi_gram_counts_she, year == lubridate::year(input$she_year))$n) * 100 
      
    }
    
    plot_data_she %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=14,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank()) +
      labs(y = "Relative Appearance (in %)", x = "")
  })
  
  
  ### He
  ## make plot
  output$he_analysis <- renderPlot({
    
    if (input$aux_verbs_i){
      plot_data_he <- he_data %>%
        filter(year == lubridate::year(input$he_year)) %>%
        filter(!verb %in% aux_verbs) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_he$n <- (plot_data_he$n/filter(bi_gram_counts_he, year == lubridate::year(input$he_year))$n) * 100 
      
    } else {
      plot_data_he <- he_data %>%
        filter(year == lubridate::year(input$he_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_he$n <- (plot_data_he$n/filter(bi_gram_counts_he, year == lubridate::year(input$he_year))$n) * 100 
      
    }
    
    plot_data_he %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=14,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank()) +
      labs(y = "Relative Appearance (in %)", x = "")
  })
  
  
  ## Comparison Plot
  output$pronoun_comparison_plot <- renderPlot({
    
    if (input$pr_input_genre == "All"){
      h <- he_data %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        group_by(year) %>%
        summarise(n = sum(n)) %>%
        select(year, n) %>%
        rename("He" = "n") %>%
        left_join(bi_gram_counts_he, by = "year") %>%
        mutate(He = He/n) %>%
        select(year, He)
      
      s <- she_data %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        group_by(year) %>%
        summarise(n = sum(n)) %>%
        select(year, n) %>%
        rename("She" = "n") %>%
        left_join(bi_gram_counts_she, by = "year") %>%
        mutate(She = She/n) %>%
        select(year, She)
      
      w <- we_data %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        group_by(year) %>%
        summarise(n = sum(n)) %>%
        select(year, n) %>%
        rename("We" = "n") %>%
        left_join(bi_gram_counts_we, by = "year") %>%
        mutate(We = We/n) %>%
        select(year, We)
      
      i <- i_data %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        group_by(year) %>%
        summarise(n = sum(n)) %>%
        select(year, n) %>%
        rename("I" = "n") %>%
        left_join(bi_gram_counts_i, by = "year") %>%
        mutate(I = I/n) %>%
        select(year, I)
    } else {
      h <- he_data %>%
        filter(genre == input$pr_input_genre) %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        select(year, n) %>%
        rename("He" = "n") %>%
        left_join(bi_gram_counts_he, by = "year") %>%
        mutate(He = He/n) %>%
        select(year, He)
      
      s <- she_data %>%
        filter(genre == input$pr_input_genre) %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        select(year, n) %>%
        rename("She" = "n") %>%
        left_join(bi_gram_counts_she, by = "year") %>%
        mutate(She = She/n) %>%
        select(year, She)
      
      w <- we_data %>%
        filter(genre == input$pr_input_genre) %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        select(year, n) %>%
        rename("We" = "n") %>%
        left_join(bi_gram_counts_we, by = "year") %>%
        mutate(We = We/n) %>%
        select(year, We)
      
      i <- i_data %>%
        filter(genre == input$pr_input_genre) %>%
        filter(verb == input$input_verb) %>%
        arrange(year) %>%
        select(year, n) %>%
        rename("I" = "n") %>%
        left_join(bi_gram_counts_i, by = "year") %>%
        mutate(I = I/n) %>%
        select(year, I)
    }
    
    d <- full_join(h, s, by = "year") %>%
      full_join(w, by = "year") %>%
      full_join(i, by = "year") %>%
      gather(-year, key = "Pronoun", value = "Count") %>%
      mutate(Count = Count*100)
    
    d[is.na(d)] <- 0
    
    
    d %>%
      filter(Pronoun %in% input$selected_pronoun) %>%
      ggplot(aes(x = year, y = Count)) +
      geom_line(aes(color = Pronoun), size = 1.5) +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5)) +
      labs(y = "Relative Appearance (in %)", x = "Year")
    
  })
  
  
  ### similarity analysis 
  
  
  output$similarity_time <- renderPlot({
    ggplot(similarity_topics_df, aes(x = p1, y = p2, text = combination)) + 
      geom_point(size = 0.35, aes(color = `First Chart Appearance` ) )  + 
      xlab("Principal Component 1 Values") + 
      ylab("Principal Component 2 Values") + 
      ggtitle("PCA of SBERT Embeddings across genre")+
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12), 
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(0.5,"cm") ) 
    
  })
  
  
  
  output$similarity_genre <- renderPlot({
  df <- similarity_topics_df[similarity_topics_df$genre %in% input$genre_similarity_select , ]
  ggplot(df, aes(x = p1, y = p2, text = combination)) + 
    geom_point(size = 0.75, aes(color = genre)) + #  color = "steel blue") + 
    xlab("Principal Component 1 Values") + 
    ylab("Principal Component 2 Values") + 
    ggtitle("PCA of SBERT Embeddings across genre")+
    theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 13, hjust = 0.5),
          legend.text = element_text(size=12), 
          legend.title = element_text(size=13 , face="bold",  hjust = 0.5))
  })
  
  
  observeEvent(input$button_similarity_1, {
    toggle('text_div_2')
    output$text_sim_1 <- renderText({"For obtainining the lyrical similarity, we have embedded all cleaned lyrics using a Sentence BERT Model, a complex Deep Learning Model which outputs the semantic information per sentence or text in a high-dimensional space. These have been the input to the Principal Component Analysis used for mapping the high dimensional space to a 2d-subspace."})
  
    output$text_sim_2 <- renderText({"         "})    
    output$text_sim_3 <- renderText({" Consider the following sources:"})
    output$text_sim_4 <- renderText({"HuggingFace (2020): BERT, URL: https://huggingface.co/docs/transformers/model_doc/bert."})
    output$text_sim_5 <- renderText({"HuggingFace (2021), Bert base NLI mean Tokens, URL:https://huggingface.co/sentence-transformers/bert-base-nli-mean-tokens "})
  })
  
  

    
  
  output$songs <- renderText({
    if (nrow(similarity_topics_df[similarity_topics_df$combination == input$similarity_string, ]) > 0) {
      text_out_1 <- "The threee most similar songs lyrics-wise are: 1) "
      text_out_2 <- as.character(similarity_topics_df$s1[similarity_topics_df$combination == input$similarity_string] )
      text_out_3 <- ", 2) "
      text_out_4 <- as.character(similarity_topics_df$s2[similarity_topics_df$combination == input$similarity_string] )
      text_out_5 <- " and 3) "
      text_out_6 <- as.character(similarity_topics_df$s3[similarity_topics_df$combination == input$similarity_string] )
      text_out_7 <- "!"
      text_out <- paste0(text_out_1, text_out_2, text_out_3, text_out_4, text_out_5, text_out_6, text_out_7)
    } else {
      text_out <- "This combination did not match any song. Perhaps it has never entered the Billboard Hot 100. Try another Style of Spelling or another song."
    }
    
    #text_out
  })
} ## end of server function 

