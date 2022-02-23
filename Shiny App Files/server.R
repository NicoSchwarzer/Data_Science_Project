
##################
## Shiny Server ##
################## 



server <- function(input, output, session) {
  
  
  ##############################
  ## Reading In reactive Data ## 
  ##############################
  
  #### Data for overview data ####
  
  df_over <- reactive({
    if (input$refresh_data < 1){
      df_over <- data.table::fread("df_over.csv")
    } else {
      df_over <- data.table::fread("df_over.csv")
    }
    data.frame(df_over)
  })
  
  
  #### Data for Length and Complexity ####
  
  df_lengths_genres_dates <- reactive({
    if (input$refresh_data < 1){
      df_lengths_genres_dates <- data.table::fread("df_lengths_genres_dates.csv")
    } else {
      df_lengths_genres_dates <- data.table::fread("df_lengths_genres_dates.csv")
    }
    data.frame(df_lengths_genres_dates)
  })
  
  length_duration <- reactive({
    if (input$refresh_data < 1){
      length_duration <- data.table::fread("length_duration.csv")
    } else {
      length_duration <- data.table::fread("length_duration.csv")
    }
    data.frame(length_duration)
  })
  
  df_uniques <- reactive({
    if (input$refresh_data < 1){
      df_uniques <- data.table::fread("df_uniques.csv")
    } else {
      df_uniques <- data.table::fread("df_uniques.csv")
    }
    data.frame(df_uniques)
  })
  
  
  #### Data for Sentiment Analysis ####
  
  corr_table1 <- reactive({
    if (input$refresh_data < 1){
      corr_table1 <- data.table::fread("corr_table1.csv")
    } else {
      corr_table1 <- data.table::fread("corr_table1.csv")
    }
    data.frame(corr_table1, check.names = FALSE)
  })
  
  df_seasonal_sent_month <- reactive({
    if (input$refresh_data < 1){
      df_seasonal_sent_month <- data.table::fread("df_seasonal_sent_month.csv")
    } else {
      df_seasonal_sent_month <- data.table::fread("df_seasonal_sent_month.csv")
    }
    data.frame(df_seasonal_sent_month, check.names = FALSE)
  })
  
  df_seasonal_sent_season <- reactive({
    if (input$refresh_data < 1){
      df_seasonal_sent_season <- data.table::fread("df_seasonal_sent_season.csv")
    } else {
      df_seasonal_sent_season <- data.table::fread("df_seasonal_sent_season.csv")
    }
    data.frame(df_seasonal_sent_season, check.names = FALSE)
  })
  
  
  
  #### Data for Pronoun Analysis ####
  
  w_she_data <- reactive({
    if (input$refresh_data < 1){
      she_data <- data.table::fread("she_data.csv")
    } else {
      she_data <- data.table::fread("she_data.csv")
    }
    data.frame(she_data)
    
  })
  
  w_bi_gram_counts_she <- reactive({
    bi_gram_counts_she <- w_she_data() %>%
      group_by(year) %>%
      summarise(n = sum(n))
    data.frame(bi_gram_counts_she)
    
  })
  
  w_he_data <- reactive({
    if (input$refresh_data < 1){
      he_data <- data.table::fread("he_data.csv")
    } else {
      he_data <- data.table::fread("he_data.csv")
    }
    data.frame(he_data)
    
  })
  w_bi_gram_counts_he <- reactive({
    bi_gram_counts_he <- w_he_data() %>%
      group_by(year) %>%
      summarise(n = sum(n))
    data.frame(bi_gram_counts_he)
    
  })
  
  w_we_data <- reactive({
    if (input$refresh_data < 1){
      we_data <- data.table::fread("we_data.csv")
    } else {
      we_data <- data.table::fread("we_data.csv")
    }
    data.frame(we_data)
    
  })
  bi_gram_counts_we <- reactive({
    bi_gram_counts_we <- w_we_data() %>%
      group_by(year) %>%
      summarise(n = sum(n))
    data.frame(bi_gram_counts_we)
    
  })
  
  i_data <- reactive({
    if (input$refresh_data < 1){
      i_data <- data.table::fread("i_data.csv")
    } else {
      i_data <- data.table::fread("i_data.csv")
    }
    data.frame(i_data)
    
  })
  bi_gram_counts_i <- reactive({
    bi_gram_counts_i <- i_data() %>%
      group_by(year) %>%
      summarise(n = sum(n))
    data.frame(bi_gram_counts_i)
    
  })
  
  
  #### Data for Quiz ####
  
  quiz_lyrics_data <- reactive({
    if (input$refresh_data < 1){
      quiz_lyrics_data <- data.table::fread("quiz_lyrics_data.csv")
      quiz_lyrics_data$best_rank <- as.numeric(quiz_lyrics_data$best_rank)
    } else {
      quiz_lyrics_data <- data.table::fread("quiz_lyrics_data.csv")
      quiz_lyrics_data$best_rank <- as.numeric(quiz_lyrics_data$best_rank)
    }
    data.frame(quiz_lyrics_data, check.names = FALSE)
  })
  
  
  
  #######################
  ## Introductory Page ## 
  #######################
  
  
  output$overview_table <- renderTable({
    
    df_over_print <- data.frame("First Date" = as.character(df_over()[,1]),
                                "Most recent Date" = as.character(df_over()[,2]),
                                "Number of Songs scraped" = as.integer(df_over()[,3]),
                                "Songs with available Lyrics" = as.integer(df_over()[,4]),
                                "Unique available Songs" = as.integer(df_over()[,5]),
                                check.names = FALSE)
    
    
    
    df_over_print
  },    align = "c")
  
  ## Info box Approach
  observeEvent(input$explain_button, {
    shinyalert(title = "Details on our Approach",
               text = tags$div("1) Technical Details",
                               tags$br(),
                               "This App is hosted on a R-Shiny Server located on the BW-Cloud. All code scipts and data is also located within this cloud environment. This allows 
                    for a smooth and continuous functionality - especially considering the weekly updating mechanism that is laid out below",
                               tags$br(),
                               tags$br(),
                               "2) Scraping the Songs",
                               tags$br(),
                               "All Billboard Hot 100 Songs and corresponding Artists from 1960 until today have been scraped from the Company's webpage - resulting in 100 songs for each
                    week of the past 60 years. The Billboard Top 100 were chosen because of the availability for such a long time period which allows 
                    for a thorough and long-term analysis of the lyrics of popular music.",
                               tags$br(),
                               tags$br(),
                               "3) Acquiring Lyrics & Additional data",
                               tags$br(),
                               "The Lyrics were obtained using the GeniusAPI. For the Genres, a three-step approach was implemented based on the AudioDB API, the FastFM API and the Spotify API (in that order). 
                               Since Spotify merely stores the Genres per Artist, only for songs for which the other APIs cannot return a Genre, the Spotiy API is consulted. However, Spotify stores Acoustic Features such as the Length of Songs at a per-Song Level, which we made use of.",
                               tags$br(),
                               "Since many Song Titles and Artist Names come with different styles of spelling, convoluted queries that manipulate both strings in many
                    ways before calling the APIs were set up.",
                               tags$br(),
                               tags$br(),
                               "4) Language Identification and Translation",
                               tags$br(),
                               "All our analyses rely on Lyrics in English. Thus, we implemented a Language-Detection Algorithm (XGBoost based) to 
                    identify the Language of all songs. Using the DeepL API after that, all Non-English Lyrics were translated.",
                               tags$br(),
                               tags$br(),
                               "5) Cleaning and Lyrical Examination",
                               tags$br(),
                               "We found it to be optimal to run relevant text cleaning steps depending on the type of Research Question under consideration. The latter correspond to the
                    pages of this App. The information buttons on each of the pages reveal more on the exact procedures. What is more, the user is invited to derive one's own 
                    conclusions and play aaround with the implemented features.",
                               tags$br(),
                               tags$br(),
                               "6) Weekly Updating",
                               tags$br(),
                               "Lastly, we implemeted a mechanism that scrapes all new Chart Songs available and performs all above mentioned steps. 
                    The Sentiment Analysis, the Topic Modelling and the Similarity Analysis constitue exceptions to this as these rely on complex Deep Leanring models and
                    were implemented on external GPU serves. This, sadly, prevents a weekly updating for these pages.",
                               tags$br(),
                               style = "text-align: justify;" ),
               # text = t,
               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")
  } )
  
  
  ## Info box Resources
  observeEvent(input$resources_button, {
    shinyalert(title = "Resources Used",
               text = tags$div("Below, you can find the References to all used Resources. These mainly include data sources for 
               our Analysis as well as Model Training and specific Machine Learning Models and Approaches.",
                               tags$br(),
                               tags$br(),
                               "Beth Levin (1993). English Verb Classes And Alternations: A Preliminary Investigation. The University of Chicago Press.",
                               tags$br(),
                               "Billboard Media, LLC. (2022). The Top 100, URL: https://www.billboard.com/charts/hot-100/.",
                               tags$br(),
                               "Ãano, Erion and Morisio, Maurizio (2017). MoodyLyrics: A Sentiment Annotated Lyrics Dataset. In: 2017 International Conference on Intelligent Systems, Metaheuristics & Swarm Intelligence, Hong Kong, March, 2017. pp. 118-124.",
                               tags$br(),
                               "DeepL SE (2022). DeepL API, URL: https://www.deepl.com/docs-api/.",
                               tags$br(),
                               "Devlin, J. and Ming, W.C. (2018): Pre-training of Deep Bidirectional Transformers for Language. International Conference on Intelligent Systems, Metaheuristics & Swarm Intelligence, Hong Kong, March, 2017. pp. 118-124.",
                               tags$br(),
                               "Genius Media Group Inc. (2022): The Genius API, URL: https://genius.com/developers.",
                               tags$br(),
                               "Heitmann, Mark, Siebert, Christian and Hartmann, Jochen and Schamp, Christina (2020). More than a feeling: Benchmarks for sentiment analysis accuracy.",
                               tags$br(),
                               "HuggingFace (2020): BERT, URL: https://huggingface.co/docs/transformers/model_doc/berts.",
                               tags$br(),
                               "HuggingFace (2021), BERT base uncased, URL: https://huggingface.co/bert-base-uncased.",
                               tags$br(),
                               "HuggingFace (2021), Model All-mpnet-base-v2, URL: https://huggingface.co/sentence-transformers/all-mpnet-base-v2.",
                               tags$br(),
                               "HuggingFace (2021), Sentiment-roberta-large-english, URL: https://huggingface.co/siebert/sentiment-roberta-large-english.",
                               tags$br(),
                               "Julia Silge and David Robinson (2016). Tidytext: Text Mining and Analysis Using Tidy Data Principles in R. The Open Journal.",
                               tags$br(),
                               "Kipper, K., Korhonen, A., Ryant, N. and Palmer, M. (2008). A large-scale classification of English verbs. Language Resources and Evaluation, 42(1), 21-40.",
                               tags$br(),
                               "Last.fm Ltd. (2022). The LastFM API, URL: https://www.last.fm/de/api.",
                               tags$br(),
                               "Project Gutenberg. (n.d.). Retrieved December 02, 2021, URL: www.gutenberg.org.",
                               tags$br(),
                               "Reimers, Nils and Gurevych, Iryna (2019). Sentence-bert: Sentence embeddings using siamese bert-networks. arXiv preprint arXiv:1908.10084.",
                               tags$br(),
                               "Spotify AB (2022). Web API, URL: https://developer.spotify.com/documentation/web-api/.",
                               tags$br(),
                               "TheAudioDB.com (2021). The Audio DB API, URL: https://www.theaudiodb.com/api_guide.php.",
                               tags$br(),
                               "Timothy Conwell (2021). textTools: Functions for Text Cleansing and Text Analysis, ULR: https://CRAN.R-project.org/package=textTools.",
                               tags$br(),
                               style = "text-align: justify;" ),
               
               # text = t,
               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")
  } )
  
  
  ###########################
  ## Length and complexity ##
  ###########################  
  
  
  ## Hidden information button
  observeEvent(input$button_length_1, {
    toggle('text_div')
    output$text_length_1 <- renderText({
      "At closer inspection, one also sees how songs of the genre rock-and-roll and rock songs contain comparatively few words, while songs belonging to the genres  dance, jazz, reaggae or rnb contain considerably more.
      Also, for many genres, peaks and drops in the word count are observeable - showing that for some time periods, the charts songs of some genre might be substantially longer or shorter than common for that genre.
      This might, however, also be attributed to the fact that in some years, only few songs of one genre were present in the Hot 100. Should these few songs be extraordinarily log or short, a peak or drop occurs.
      Also when removing stopwords and onomatopoetics, while the overall number of words drops (as expected), the rising trend is still present. The per-genre development of lyrics length differs only slightly from the case of no word removals for most genres.
      An exception is constituted by reggae music with peaks around 1990 and 2005. This reveals that reggae music uses comparatively fewer stopwords and onomatopoetics than other genres.




      Concerning implementation, the english stopwords provided in BaseR as well as a extensive handpicked lists of onomatopoetics retrieved from our song database were utilized for this."
    })
  })




  ## Plot complexity 1
  output$plot_complexity_1 <- renderPlot({

    plotData <- data.frame(df_lengths_genres_dates()[df_lengths_genres_dates()$genre %in% input$Genre_compl_1, ])

    ggplot(data= plotData , aes(x=Date,  y= Mean_Song_Length) ) +
      geom_line( aes(colour = genre)) +
      xlab("Time") +
      ylab("Words per song") +
      ggtitle("Mean Number of words per song")+
      labs(color='Genre')  +
      stat_smooth(method=lm, colour = "black") +
      labs(  subtitle="Overall trend in black") +
      #scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5),   #) +
            plot.background = element_rect(fill="#F7FBFF"),
            legend.key =  element_rect(fill = "#F7FBFF") )
  })

  ## Plot complexity 2
  output$plot_complexity_2 <- renderPlot({

    plotData <- data.frame(df_lengths_genres_dates()[df_lengths_genres_dates()$genre %in% input$Genre_compl_1, ])
    # ggplotly(
    ggplot(data= plotData , aes(x=Date,  y = Mean_Red_Song_Length) ) +
      geom_line( aes(colour = genre)) +
      xlab("Time") +
      ylab("Words per song") +
      ggtitle("No stopwords or onomatopoetics")+
      labs(color='Genre')  +
      stat_smooth(method=lm, colour = "black") +
      labs(  subtitle="Overall trend in black") +
      #scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF"))

    # )
  })



  ## Info box Topics
  observeEvent(input$length_button, {
    shinyalert(title = "Approach for Length and  Complexity Analysis",
               text = tags$div("Taking Care of Stopwords and Onomatopoetics",
                               tags$br(),
                               "Words commonly assumned to be Stopwords in the English language are readily available in BaseR.
                For the Onomatopoetics, we manually extracted the 5000 most common words in the Lyrics of different time periods and based on this, created a list of onomatopoetics present in the Lyrics.",
                               tags$br(),
                               tags$br(),
                               "Word Stemming",
                               tags$br(),
                               "The word stemming, i.e.g the mapping of all words to overarchign word stems was done utilizing R's TextTools Package. A check is run to determine the word stems per token per Lyrics. For a reference, click ",
                               tags$a(href = "https://www.rdocumentation.org/packages/RTextTools/versions/1.4.3/topics/wordStem", "here", target="_blank"),
                               "!",
                               tags$br(),
                               tags$br(),
                               "Obtaining Adverbs",
                               tags$br(),
                               "The determine which of the tokens in the Lyrics are Adverbs, one can use R's tidytext package.
                The function 'nma_words' contains lists of all English tokens that belonging to a certain grammatical word class - one of which being Adverbs.
                For a reference,click ",
                               tags$a(href = "https://rdrr.io/cran/tidytext/man/nma_words.html", "here", target="_blank"),
                               "!",
                               style = "text-align: justify;" ),

               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")
  } )



  output$relationship_1 <- renderPlotly({
    # renderPLotly

    if (input$Relation_select == "Potential Driving Force: Tempo") {
      ggplotly(
        ggplot(df_uniques()[df_uniques()$tempo > 10,], aes(x = tempo, y = len, text = combination)) +
          geom_point(size = 0.03, color = "steel blue") +
          xlab("Beats per Minute") +
          ylab("Words per Song") +
          ggtitle("BPM vs. Lyrics Length")+
          ylim(0, 800) +
          xlim(0, 220) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                axis.title=element_text(size=11,face="bold")  ,
                axis.text=element_text(size= 11, face="bold"),
                plot.subtitle = element_text(size = 11, hjust = 0.5),
                legend.text = element_text(size=12),
                legend.title = element_text(size=11 , face="bold",  hjust = 0.5),
                plot.background = element_rect(fill="#F7FBFF")),
        tooltip = "combination"
      )
    } else { ## in case of duration
      ggplotly(
        ggplot(df_uniques()[df_uniques()$duration > 30,], aes(x = duration, y = len, text = combination)) +
          geom_point(size = 0.03, color = "steel blue") +
          xlab("Duration (in seconds)") +
          ylab("Words per Song") +
          ggtitle("Duration vs. Lyrics Length")+
          ylim(0, 800) +
          xlim(0, 600) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # 18
                axis.title=element_text(size=11,face="bold")  ,    # 14
                axis.text=element_text(size= 11, face="bold"), # 14
                plot.subtitle = element_text(size = 11, hjust = 0.5), # 14
                legend.text = element_text(size=12),
                legend.title = element_text(size=11 , face="bold",  hjust = 0.5),
                plot.background = element_rect(fill="#F7FBFF")),
        tooltip = "combination"
      )
    }
  })



  output$relationship_2 <- renderPlot({
    # renderPLotly
    if (input$Relation_select == "Potential Driving Force: Tempo") {
      ggplot(length_duration(), aes(x = Date, y = Mean_Tempo)) +
        geom_line(color = "steel blue") +
        xlab("Time") +
        ylab("Mean BPM") +
        ggtitle("Mean Beats per Minute")+
        ylim(115, 128) +
        #geom_vline(xintercept= as.Date("1992-01-01"), linetype="dashed", color = "black") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12),
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
              plot.background = element_rect(fill="#F7FBFF"))

    } else {   ## in case of duration
      ggplot(length_duration(), aes(x = Date, y = Mean_Duration)) +
        geom_line(color = "steel blue") +
        xlab("Time") +
        ylab("Mean Duration (in seconds)") +
        ggtitle("Mean Duration of songs")+
        ylim(100, 280) +
        #geom_vline(xintercept= as.Date("1992-01-01"), linetype="dashed", color = "black") +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=12),
              legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
              plot.background = element_rect(fill="#F7FBFF"))


    }
  })


  ## Correlation Table
  #  output$correlations_table_compl1 <- renderTable({corr_table1[,c("Genre", "Correlation Lyrics Length & BPM", "Correlation Lyrics Length & Duration")]})

  ## Complexity part in the page

  output$plot_complex_un_1 <- renderPlot({
    plotData <- data.frame(df_lengths_genres_dates()[df_lengths_genres_dates()$genre %in% input$Genre_compl_2, ])
    # ggplotly(
    ggplot(data= plotData , aes(x=Date,  y= Mean_Compl ) ) +
      geom_line( aes(colour = genre)) +
      xlab("Time") +
      ylab(" # Unique words / Length") +
      ggtitle(" Unique words / Length")+
      stat_smooth(method=lm, colour = "black") +
      labs(color='Genre')  +
      labs(  subtitle="Overall trend in black") +
      #scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF"))

    # )
  })

  output$plot_complex_un_2 <- renderPlot({
    plotData <- data.frame(df_lengths_genres_dates()[df_lengths_genres_dates()$genre %in% input$Genre_compl_2, ])
    # ggplotly(
    ggplot(data= plotData , aes(x=Date,  y= Mean_Compl_Stem ) ) +
      geom_line( aes(colour = genre)) +
      xlab("Time") +
      ylab(" # Unique word stems / Length") +
      ggtitle(" Unique word stems / Length")+
      labs(color='Genre')  +
      stat_smooth(method=lm, colour = "black") +
      labs(  subtitle="Overall trend in black") +
      #scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF"))

    # )
  })

  output$correlations_table_compl2 <- renderTable({corr_table1()[,c("Genre", "Percentage of adverbs")]})

  output$adverb_slider <- renderPlot({
    plotData <- data.frame(df_lengths_genres_dates()[df_lengths_genres_dates()$genre %in% input$Genre_compl_3, ])
    # ggplotly(
    ggplot(data= plotData , aes(x=Date,  y= Rel_Adverbs) ) +
      geom_line( aes(colour = genre)) +
      xlab("Time") +
      ylab(" Percentage of Adverbs") +
      ggtitle(" Relative frequency of adverbs")+
      labs(color='Genre')  +
      stat_smooth(method=lm, colour = "black") +
      labs(  subtitle="Overall trend in black") +
      #scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF"))

    # )
  })








  ########################
  ## Sentiment Analysis ##
  ########################

  output$plot_sentiment_1 <- renderPlot({
    plotData <- data.frame(df_sents_genre_year[df_sents_genre_year$genre %in% input$Genre_sent_1, ])
    # ggplotly(
    ggplot(data= plotData , aes(x=Date,  y= Scores) ) +
      geom_line( aes(colour = genre)) +
      xlab("Time") +
      ylab("Mean Sentiment score") +
      ggtitle("Mean Sentiment Score per song")+
      stat_smooth(method=lm, colour = "black") +
      labs(  subtitle="Overall trend in black") +
      # scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 16, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=18 , face="bold",  hjust = 0.5),
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(0.5,"cm"),
            plot.background = element_rect(fill="#F7FBFF"))

    # )
  })


  ## Info box Sentiment
  observeEvent(input$sent_button, {
    shinyalert(title = "Approach for Sentiment Analysis",
               text = tags$div("To capture the sentiment of the Lyrics, different supervised Deep Learning Approaches were implemented. All Approaches are based on Transformers. Since the scraped songs do not contain Labels indicating the Sentiment,
                                the 'labelled Lyrics Dataset', a dataset containing 5000 songs and labels describing their mood (positive vs. negative) was used for training and evaluation.",
                               tags$br(),
                               tags$br(),
                               "First Method - Fine Tuning a Sentiment Transformer",
                               tags$br(),
                               tags$br(),
                               "Many Deep Leanring Models in the NLP Realm were pretrained for specific excercise, such as sentiment classification. A model that was pretrained to predict binary sentiment values is the
                               'sentiment-roberta-large-english' model. This model weas once just applied to the test set and once specifically trained and then evaluated. The results were as follows: ",
                               tags$li("Micro F1 without fine-tuning:  0.598") ,
                               tags$li("Micro F1 with fine-tuning: 0.644") ,
                               tags$br(),
                               tags$br(),
                               "Base BERT Model with added linear Layers",
                               tags$br(),
                               tags$br(),
                               "Alternatively, a BERT model can be combined with linear Layers for the Classification Task. This was tried for one and two linear Layers on top. After fine-tuning the models, the test F1 Scores were as follows below. Because this model
                               outperforms the others, the BERT Model with two added layers was chosen and finally used for the Lyrics data.",
                               tags$li("Micro F1 for Model with 1 Hidden Layer: 0.734") ,
                               tags$li("Micro F1 for Model with 2 Hidden Layers: 0.834") ,
                               tags$br(),
                               tags$br(),
                               "Consider the following sources:",
                               tags$br(),
                               "HuggingFace (2020): BERT, to be found here: " ,
                               tags$a(href = "https://huggingface.co/docs/transformers/model_doc/bert", "Link", target="_blank"),
                               tags$br(),
                               "HuggingFace (2021), BERT base uncased, to be found here: ",
                               tags$a( href = "https://huggingface.co/bert-base-uncased", "Link", target="_blank"),
                               tags$br(),
                               "HuggingFace (2021), Sentiment-roberta-large-english, to be found here: ",
                               tags$a( href = "https://huggingface.co/siebert/sentiment-roberta-large-english", "Link", target="_blank"),
                               tags$br(),
                               "Devlin, J. and Ming, W.C. (2018): Pre-training of Deep Bidirectional Transformers for Language. International Conference on Intelligent Systems, Metaheuristics & Swarm Intelligence, Hong Kong, March, 2017. pp. 118-124",
                               tags$br(),
                               "Heitmann, Mark and Siebert, Christian and Hartmann, Jochen and Schamp, Christina (2020). More than a feeling: Benchmarks for sentiment analysis accuracy",
                               tags$br(),
                               "ÃÂano, Erion; Morisio, Maurizio (2017). MoodyLyrics: A Sentiment Annotated Lyrics Dataset. In: 2017 International Conference on Intelligent Systems, Metaheuristics & Swarm Intelligence, Hong Kong, March, 2017. pp. 118-124",

                               style = "text-align: justify;" ),

               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")

  } )



  output$plot_sentiment_2 <- renderPlot({

    ggplot(data= df_sents_year , aes(x=Date,  y= Scores) ) +
      geom_line(size = 0.8, color = "steel blue") +
      ggtitle("Sentiment and historical events") +
      xlab("Time") +
      ylim(0.47, 0.7) +
      ylab("Mean Sentiment score") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=16,face="bold")  ,
            axis.text=element_text(size= 16, face="bold"),
            plot.subtitle = element_text(size = 16, hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF")
      ) +

      # Oktober 1962 - Cuba Missile Crisis
      geom_vline(xintercept =   df_sents_year$Date[34], color = "black", linetype="dotted", size = 1.2) +
      geom_text(aes(x=df_sents_year$Date[34], label="1", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +

      # July 1996 - Moon Landing
      geom_vline(xintercept =   df_sents_year$Date[109], color = "black", linetype="dotted", size = 1.2) +
      geom_text(aes(x=df_sents_year$Date[109], label="2", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +


      # May 1975 - End of Vietnam War
      geom_vline(xintercept =   df_sents_year$Date[186], color = "black", linetype="dotted", size = 1.2)  +
      geom_text(aes(x=df_sents_year$Date[186], label="3", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +


      # January 1981 - January 1983 - US Economic Crisis
      geom_vline(xintercept =   df_sents_year$Date[253], color = "black" , linetype="dotted", size = 1.2) +
      geom_vline(xintercept =   df_sents_year$Date[277], color = "black", linetype="dotted", size = 1.2) +
      ggplot2::annotate("rect", xmin = df_sents_year$Date[253], xmax = df_sents_year$Date[277], ymin = -Inf, ymax = Inf,
                        alpha = .4) +
      geom_text(aes(x=df_sents_year$Date[253], label="4", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +


      ## July 1988 - December 1991 - Dissolution of the Soviet Union
      geom_vline(xintercept =   df_sents_year$Date[356], color = "black", linetype="dotted", size = 1.2) +
      geom_vline(xintercept =   df_sents_year$Date[385], color = "black", linetype="dotted", size = 1.2) +
      ggplot2::annotate("rect", xmin = df_sents_year$Date[356], xmax = df_sents_year$Date[385], ymin = -Inf, ymax = Inf,
                        alpha = .4) +
      geom_text(aes(x=df_sents_year$Date[356], label="5", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +


      # September 2001 - 9/11
      geom_vline(xintercept =   df_sents_year$Date[501], color = "black", linetype="dotted", size = 1.2) +
      geom_text(aes(x=df_sents_year$Date[501], label="6", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +



      ## March 2003 - Start of second Iraq War
      geom_vline(xintercept =   df_sents_year$Date[519], color = "black", linetype="dotted", size = 1.2)  +
      geom_text(aes(x=df_sents_year$Date[519], label="7", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +


      ## August 2007 - March 2009 US financial crisis
      geom_vline(xintercept =   df_sents_year$Date[572], color = "black", linetype="dotted", size = 1.2)  +
      geom_vline(xintercept =   df_sents_year$Date[592], color = "black", linetype="dotted", size = 1.2)  +
      ggplot2::annotate("rect", xmin = df_sents_year$Date[572], xmax = df_sents_year$Date[592], ymin = -Inf, ymax = Inf,
                        alpha = .4) +
      geom_text(aes(x=df_sents_year$Date[572], label="8", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8) +


      # March 2020 - First Wave of Covid Pandemic commences
      geom_vline(xintercept =   df_sents_year$Date[723], color = "black", linetype="dotted", size = 1.2) +
      geom_text(aes(x=df_sents_year$Date[723], label="9", y=0.5), colour="steel blue", angle=0, vjust = 1, text=element_text(size=14) , size = 8)


  })


  output$table_hist <- renderTable({df_historical[,"Events"] },    align = "l")


  output$corr_sent_tempo   <- renderPlotly({
    ggplotly(
      ggplot(df_uniques_sent[df_uniques_sent$tempo > 10,], aes(x = tempo, y = sentiment, text = combination)) +
        geom_point(size = 0.07, color = "steel blue") + # 0.1
        xlab("Beats per Minute") +
        ylab("Mean Sentiment Score") +
        ggtitle("BPM vs. Sentiment")+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # 14
              axis.title=element_text(size=11,face="bold")  , # 14
              axis.text=element_text(size= 11, face="bold"), # 13
              plot.subtitle = element_text(size = 11, hjust = 0.5), # 13
              legend.text = element_text(size=12), # 12
              legend.title = element_text(size=11 , face="bold",  hjust = 0.5), # 13
              plot.background = element_rect(fill="#F7FBFF"))
      ,
      tooltip = "combination"
    )
  })

  output$corr_sent_dance   <- renderPlotly({
    ggplotly(
      ggplot(df_uniques_sent, aes(x = danceability, y = sentiment, text = combination)) +
        geom_point(size = 0.03, color = "steel blue") +
        xlab("Danceability") +
        ylab("Mean Sentiment Score") +
        ggtitle("Danceability vs. Sentiment")+
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              axis.title=element_text(size=11,face="bold")  ,
              axis.text=element_text(size= 11, face="bold"),
              plot.subtitle = element_text(size = 11, hjust = 0.5),
              legend.text = element_text(size=12),
              legend.title = element_text(size=11 , face="bold",  hjust = 0.5),
              plot.background = element_rect(fill="#F7FBFF"))
      ,
      tooltip = "combination"
    )
  })

  output$corr_sent_table <- renderTable({
    corr_table2[, c("Genre", "Correlation Sentiment & BPM" , "Correlation Sentiment & Danceability")]
  },     align = "c")



  output$sent_season <- renderPlot({
    ggplot(df_seasonal_sent_season(), aes(x = Season, y = x)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      xlab("Seasons") +
      ylab("Mean Sentiment Score") +
      ggtitle("Sentiment over Seasons")+
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF"))

  })

  output$sent_months <- renderPlot({
    ggplot(df_seasonal_sent_month(), aes(x = Month, y = x)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      xlab("Months") +
      ylab("Mean Sentiment Score") +
      ggtitle("Sentiment over Months")+
      theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            plot.background = element_rect(fill="#F7FBFF") ) +
      scale_x_discrete(labels=c("01" = "Jan", "02" = "Feb", "03" = "Mar", "04" = "Apr", "05" = "May", "06" = "Jun", "07" = "Jul", "08" = "Aug", "09" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec"))
  })

  #######################
  ### Topic Modelling ###
  #######################

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
            legend.text = element_text(size = 14) ,
            plot.background = element_rect(fill="#F7FBFF"))


  })


  output$plot_co_topics <- renderPlot({

    topic <- input$topic_select
    #topic <- "Love and Romance"

    co_occuring_df <- data.frame(co_occuring_df)
    names(co_occuring_df) <- c("Topics", "Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")
    data <- co_occuring_df[, c("Topics", topic)]

    data2 <- data[(data$Topics != topic), ]

    ggplot(data2[data2$Topics != topic,], aes(x="", y= data2[1:4,2]  , fill = Topics)) +
      geom_bar(position="fill", stat = "identity", width = 0.5) +
      ggtitle("Share of second assigned Topic") +
      scale_fill_brewer(palette = "Blues") +
      theme_bw() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
            legend.title = element_text(size= 16, face="bold"),
            legend.text = element_text(size = 14),
            plot.background = element_rect(fill="#F7FBFF"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())


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
            legend.text = element_text(size = 14),
            plot.background = element_rect(fill="#F7FBFF"))


  })

  output$plot_genre_topics <- renderPlot({

    prep_genre <- data.frame(prep_genre)

    df <- prep_genre[(prep_genre$genre == input$topic_genre_select), ]
    names(df)[2:6] <- c("Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")

    df <- df %>%
      gather(key = "Topic", value = "Share", -genre)


    ggplot(df , aes(x="", y= Share  , fill = Topic )) +
      geom_bar(position="fill", stat = "identity", width = 0.5) +
      theme_void() +
      ggtitle(paste0("Topics Share - ", stringr::str_to_title(as.character(input$topic_genre_select), " Songs" )) ) + # input$topic_genre_select)
      scale_fill_brewer(palette = "Blues") +
      theme_bw() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0),
            legend.title = element_text(size= 16, face="bold"),
            legend.text = element_text(size = 14),
            plot.background = element_rect(fill="#F7FBFF"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_blank(),
            axis.line = element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())


  })


  ## Info box Topics
  observeEvent(input$topic_button, {
    shinyalert(title = "Approach for Topic Classification",
               text = tags$div("For Assigning the Lyrics to Topics, we could not rely on Supervised Machine Learning Techniques as we did not find labelled data linking Lyrics to Topics. Hence, an unsupervised approach using a Deep-Learning-based Sentence BERT Model was deployed. This model produces embeddings per Input Sequence.
                                The Embeddings were deduced for all Song Lyrics. On top, 5 short strings desccribing the topics were created manually and also embedded. Based on this, for each Song Lyrics Embedding, the most Similar Topic Embeddings can be inferred and the Song can thus be attributed to this Topic.
                                Since there often is no clear-cut assignment of one Song to only one Topic, we inferred the top two Topics per Song. Also, to make sure that this procedure works, hundreds of assigned Lyrics - Topics Pairs were manually checked. This reveals that this technique works outstandignly well.",
                               tags$br(),
                               tags$br(),
                               "The Description Strings we used per Topic are as follows:",
                               tags$br(),
                               tags$ul(
                                 tags$li("Topic Love and Romance: " , tags$em("Love, romance and sex")),
                                 tags$li("Topic Feelgood, friends and Party: ", tags$em("Friends, parties and dancing")),
                                 tags$li("Topic Affluence and Fame: ", tags$em("Money, fame and affluence")),
                                 tags$li("Topic Sadness and Critique: ", tags$em("Anger, critique and sadness")),
                                 tags$li("Topic Motivation and Ambitions: ", tags$em("Motivation and ambition"))
                               ),
                               tags$br(),
                               "Choice of the Model",
                               tags$br(),
                               "We have chosen the pretrained All-mpnet-base-v2 Transformer Model as it is a State-of-the-Art Model when it comes to Sentence Embeddings. Since no training data was available,
                               fine-tuning this model was not possible, yet, as mentioned above, not needed either.",
                               tags$br(),
                               tags$br(),

                               "Consider the following sources:",
                               tags$br(),
                               "HuggingFace (2020): BERT, to be found here: " ,
                               tags$a(href = "https://huggingface.co/docs/transformers/model_doc/bert", "Link", target="_blank"),
                               tags$br(),
                               "HuggingFace (2021), Model All-mpnet-base-v2, to be found here: ",
                               tags$a( href = "https://huggingface.co/sentence-transformers/all-mpnet-base-v2", "Link", target="_blank"),
                               tags$br(),
                               "HuggingFace (2021), Pretrained Models, to be found here: ",
                               tags$a(href = "https://www.sbert.net/docs/pretrained_models.html", "Link", target="_blank"),
                               tags$br(),
                               "Reimers, Nils and Gurevych, Iryna (2019). Sentence-BERT: Sentence Embeddings using Siamese BERT-Networks. Proceedings of the 2019 Conference on Empirical Methods in Natural Language Processing. Association for Computational Linguistics",

                               style = "text-align: justify;" ),
               # text = t,
               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")
  } )


  observeEvent(input$button_topic_1, {
    toggle('text_div_3')
    output$text_topic_1 <- renderText({"For hits taks of unsupervised clasification, we used Sentence Transformer, more specifically the pretrained All-mpnet-base-v2 BERT model to obtain semantically meaningful embeddings of all lyrics and of all strings describing the topics. The latter is a Deep Learning Approach which outputs the semantic information on a per-text basis. The closest two topic embeddings (based on cosine-similarity) were deduced. This procedure was manually tested on a high number of songs and was found to function really well."})
    output$text_topic_2 <- renderText({"         "})
    output$text_topic_3 <- renderText({" Consider the following sources:"})
    output$text_topic_4 <- renderText({"HuggingFace (2020): BERT, URL: https://huggingface.co/docs/transformers/model_doc/bert."})
    output$text_topic_5 <- renderText({"HuggingFace (2021), Model All-mpnet-base-v2, URL: https://huggingface.co/sentence-transformers/all-mpnet-base-v2"})
  })





  ########################
  ### Pronoun Analysis ###
  ########################

  aux_verbs <- c("be", "have", "will", "could", "would", "should", "shall", "may", "might")



  ### She - categories
  output$she_categories <- renderPlot({
    she_count_year <- w_bi_gram_counts_she() %>%
      filter(year == lubridate::year(input$she_cat_year)) %>%
      select(n)

    if (input$verb_classification_scheme == "authors_cat"){
      d <- w_she_data() %>%
        filter(year == lubridate::year(input$she_cat_year)) %>%
        filter(!is.na(own_category)) %>%
        group_by(year, own_category) %>%
        summarise(n = sum(n)) %>%
        rename("category" = "own_category")
    } else if (input$verb_classification_scheme == "levins_cat"){
      d <- w_she_data() %>%
        filter(year == lubridate::year(input$she_cat_year)) %>%
        filter(!is.na(levin_category)) %>%
        group_by(year, levin_category) %>%
        summarise(n = sum(n)) %>%
        rename("category" = "levin_category") %>%
        arrange(desc(n)) %>%
        slice(1:12)
    }

    d$n <- d$n/she_count_year$n[1] * 100

    ggplot(d, aes(reorder(category, n), n)) +
      coord_flip() +
      geom_bar(stat = "identity", fill = "steel blue") +
      scale_color_brewer(palette = "Blues") +
      ggtitle("Third Person: She") +
      xlab("") +
      ylab("Relative Appearance (in %)") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13),
            legend.title = element_text(size= 16, face="bold"),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill = "#EBF5FE", colour = "#EBF5FE"),
            panel.grid.major = element_line(colour = "#EBF5FE"),
            panel.grid.minor = element_line(colour = "#EBF5FE"),
            plot.background = element_rect(fill="#F7FBFF"))

  })



  ### he - categories
  output$he_categories <- renderPlot({
    he_count_year <- w_bi_gram_counts_he() %>%
      filter(year == lubridate::year(input$he_cat_year)) %>%
      select(n)

    if (input$verb_classification_scheme == "authors_cat"){
      d <- w_he_data() %>%
        filter(year == lubridate::year(input$he_cat_year)) %>%
        filter(!is.na(own_category)) %>%
        group_by(year, own_category) %>%
        summarise(n = sum(n)) %>%
        rename("category" = "own_category")
    } else if (input$verb_classification_scheme == "levins_cat"){
      d <- w_he_data() %>%
        filter(year == lubridate::year(input$he_cat_year)) %>%
        filter(!is.na(levin_category)) %>%
        group_by(year, levin_category) %>%
        summarise(n = sum(n)) %>%
        rename("category" = "levin_category") %>%
        arrange(desc(n)) %>%
        slice(1:12)
    }

    d$n <- d$n/he_count_year$n[1] * 100

    ggplot(d, aes(reorder(category, n), n)) +
      coord_flip() +
      geom_bar(stat = "identity", fill = "steel blue") +
      scale_color_brewer(palette = "Blues") +
      ggtitle("Third Person: He") +
      xlab("") +
      ylab("Relative Appearance (in %)") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13),
            legend.title = element_text(size= 16, face="bold"),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill = "#EBF5FE", colour = "#EBF5FE"),
            panel.grid.major = element_line(colour = "#EBF5FE"),
            panel.grid.minor = element_line(colour = "#EBF5FE"),
            plot.background = element_rect(fill="#F7FBFF"))

  })








  ### Lyrisches Ich
  ## make plot
  output$i_analysis <- renderPlot({

    if (input$aux_verbs_i){
      plot_data_i <- i_data() %>%
        filter(year == lubridate::year(input$i_year)) %>%
        filter(!verb %in% aux_verbs) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_i$n <- (plot_data_i$n/filter(bi_gram_counts_i(), year == lubridate::year(input$i_year))$n) * 100

    } else {
      plot_data_i <- i_data() %>%
        filter(year == lubridate::year(input$i_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_i$n <- (plot_data_i$n/filter(bi_gram_counts_i(), year == lubridate::year(input$i_year))$n) * 100

    }

    plot_data_i %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=10,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "#EBF5FE", colour = "#EBF5FE"),
            panel.grid.major = element_line(colour = "#EBF5FE"),
            panel.grid.minor = element_line(colour = "#EBF5FE"),
            plot.background = element_rect(fill="#F7FBFF"),
            axis.ticks = element_blank()) +
      ggtitle("First Person Singular - I") +
      xlab("") +
      ylab("Relative Appearance (in %)")

  })


  ### We
  ## make plot
  output$we_analysis <- renderPlot({

    if (input$aux_verbs_i){
      plot_data_we <- w_we_data() %>%
        filter(year == lubridate::year(input$we_year)) %>%
        filter(!verb %in% aux_verbs) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_we$n <- (plot_data_we$n/filter(bi_gram_counts_we(), year == lubridate::year(input$we_year))$n) * 100

    } else {
      plot_data_we <- w_we_data() %>%
        filter(year == lubridate::year(input$we_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_we$n <- (plot_data_we$n/filter(bi_gram_counts_we(), year == lubridate::year(input$we_year))$n) * 100

    }

    plot_data_we %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=10,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "#EBF5FE", colour = "#EBF5FE"),
            panel.grid.major = element_line(colour = "#EBF5FE"),
            panel.grid.minor = element_line(colour = "#EBF5FE"),
            plot.background = element_rect(fill="#F7FBFF"),
            axis.ticks = element_blank()) +
      ggtitle("First Person Plural - We") +
      xlab("") +
      ylab("Relative Appearance (in %)")
  })


  ### She
  ## make plot
  output$she_analysis <- renderPlot({

    if (input$aux_verbs_i){
      plot_data_she <- w_she_data() %>%
        filter(year == lubridate::year(input$she_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        filter(!verb %in% aux_verbs) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_she$n <- (plot_data_she$n/filter(w_bi_gram_counts_she(), year == lubridate::year(input$she_year))$n) * 100

    } else {
      plot_data_she <- w_she_data() %>%
        filter(year == lubridate::year(input$she_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_she$n <- (plot_data_she$n/filter(w_bi_gram_counts_she(), year == lubridate::year(input$she_year))$n) * 100

    }

    plot_data_she %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=10,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "#EBF5FE", colour = "#EBF5FE"),
            panel.grid.major = element_line(colour = "#EBF5FE"),
            panel.grid.minor = element_line(colour = "#EBF5FE"),
            plot.background = element_rect(fill="#F7FBFF"),
            axis.ticks = element_blank()) +
      ggtitle("Third Person Singular - She") +
      xlab("") +
      ylab("Relative Appearance (in %)")
  })


  ### He
  ## make plot
  output$he_analysis <- renderPlot({

    if (input$aux_verbs_i){
      plot_data_he <- w_he_data() %>%
        filter(year == lubridate::year(input$he_year)) %>%
        filter(!verb %in% aux_verbs) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_he$n <- (plot_data_he$n/filter(w_bi_gram_counts_he(), year == lubridate::year(input$he_year))$n) * 100

    } else {
      plot_data_he <- w_he_data() %>%
        filter(year == lubridate::year(input$he_year)) %>%
        group_by(verb) %>%
        summarise(n = sum(n)) %>%
        arrange(desc(n)) %>%
        slice(1:15)
      plot_data_he$n <- (plot_data_he$n/filter(w_bi_gram_counts_he(), year == lubridate::year(input$he_year))$n) * 100

    }

    plot_data_he %>%
      ggplot(aes(reorder(verb, n), n)) +
      geom_bar(stat = "identity", fill = "steel blue") +
      coord_flip() +
      scale_color_brewer(palette = "Blues") +
      theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size=10,face="bold"),
            axis.text=element_text(size= 13),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5),
            panel.background = element_rect(fill = "#EBF5FE", colour = "#EBF5FE"),
            panel.grid.major = element_line(colour = "#EBF5FE"),
            panel.grid.minor = element_line(colour = "#EBF5FE"),
            plot.background = element_rect(fill="#F7FBFF"),
            axis.ticks = element_blank()) +
      ggtitle("Third Person Singular - He") +
      xlab("") +
      ylab("Relative Appearance (in %)")
  })


  ## Comparison Plot
  output$pronoun_comparison_plot <- renderPlot({

    if (input$pr_input_genre == "All"){
      h_own <- w_he_data() %>%
        # filter(genre == input$pr_input_genre) %>%
        filter(own_category %in% input$input_author_cat) %>%
        group_by(year, own_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, own_category) %>%
        rename("He" = "n") %>%
        left_join(w_bi_gram_counts_he(), by = "year") %>%
        mutate(count = He/n) %>%
        mutate(comb = paste0("He - ", own_category, " (Authors' Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "He")

      h_levin <- w_he_data() %>%
        # filter(genre == input$pr_input_genre) %>%
        filter(levin_category %in% input$input_levin_cat) %>%
        group_by(year, levin_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, levin_category) %>%
        rename("He" = "n") %>%
        left_join(w_bi_gram_counts_he(), by = "year") %>%
        mutate(count = He/n) %>%
        mutate(comb = paste0("He - ", levin_category, " (Levin's Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "He")


      s_own <- w_she_data() %>%
        # filter(genre == input$pr_input_genre) %>%
        filter(own_category %in% input$input_author_cat) %>%
        group_by(year, own_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, own_category) %>%
        rename("She" = "n") %>%
        left_join(w_bi_gram_counts_she(), by = "year") %>%
        mutate(count = She/n) %>%
        mutate(comb = paste0("She - ", own_category, " (Authors' Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "She")

      s_levin <- w_she_data() %>%
        # filter(genre == input$pr_input_genre) %>%
        filter(levin_category %in% input$input_levin_cat) %>%
        group_by(year, levin_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, levin_category) %>%
        rename("She" = "n") %>%
        left_join(w_bi_gram_counts_she(), by = "year") %>%
        mutate(count = She/n) %>%
        mutate(comb = paste0("She - ", levin_category, " (Levin's Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "She")

    } else {

      h_own <- w_he_data() %>%
        filter(genre == input$pr_input_genre) %>%
        filter(own_category %in% input$input_author_cat) %>%
        group_by(year, own_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, own_category) %>%
        rename("He" = "n") %>%
        left_join(w_bi_gram_counts_he(), by = "year") %>%
        mutate(count = He/n) %>%
        mutate(comb = paste0("He - ", own_category, " (Authors' Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "He")

      h_levin <- w_he_data() %>%
        filter(genre == input$pr_input_genre) %>%
        filter(levin_category %in% input$input_levin_cat) %>%
        group_by(year, levin_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, levin_category) %>%
        rename("He" = "n") %>%
        left_join(w_bi_gram_counts_he(), by = "year") %>%
        mutate(count = He/n) %>%
        mutate(comb = paste0("He - ", levin_category, " (Levin's Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "He")


      s_own <- w_she_data() %>%
        filter(genre == input$pr_input_genre) %>%
        filter(own_category %in% input$input_author_cat) %>%
        group_by(year, own_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, own_category) %>%
        rename("She" = "n") %>%
        left_join(w_bi_gram_counts_she(), by = "year") %>%
        mutate(count = She/n) %>%
        mutate(comb = paste0("She - ", own_category, " (Authors' Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "She")

      s_levin <- w_she_data() %>%
        filter(genre == input$pr_input_genre) %>%
        filter(levin_category %in% input$input_levin_cat) %>%
        group_by(year, levin_category) %>%
        summarise(n = sum(n)) %>%
        arrange(year) %>%
        select(year, n, levin_category) %>%
        rename("She" = "n") %>%
        left_join(w_bi_gram_counts_she(), by = "year") %>%
        mutate(count = She/n) %>%
        mutate(comb = paste0("She - ", levin_category, " (Levin's Cat.)")) %>%
        select(year, count, comb) %>%
        mutate(Pronoun = "She")

    }

    d <- rbind(h_own, h_levin, s_own, s_levin) %>%
      mutate(count = count*100)

    d[is.na(d)] <- 0


    d %>%
      filter(Pronoun %in% input$selected_pronoun) %>%
      ggplot(aes(x = year, y = count)) +
      geom_line(aes(color = comb), size = 1) +
      scale_color_brewer(palette = "Dark2") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),   # size = 14
            axis.title=element_text(size = 14,face="bold")  ,
            axis.text=element_text(size = 14, face="bold"),
            plot.subtitle = element_text(size = 13, hjust = 0.5),
            legend.text = element_text(size=12),
            plot.background = element_rect(fill="#F7FBFF"),
            legend.title = element_text(size=13 , face="bold",  hjust = 0.5)) +
      labs(y = "Relative Appearance (in %)", x = "Year")

  })




  ## Info box Pronoun Categories
  observeEvent(input$pronoun_cats,{
    shinyalert(title = "Categorization of Verbs",
               text = tags$div("Verbs are categorized by two different schemes, the Levin's classification of 1993 and a classification we conducted our own.",
                               tags$br(),
                               tags$br(),
                               "Levin categorized 3,024 english verbs into 48 broad categories.
                               This categorization somewhat serves as the ", tags$em("Gold Standard"), " for linguists when classifying verbs into different categories.
                               A few categories along with some examples are displayed below:",
                               tags$br(),
                               tags$br(),
                               tags$ul(
                                 tags$li("Verbs of Possession: ", tags$em("keep, give, lend, pay, supply, buy, receive")),
                                 tags$li("Perception: ", tags$em("see, sense, feel, detect, hear")),
                                 tags$li("Psychological State: ", tags$em("admire, enjoy, dislike, shock")),
                                 tags$li("Desire: ", tags$em("want, need, desire, wish, pray")),
                                 tags$li("Predactive Complements: ", tags$em("think, act, make, imagine, consider"))
                               ),
                               tags$br(),
                               "The full list of verbs that was used for this classifiction can be found",
                               tags$a(href = "http://www-personal.umich.edu/~jlawler/levin.verbs", "here", target="_blank"),
                               ". Another interesting publication that deals - beside others - with the Levin classification of verbs is the work by ",
                               tags$a(href = "https://verbs.colorado.edu/~kipper/Papers/lrec-journal.pdf", "Kipper et al. (2008)", target="_blank"),
                               ".",
                               tags$br(),
                               tags$br(),
                               "However, the number of categories and the rather large variation within each category can make it difficult to get an actucal qualitative meaning from the classification.
                               That's why we also categorized some verbs on our own into categories of which we thought could help to more accurately extract the overall meaning of actions.
                               All 10 categories along with some examples are shown below:",
                               tags$br(),
                               tags$br(),
                               tags$ul(
                                 tags$li("Thinking: ", tags$em("think, believe, imagine, realize, consider")),
                                 tags$li("Communication: ", tags$em("say, tell, shout, ask, pronoune, speak")),
                                 tags$li("Positive Emotions: ", tags$em("love, like, adore, admire, praise")),
                                 tags$li("Negative Emotions: ", tags$em("hate, dislike, criticize, blame")),
                                 tags$li("Senses: ", tags$em("see, hear, smell, taste, feel, touch")),
                                 tags$li("Possession: ", tags$em("want, desire, need, have, get, take")),
                                 tags$li("Positive Actions: ", tags$em("help, kiss, save, support")),
                                 tags$li("Negative Actions: ", tags$em("kill, steal, hit, fail, break, miss")),
                                 tags$li("Positive Expectations: ", tags$em("hope, wish, trust, pray")),
                                 tags$li("Negative Expectations: ", tags$em("fear, worry"))),
                               style = "text-align: justify;"),
               # text = t,
               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")
  })




  ###########################
  ### Similarity Analysis ###
  ###########################
  
  ## choices songs
  updateSelectizeInput(session, 'similarity_string', choices = combination_choices$x, server = TRUE)

  output$similarity_time <- renderPlot({

    #    similarittopics_df$'First '

    if (input$dim_red == "Principal Component Analysis") {
      ggplot(similarity_topics_df, aes(x = p1, y = p2, text = combination)) +
        geom_point(size = 0.35, aes(color = first_appearance ) )  +
        xlab("Principal Component 1 Values") +
        ylab("Principal Component 2 Values") +
        ggtitle("PCA of SBERT Embeddings across Time")+
        labs(color='Chart Entry Date')  +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    } else if (input$dim_red == "t-SNE") {
      ggplot(similarity_topics_df, aes(x = tsne_1, y = tsne_2, text = combination)) +
        geom_point(size = 0.35, aes(color = first_appearance ) )  +
        xlab("Embedding Dimension 1") +
        ylab("Embedding Dimension 2") +
        ggtitle("t-SNE of SBERT Embeddings across Time")+
        labs(color='Chart Entry Date')  +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    } else if (input$dim_red == "Isomap") {
      ggplot(similarity_topics_df, aes(x = iso_1, y = iso_2, text = combination)) +
        geom_point(size = 0.35, aes(color = first_appearance ) )  +
        xlab("Embedding Dimension 1") +
        ylab("Embedding Dimension 2") +
        ggtitle("Isomap of SBERT Embeddings across Time")+
        labs(color='Chart Entry Date')  +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    } else if (input$dim_red == "UMAP") {
      ggplot(similarity_topics_df, aes(x = UMAP1, y = UMAP2, text = combination)) +
        geom_point(size = 0.35, aes(color = first_appearance ) )  +
        xlab("Embedding Dimension 1") +
        ylab("Embedding Dimension 2") +
        ggtitle("UMAP of SBERT Embeddings across Time")+
        labs(color='Chart Entry Date')  +
        theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    }


  })



  output$similarity_genre <- renderPlot({
    # by genre
    df <- similarity_topics_df[similarity_topics_df$genre %in% input$genre_similarity_select , ]

    # by algorithm
    if (input$dim_red == "Principal Component Analysis") {
      ggplot(df, aes(x = p1, y = p2, text = combination)) +
        geom_point(size = 0.75, aes(color = genre)) + #  color = "steel blue") +
        xlab("Principal Component 1 Values") +
        ylab("Principal Component 2 Values") +
        ggtitle("PCA of SBERT Embeddings across Genres")+
        labs(color='Genre')  +
        theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    } else if (input$dim_red == "t-SNE") {
      ggplot(df, aes(x = tsne_1, y = tsne_2, text = combination)) +
        geom_point(size = 0.75, aes(color = genre)) + #  color = "steel blue") +
        xlab("Embedding Dimension 1") +
        ylab("Embedding Dimension 2") +
        ggtitle("t-SNE of SBERT Embeddings across Genres")+
        labs(color='Genre')  +
        theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    } else if (input$dim_red == "Isomap") {
      ggplot(df, aes(x = iso_1, y = iso_2, text = combination)) +
        geom_point(size = 0.75, aes(color = genre)) + #  color = "steel blue") +
        xlab("Embedding Dimension 1") +
        ylab("Embedding Dimension 2") +
        ggtitle("Isomap of SBERT Embeddings across Genres")+
        labs(color='Genre')  +
        theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    } else if (input$dim_red == "UMAP") {
      ggplot(df, aes(x = UMAP1, y = UMAP2, text = combination)) +
        geom_point(size = 0.75, aes(color = genre)) + #  color = "steel blue") +
        xlab("Embedding Dimension 1") +
        ylab("Embedding Dimension 2") +
        ggtitle("UMAP of SBERT Embeddings across Genres")+
        labs(color='Genre')  +
        theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5),
              axis.title=element_text(size=14,face="bold")  ,
              axis.text=element_text(size= 13, face="bold"),
              plot.subtitle = element_text(size = 13, hjust = 0.5),
              legend.text = element_text(size=14),
              legend.title = element_text(size=16 , face="bold",  hjust = 0.5),
              legend.key.size = unit(1, "cm"),
              legend.key.width = unit(0.5,"cm"),
              plot.background = element_rect(fill="#F7FBFF"))
    }



  })



  ## Info box Lyrical Similarity
  observeEvent(input$sim_button, {
    shinyalert(title = "Approach for Deducing Lyrical Similarity",
               text = tags$div("For obtainining the lyrical similarity, we have embedded all cleaned lyrics using a Sentence BERT Model, a complex Deep Learning Model which outputs the semantic information per sentence or text in a 786-dimensional space. As was done for the Topic Assignment, the pretrained All-mpnet-base-v2 Model provided by Huggingface was used as it a top-performing Model for Sentence Embeddings.
                               The embeddings per Song Lyrics have then been mapped to a 2-dimensional subspace using 4 different Dimensionality Reduction Algorithms: A linear, Variance-based Approoach called Principal Component Analysis (PCA), two non-linear Density Approach called t-distributed Stochastic Neighbor Embedding (t-SNE) and Uniform Manifold Approximation and Projection (UMAP), and another non-linear manifold Technique called Isomap.
                               This mappig enables one to detect the Lyrical Proximities and can reveal in how for some Lyrics are more related than others.",
                               tags$br(),
                               tags$br(),

                               "Consider the following sources:",
                               tags$br(),
                               "HuggingFace (2020): BERT, to be found here: " ,
                               tags$a(href = "https://huggingface.co/docs/transformers/model_doc/bert", "Link", target="_blank"),
                               tags$br(),
                               "HuggingFace (2021), Model All-mpnet-base-v2, to be found here: ",
                               tags$a( href = "https://huggingface.co/sentence-transformers/all-mpnet-base-v2", "Link", target="_blank"),
                               tags$br(),
                               "HuggingFace (2021), Pretrained Models, to be found here: ",
                               tags$a(href = "https://www.sbert.net/docs/pretrained_models.html", "Link", target="_blank"),
                               tags$br(),
                               "Reimers, Nils and Gurevych, Iryna (2019). Sentence-BERT: Sentence Embeddings using Siamese BERT-Networks. Proceedings of the 2019 Conference on Empirical Methods in Natural Language Processing. Association for Computational Linguistics",

                               style = "text-align: justify;" ),
               # text = t,
               type = "info",
               html = TRUE,
               closeOnClickOutside = TRUE,
               size = "m")
  } )




  output$songs_1 <- renderUI({
    if (nrow(similarity_topics_df[similarity_topics_df$combination == input$similarity_string, ]) > 0) { ## checking if it worked at all
      text_0_1 <- "          "
      text_0_2 <- "The three most similar songs are:"
      text_begin <- paste0("1) ", gsub("  ", " - ", as.character(similarity_topics_df$s1[similarity_topics_df$combination == input$similarity_string])), ",  2) ", gsub("  ", " - ", as.character(similarity_topics_df$s2[similarity_topics_df$combination == input$similarity_string]))," and  3) ", gsub("  ", " - ", as.character(similarity_topics_df$s3[similarity_topics_df$combination == input$similarity_string]))  )
      HTML(paste(text_0_1, text_0_2, text_begin, sep = '<br/>'))

    }  else {
      HTML("This combination did not match any song. Perhaps it has never entered the Billboard Hot 100. Try another Style of Spelling or Another Song.")
    }
  })


  output$songs_2 <- renderUI({
    if (nrow(similarity_topics_df[similarity_topics_df$combination == input$similarity_string, ]) > 0) { ## checking if it worked at all

      # getting the texts
      idx0 <- similarity_topics_df$index_0[similarity_topics_df$combination == input$similarity_string]
      idx1 <- similarity_topics_df$index_1[similarity_topics_df$combination == input$similarity_string]
      idx2 <- similarity_topics_df$index_2[similarity_topics_df$combination == input$similarity_string]
      idx3 <- similarity_topics_df$index_3[similarity_topics_df$combination == input$similarity_string]
      ###
      text_song <- as.character( read.table("lyrics_df.dat", skip = idx0, nrows=1)$V1)
      text_1 <- as.character(read.table("lyrics_df.dat",skip=(idx1+1) ,nrows=1 )$V1)
      text_2 <- as.character(read.table("lyrics_df.dat",skip=(idx2+1) ,nrows=1 )$V1)
      text_3 <- as.character(read.table("lyrics_df.dat",skip=(idx3+1) ,nrows=1 )$V1)

      text_0_1 <- "          "

      if (input$song_lyrics == "Lyrics of Choosen Song") {
        text_0_2 <- "The Lyrics of the Choosen Song are:"
        text_begin <- text_song
      } else if (input$song_lyrics == "Most Similar Lyrics") {
        text_0_2 <- "The most similar Lyrics to the choosen song in the Hot 100 of the past 60 years are:"
        text_begin <- text_1
      } else if (input$song_lyrics == "2nd most Similar Lyrics") {
        text_0_2 <- "The second most similar Lyrics to the choosen song in the Hot 100 of the past 60 years are:"
        text_begin <- text_2
      } else if (input$song_lyrics == "3rd most Similar Lyrics") {
        text_0_2 <- "The third most similar Lyrics to the choosen song in the Hot 100 of the past 60 years are:"
        text_begin <- text_3
      }

      HTML(paste(text_0_1, text_0_2, text_begin, sep = '<br/>'))

    } else {  # in case ghe song selection did not work above
      HTML("     ")
    }
  })




  ###########################
  ########### Quiz ##########
  ###########################

  song_list <- reactive({

    if (input$start_quiz < 1000000){

      if(input$quiz_band_input == "All"){
        relevant_quiz_lyrics_data <- quiz_lyrics_data() %>%
          dplyr::filter(best_rank <= input$quiz_rank_filter) %>%
          dplyr::filter(first_appearance > as.Date(input$date_range_quiz[1])) %>%
          dplyr::filter(first_appearance < as.Date(input$date_range_quiz[2]))
      } else {
        relevant_quiz_lyrics_data <- quiz_lyrics_data() %>%
          filter(artists == input$quiz_band_input)

      }


      songs_idx <- sample(1:nrow(relevant_quiz_lyrics_data), size = 4)


      song1 <- relevant_quiz_lyrics_data[songs_idx[1],]
      song2 <- relevant_quiz_lyrics_data[songs_idx[2],]
      song3 <- relevant_quiz_lyrics_data[songs_idx[3],]
      song4 <- relevant_quiz_lyrics_data[songs_idx[4],]

      # rm(d)

      correct_song <- list(song1, song2, song3, song4)[[sample(1:4, 1)]]



      list(song1 = song1,
           song2 = song2,
           song3 = song3,
           song4 = song4,
           correct_song = correct_song)
    }
  })

  # output$test <- renderPrint(str(quiz_lyrics_data()))


  output$output_lyrics <- renderPrint({
    song_list()$correct_song$V3
  })

  output$output_lyrics_2 <- renderPrint({
    # song_list()$correct_song$V3

    paste0(song_list()$song1$V2, " - ", song_list()$song1$V2, "\n",
           song_list()$song2$V2, " - ", song_list()$song2$V2, "\n",
           song_list()$song3$V2, " - ", song_list()$song3$V2, "\n",
           song_list()$song4$V2, " - ", song_list()$song4$V2, "\n")

  })


  output$quiz_wordcloud <- renderPlot({  # renderWordcloud2


    if(input$start_quiz >= 1){

      wordcloud_data <- data.frame(table(tokenizers::tokenize_words(as.character(song_list()$correct_song$lyrics))[[1]])) %>%
        filter(!Var1 %in% stopwords(kind = "en"))
      # filter(Freq > 1)

      # check if title should be excluded
      if(input$quiz_exclude_title == TRUE){

        title <- paste(song_list()$correct_song[,1], " - ", song_list()$correct_song[,2])

        wordcloud_data <- wordcloud_data %>%
          filter(!Var1 %in% tolower(stringr::str_split(title, " ")[[1]]))
      }

      # wordcloud2(wordcloud_data, shape = 'circle')

      # wordcloud(words = wordcloud_data$Var1, freq = wordcloud_data$Freq)
      par(bg = "#E7EBFE")
      wordcloud(words = wordcloud_data$Var1, freq = wordcloud_data$Freq,
                min.freq = 0, max.words = 300, random.order = FALSE,
                colors=brewer.pal(8, "Dark2"))
    }

  })  # , height = 400, width = 600


  observeEvent(input$start_quiz,{


    quiz_choices <- c(
      paste(song_list()$song1[,1], " - ", song_list()$song1[,2]),
      paste(song_list()$song2[,1], " - ", song_list()$song2[,2]),
      paste(song_list()$song3[,1], " - ", song_list()$song3[,2]),
      paste(song_list()$song4[,1], " - ", song_list()$song4[,2])
    )

    # updateRadioButtons(session, "inRadioButtons2",
    updateRadioButtons(session, "inRadioButtons2",
                       choices = quiz_choices,
                       selected = character(0))
  })

  observeEvent(input$sent_guess,{
    if (input$inRadioButtons2 == paste(song_list()$correct_song[,1], " - ", song_list()$correct_song[,2])){
      shinyalert("Correct!", "You know your lyrics pretty well!", type = "success")
    } else {
      shinyalert("Oops!", "That was not correct - try again!", type = "error")
    }
  })



  
} ## end of server function 

