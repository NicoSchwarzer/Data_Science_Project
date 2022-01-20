ui <- navbarPage(
  
  useShinyjs(),
  
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
               tags$h4("The goal of the Project is to gather insights as to how Song Lyrics of very popular songs have evolved over time using NLP tools. Since To this aim, we have scraped all hot 100 Billboard charts from 1960 until today and used the Genuis API to retrieve the lyrics. The Billboard Top 100 were choosen because of their availability in decades past. It is noteworthy, still, that they do not represent the music industry as a whole. Our findings thus apply to popular or economically spoken the demand of music (not its supply). Also, we have gathered genre information with use of the Spotify API and assigned each genre to overarching genres by hand. The Spotify API was also used to obtain information on on each song's acoustic features. The latter include Tempo (BPM), Duration (in seconds), Energy (perceptual measure of a song's liveliness) and Danceability (Suitability for dancing based on rythmic features).  Going further, we have trained and implemented a binary classifier to detect of the songs lyrics are in english and made use of the DeepL API if need be for translation."),
             ),
             fluidRow(
               tags$h4("Obviously, each week, a new hot 100 Chart List is released. To cope with this, we have also implemented a mechanism that performs all above mentioned steps once a week and appends our overall database. As a next step, the lyrics are cleaned and and manipulated for the further analytics tasks. As we gather more lytics each week, all post-processign steps are also re-run on a weekly basis."),
             ),
             fluidRow(
               tags$h3("Overview of available data")),
             fluidRow(
               tags$h4("Here, relevant parameters descriving the scope of the available lyrics data are summarized.")),

             fluidRow( column(12, align="center",  tableOutput("overview_table") ) ),

             )
            ),
  
  tabPanel("Length and Complexity of Lyrics",

           fluidPage(

             ### Song Length ###

             tags$h1("Length of Lyrics"),

             fluidRow(
               tags$h3("Development of Song Lyrics Length across Time")),
             fluidRow(
               tags$h4("When analyzing the length of songs lyrics across time, it becomes very much apparent that the average number of words (quite strikingly) almost doubled from 1960 until today. This trend is observeable across genres (with the exception of rock-n-roll). Quick tip: Do not choose too many genres at the same time.")
             ),
                
             fluidRow(
               tags$h4("Going further, one might analyze if the increase in lyrics length is powered by a more freuqnet use of onomatopoeic words ('Uh', 'Ah', 'La-La'). These, alongside common stopwords of the english language, have been removed from the lyrics. Select genres to see the development across time and genre. To obtain more information, press the button beneat the plots."
               )),

             fluidRow(width=12,
                      column(width=12,
                             selectInput("Genre_compl_1", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "dance"), selectize = TRUE, width = NULL, size = NULL),
                      )),
             fluidRow(width = 12,
                      column(width = 6,
                             plotly::plotlyOutput("plot_complexity_1")) ,

                      column(width = 6 , plotly::plotlyOutput("plot_complexity_2") ) ),
             
             fluidRow(
               tags$h4("  ")
               ),
             actionButton("button_length_1", "Get more info"),
             hidden(
               div(id='text_div',
                   #verbatimTextOutput("text_length_1")
                   shiny::textOutput("text_length_1")
               )
             ),


             fluidRow(
               tags$h3("Accounting for changes in tempo and duration")),

             fluidRow(
               tags$h4("One might guess that with increasing duration and with an increased number of beats per minute, the number of words rises as well. These relationships are laid out below. The positive correlation of duration and lyrics length seems quite strong while the correlation of BPM and lyrics length is rather feeble - and for some genres even around 0. Hover over the scatter plots to see which songs belong to the points.")
             ),
             fluidRow(
               width = 12,
               column(width = 4,
                      plotly::plotlyOutput("relationship_tempo")) ,
               column(width = 4,
                      plotly::plotlyOutput("relationship_duration")) ,
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
               tags$h4("A main point concerning lyrical complexity is the repetitiveness of words utilized. The fewer unique words as well as word stems used, the less complex a text can be deemed. The number of different words and the number of different word stems per length are depicted below.")
               ),
               
             fluidRow(width=12,
                      column(width=12,
                             selectInput("Genre_compl_2", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("dance", "pop", "rock"), selectize = TRUE, width = NULL, size = NULL),
                      )),

             fluidRow(width = 12,
                      column(width = 6,
                             plotly::plotlyOutput("plot_complex_un_1")) ,

                      column(width = 6 , plotly::plotlyOutput("plot_complex_un_2") ) ),
             fluidRow(
               tags$h3("Tell me what you want? Tell me what you REALLY, REALLY want!"),
             ),
             fluidRow(
               tags$h4("Really wanting something is semantically more complex than only wanting something...because of the use of adverbs - a common measurements for the elaborateness of a language. As is exhibited below, the percentage of adverbs of all words lies around 1% with with no overall increase or decrease over the last 60 years, but a high fluctuation from one year to another."),
             ),
             fluidRow(width=12,
                      column(width=12,
                             selectInput("Genre_compl_3", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("dance", "pop", "rock"), selectize = TRUE, width = NULL, size = NULL),
                      )),
             
             fluidRow(width = 12,
                      column(width = 8 , plotly::plotlyOutput("adverb_slider") ),
                      column(width = 4,
                             tableOutput("correlations_table_compl2")) )
           )
  ),
  
  
  
  tabPanel("Sentiment Analysis",
           
           ### Sentiment Analysis Panel ### 
           
           fluidPage(


             fluidRow(
               tags$h3("Development of General Sentiment across Time")),
             fluidRow(
               tags$h4("Texts, including song lyrics, carry a sentiment through their semantic structure. The latter may thus be analysed across the time and genre dimension for HOT 100 Charts of the past 60 years. Note that this project focusses primarily on lyrics, no acoustic features are omiited for emotion detection here. In a first step, the average sentiment score across time and genre dimensions is depicted below. Quick tip: Do not choose too many genres at the same time. As can be seen, the average sentiment of the Hot 100 Song Lyrics has taken a slight fall during the last 60  years. In case of taking an interest in further details and the sentiment algorithm, click on the info button below the graph.")
             ),
             fluidRow(width=12,
                      column(width=12,
                             selectInput("Genre_sent_1", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "folk"), selectize = TRUE, width = NULL, size = NULL),
                      )),

             fluidRow(width = 12,
                      column(width = 12,
                             plotly::plotlyOutput("plot_sentiment_1")) ),
             
             
             
             actionButton("button_sentiment_1", "Get more info"),
             hidden(
               div(id='text_div_1',
                   #verbatimTextOutput("text_length_1")
                   shiny::textOutput("text_sent_1"),
                   shiny::textOutput("text_sent_2"),
                   shiny::textOutput("text_sent_3"),
                   shiny::textOutput("text_sent_4"),
                   shiny::textOutput("text_sent_5"),
                   shiny::textOutput("text_sent_6"),
                   shiny::textOutput("text_sent_7")
               )
             ),
             
             fluidRow(
               tags$h3("Considering major historical events")),
             fluidRow(
               tags$h4("One might expect that historical events, especially those relevant to the American public lead to changes in demand of popular music in the form of happier or sadder Song Lyrics. The overall development if the lyrical emotion score is depicted below together with date indications of impactful events, which are outlined below."
                   )),
             fluidRow(width = 12,
               column(width = 9,
                      plotOutput("plot_sentiment_2") ),
               column(width = 3, tableOutput("table_hist")
                      )
               ),

               #             fluidRow(
#               tags$h5("1) May 1975: End of Vietnam War")),
#             fluidRow(
#               tags$h5("2) January 1981 - January 1993 - US Economic Crisis")),
#             fluidRow(
#                tags$h5("3) July 1988 - December 1991 - Dissolution of the Soviet Union")), 
#             fluidRow(
#               tags$h5("4) September 2001 - 9/11")),
 #            fluidRow(
#               tags$h5("5) March 2003 - Start of second Iraq War")),
#             fluidRow(
#               tags$h5("6) August 2007 - March 2009 US financial crisis")),
#             fluidRow(
#               tags$h5("7) March 2020 - First Wave of Covid Pandemic commences")),
#             fluidRow(
#               tags$h5("8) September 2021 - Second Wave of Covid Pandemic commences")),
             
             
             
             
             #fluidRow(
            #   tags$h3("Ratio of positive to negative expressions in Song Lyrics") ),
            # fluidRow(
            #   tags$h4("Apart from the mean sentiment score of the lyrics which also entails a degree of the positive or respectively negative connotation, one can determine to average ratio of positive to negative words across the genre and time dimension. The latter differs somewhat from one genre to another with e.g. rock-and-roll, reggae and latin song lyrics having a higher ratio than others (see the table below). These findings also conincide with those of the mean sentiment score from above. Still, all genres contain on average more more positive than negative words. This ratio does, however, fluctuate greatly per year and also decreases per time for most genres as is displayed in the slider graph below.")
             #),
             #fluidRow(width = 12,
            #          column(width = 4,
            #                 tableOutput("table_pos_neg_score")),
            #          column(width = 8,
            #                 plotly::plotlyOutput("plot_pos_neg_score")),
            # ),
            # fluidRow(
            #   tags$h3("Most common positive and negative Words in Lyrics") ),
            # fluidRow(
            #   tags$h4("Going further, the question remains what those words driving a positive or a negative sentiment score actually are. To shed some light into this issue, one can choose a Genre / Year Combination to see which words were the top five most common positively and negatively connoteted words for this subset of the data. ")
            # ),
            # fluidRow(width = 12,
            #          column( width = 6,
            #                  selectInput("word_pos_genre", "Select genres:", choices = df_lengths_genres_dates$genre, multiple = T, selected = c("pop", "rock", "folk"), selectize = TRUE, width = NULL, size = NULL),
#
#                      )  ,
 #                     column( width = 6,
#
#
#                              dateRangeInput("date_range_word", "Choose Date Range:",
 #                                            format = "yyyy",
 #                                            min = min(df_pos_words$Date),
  #                                           max   = max(df_pos_words$Date),
   #                                          start = "2010-01-01",
    #                                         end = "2020-01-01",
     #                                        startview = "year" ) ) ),
      #       fluidRow(width = 12,
       #               column(width = 6,
        #                     plotOutput("plot_pos_words")),
         #             column(width = 6,
          #                   plotOutput("plot_neg_words")) ),


             fluidRow(
               tags$h3("Relationship of sentiment with acoustic features")
             ),
             fluidRow(
               tags$h4("One might expect that the faster a song is or the more suitable it is for dancing, the better the sentiment ought to be. Take a look at the plots below displaying the correlation of the mean sentiment score and both the tempo and the danceability. No evidence of any correlation of the songs‘ sentiment scores and their respective Beats per Minute can be determined. Concerning the relationship of the sentiment and the Danceability, a slight negative correlation can be observed – which holds across almost all genres and is especially solid for the genres dance, jazz and rnb.")
             ),
             fluidRow(width = 12,
                      column(width = 4,
                             plotly::plotlyOutput("corr_sent_tempo")
                      ),
                      column(width = 4,
                             plotly::plotlyOutput("corr_sent_dance")
                      ),
                      column(width = 4,
                             tableOutput("corr_sent_table")
                      ) ),
             fluidRow(
               tags$h3("Jolly Christmas - Gloomy Autumn?")
             ),
             fluidRow(
               tags$h4("Lastly, one might ask if the sentiment score differs by month or by season, e.g. because of rather joyful Chart songs for Christmas or during the Summertime. A look at the Season graph shows that the Lyrics' implicit sentiment score of Charts in Wintertime is the highest, followed by Summer while Autumn and Spring almost share the third place. As the graphs beneath reveal, the Month with the highest assosiated average Sentiment Score of the Charts is in fact December. It seems as if the mean sentiment behaves cyclical with peaks in Winter and later Summer.")
             ),
             fluidRow(width = 12,
                      column(width = 6,
                             plotOutput("sent_season")
                      ),
                      column(width = 6,
                             plotOutput("sent_months") )
           )
           )
            ) ,
  
  tabPanel("Topic Modelling"),
  tabPanel("Pronoun Analysis",
           
           fluidPage(
             
             fluidRow(width = 12,
                      column(width = 6,
                             tags$h3("First Person - Singular"),
                             tags$h4("First Person - Singular - Explanation")),
                      column(width = 3,
                             tags$h3("First Person - Plural"),
                             tags$h4("First Person - Plural - Explanation")),
                      column(width = 3,
                             checkboxInput("aux_verbs_i", "Exclude Auxiliary Verbs", TRUE),
                             HTML("Auxiliary Verbs are words like <em>be</em>, <em>have</em>, <em>will</em> or <em>could</em>, that are often used but carry only limited meaning.")),
                      column(width = 6,
                             plotOutput("i_analysis"),
                             sliderInput("i_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                                         width = "100%", timeFormat = "%Y"
                                         #,animate = animationOptions(interval = 100)
                             )),
                      column(width = 6,
                             plotOutput("we_analysis"),
                             sliderInput("we_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                                         width = "100%", timeFormat = "%Y"
                                         #,animate = animationOptions(interval = 0.0001)
                             ))),
             
             fluidRow(width = 12,
                      column(width = 6,
                             tags$h3("Third Person - She"),
                             tags$h4("Third Person - She - Explanation"),
                             plotOutput("she_analysis"),
                             sliderInput("she_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                                         width = "100%", timeFormat = "%Y"
                                         #,animate = animationOptions(interval = 0.0001)
                             )),
                      column(width = 6,
                             tags$h3("Third Person - He"),
                             tags$h4("Third Person - He - Explanation"),
                             plotOutput("he_analysis"),
                             sliderInput("he_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                                         width = "100%", timeFormat = "%Y"
                                         #,animate = animationOptions(interval = 0.0001)
                             ))),
             
             fluidRow(width = 12,
                      tags$h3("Comparison Chart"),
                      tags$h4("Comparison Chart Explanation"),
                      column(width = 9,
                             plotOutput("pronoun_comparison_plot")),
                      column(width = 3,
                             selectInput("input_verb", "Select Verb:", choices = unique(he_data$verb), multiple = FALSE, selected = c("dance"), selectize = TRUE, width = NULL, size = NULL),
                             selectInput("pr_input_genre", "Select Genre:", choices = c("All", unique(he_data$genre)), multiple = FALSE, selected = c("All"), selectize = TRUE, width = NULL, size = NULL),
                             checkboxGroupInput("selected_pronoun", "Select Pronoun:",
                                                choices = c("She", "He", "We", "I"),
                                                selected = c("She", "He", "We", "I"))))
           
           ) ## closes the fluidPage
           ), ## closes tabPanel Prounoun Analysis
  
  tabPanel("Similarity Analysis",
           
           fluidPage(
   ### Similarity Analysis ###


fluidRow(
  tags$h3("Lyrical Similarity across features and time")
  ),
  fluidRow(
    tags$h4("Whether the lyrics of Char songs belonging to one genre or with a release date close together are more similar is an interesting point of analysis. Having captured the content of all song lyrics until January 2021 with Sentence Transformers and applying PCA, the lyrical similarities are displayed below. For more info on the approach, click the More Info Button below. For now, consider the lyrical embeddings across time. Apprently, songs with Chart appearance dates close to one another also seem to be share more lyrical content.")
    ),
  fluidRow(width = 12,
         column(width = 12,
                plotOutput("similarity_time")
         )
        ),
  fluidRow(
    tags$h4("Next, pick a few genres and compare the subscape embeddings of the encoded lyrics. A quick tip: Do not select all genres at once. It becomes apparant though, the the subspace embeddings of the lyrical embeddings are not closer for songs within one genre.")
  ),
  fluidRow(width=12,
         column(width=12,
            selectInput("genre_similarity_select", "Select genres:", choices = similarity_df$genre, multiple = T, selected = c("rock", "folk"), selectize = TRUE, width = NULL, size = NULL),
         )),
  fluidRow(width = 12,
         column(width = 12,
                plotOutput("similarity_genre")
         )
        ),


  actionButton("button_similarity_1", "Get more info "),
  hidden(
  div(id='text_div_2',
      shiny::textOutput("text_sim_1"),
      shiny::textOutput("text_sim_2"),
      shiny::textOutput("text_sim_3"),
      shiny::textOutput("text_sim_4"),
      shiny::textOutput("text_sim_5")
  )
),


fluidRow(
  tags$h3("Lyrical Recommendation Engine")
),

fluidRow(
  tags$h4("Also based on Sentence Transformer Embeddings, once can deduce the semantically closest texts for all chart songs. Search for a song by typing the Artist Name, then two blanc spaces and then the song title, e.g. 'Adele  Hello'. Some songs or artists may be called differently, so trying different styles of writing might make sense. In the case of succes, the songs with the most similar lyrics will be put out." 
) ),
fluidRow(width=12,
         column(width=12,
                selectInput("similarity_string", "Select genres:", choices = similarity_df$combination, multiple = F, selected = "Adele  Hello"),
                verbatimTextOutput("songs")
))
  
))

) ## end of ui

