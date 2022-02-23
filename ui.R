
#######################################################################

###### UI FILE

#######################################################################


ui <- #tagList(
  navbarPage(
  
  shinyjs::useShinyjs(),
  
  # Title of whole Application
  title = "NLP - Natural Lyrics Processing",
  
  # make pages fluid
  fluid = TRUE,
  
  # optimize navbar for smaller displays
  collapsible = TRUE,
  
  # set other theme
  theme = shinythemes::shinytheme("flatly"),
  
  # favicon
  tags$head(tags$link(rel="shortcut icon", href="favicon2.ico")),
  
  # play with background colors
  #tags$head(tags$style(css)),
  
  setBackgroundColor(
    color = "#F7FBFF"),  #, "#2171B5"),    gradient = "linear",    direction = "bottom"
  
  
  
  
  
  
  #########################################
  ###### PAGE 1  - Introductory page ######
  #########################################
  
  tabPanel("Introductory Page",
           
           fixedPage(
             
             tags$h2("Overview of the Lyrics Analytics Project"),
             
             fixedRow(
               tags$h4("This R-Shiny Web-Application displays the result of a University Project of the M.Sc. Program Data Science in Business and Economics at Tuebingen University. 
                       It bundles all relevant steps of a Data Science Project - the last of which is an interactive display of the results residing here. The goal of the Project is to gather insights as to how Song Lyrics of very popular songs have evolved over time using NLP tools.
                       To this aim, we have scraped all Hot 100 Billboard charts from 1960 until today and applied several NLP-related methods. More information can be
                       found below", style="text-align: justify;"),
             ),
             
             fixedRow(
               tags$h4("Each week, a new Hot 100 Chart List is released. Thus, the scraping and all following steps are repeated once a week. While the table below shows 
                      the most important Meta-Data of this Project, a click on the Refresh-Button might cause the App to also incorporate the latest Billboard Chart Songs.", style="text-align: justify;")
             ),
             
             
             ## display overview dataframe
             fixedRow(
               #width = 12,
               column(9, align = "center", tableOutput("overview_table")),
               column(2, actionButton("refresh_data", "Refresh Data!",
                                      style = "color: #fff; background-color: #00009f ; border-color: #17251c",
                                      icon = icon("arrows-rotate", lib = "font-awesome")))
             ),
             fixedRow(
               tags$br(),
               tags$br()
             ),
             
             fixedRow(align="center",
                      column(width = 2,
                             tags$br()
                      ),
                      
                      column(width = 2,
                             tags$h4(". ", style = "color:#F7FBFF"),
                             actionButton(inputId = "explain_button",
                                          label = "Sketching our method",
                                          style = "color: #fff; background-color: #9f9fff ; border-color: #17251c",
                                          icon("info-circle")) ), 
                      column(width = 4,
                             align="center",
                             tags$h3("Developed By"),
                             tags$br(),
                             
                             
                             tags$h4("Leonard Berger"),
                             tags$a(href = "https://www.linkedin.com/in/leonard-berger/",  icon("linkedin"), target="_blank"),
                             tags$br(),
                             
                             tags$h4("Nico Schwarzer"),
                             tags$a(href = "https://www.linkedin.com/in/nico-schwarzer-931b3218a/",  icon("linkedin"), target="_blank"),
                             tags$br(),
                             
                             tags$h4("GitHub Repository"),
                             tags$a(href = "https://github.com/NicoSchwarzer/Data_Science_Project",  icon("github"), target="_blank"),
                             tags$br(),
                             tags$br(),
                             tags$br(),
                             
                             
                      ),
                      column(width = 2,
                             tags$h4(". ", style = "color:#F7FBFF"),
                             actionButton(inputId = "resources_button",
                                          label = "Used Resources",
                                          style = "color: #fff; background-color: #9f9fff ; border-color: #17251c",
                                          icon("info-circle")) ) 
             ),
             
             # tags$iframe(style="height:600px; width:100%", src="http://localhost/ressources/pdf/R-Intro.pdf"),
             
             
             
             
             
             #          fixedRow(
             #             tags$h4("             The goal of the Project is to gather insights as to how Song Lyrics of very popular songs have evolved over time using NLP tools.
             #                       Since To this aim, we have scraped all hot 100 Billboard charts from 1960 until today and used the Genuis API to retrieve the lyrics.
             #                       The Billboard Top 100 were choosen because of their availability in decades past. It is noteworthy, still, that they do not represent the music industry as a whole. Our findings thus apply to popular or economically spoken the demand of music (not its supply).
             #                       Also, we have gathered genre information with use of the Spotify API and assigned each genre to overarching genres by hand. The Spotify API was also used to obtain information on on each song's acoustic features.
             #                       The latter include Tempo (BPM), Duration (in seconds), Energy (perceptual measure of a song's liveliness) and Danceability (Suitability for dancing based on rythmic features). Genre Information was obtained from 
             #                       multiple APIs including the AudioDB-, the LastFM- and the Spotify API.Going further, we have trained and implemented a binary classifier to detect of the songs lyrics are in english and made use of the DeepL API if need be for translation.")
             #           )
             
           ) # closes fluidPage
           
           
           
  ), # closes tabPanel Intro Page
  
  
  
  #############################################
  ###### PAGE 2  - Length and Complexity ######
  #############################################

    tabPanel("Length and Complexity of Lyrics",

             fixedPage(
               tags$h1("Length of Lyrics"),

               fixedRow(
                 tags$h3("Development of Song Lyrics Length across Time")
               ),

               fixedRow(
                 tags$h4("When analyzing the length of songs lyrics across time, it becomes very much apparent that the average number of words (quite strikingly) almost doubled from 1960 until today.
                         This trend is observable across genres (with the exception of rock-n-roll). Quick tip: Do not choose too many genres at the same time.", style="text-align: justify;")
               ),

               fixedRow(
                 tags$h4("Going further, one might analyze if the increase in lyrics length is powered by a more frequent use of onomatopoetic words ('Uh', 'Ah', 'La-La').
                         These, alongside common stopwords of the english language, have been removed from the lyrics. Select genres to see the development across time and genre. To obtain more information, press the button above the plots.", style="text-align: justify;")
               ),

               ## Row with Input
               fixedRow(   #width = 12,
                        column(
                          width = 9,
                          tags$br(),
                          selectInput(inputId = "Genre_compl_1",
                                      label = "Select Genres:",
                                      choices = c("blues", "classical", "country", "dance", "folk",
                                                  "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"),
                                      multiple = TRUE,
                                      selected = c("pop", "rock", "dance"),
                                      selectize = TRUE,
                                      width = NULL,
                                      size = NULL)
                        ),
                        column(width = 3,
                               tags$h4(". ", style = "color:#F7FBFF"),
                               actionButton(inputId = "length_button",
                                            label = "More Info on the Approach",
                                            style = "color: #fff; background-color: #57a773; border-color: #17251c",
                                            icon("info-circle")) )
               ),

               fixedRow(
                 tags$h4("           ")
               ),
               ## First row of plots
               fixedRow(   #width = 12,
                        column(
                          width = 6,
                          # plotly::plotlyOutput("plot_complexity_1")
                          plotOutput("plot_complexity_1")
                        ),
                        column(
                          width = 6,
                          # plotly::plotlyOutput("plot_complexity_2")
                          plotOutput("plot_complexity_2")
                        )),

               ## Hidden more Information
               fixedRow(
                 tags$h4("  ")
               ),

      #         actionButton(inputId = "button_length_1",
      #                      label = "Show/Hide more Information"),
      #         hidden(
      #           div(id = "text_div",
      #               shiny::textOutput("text_length_1"))
      #         ),

               ## Accounting for changes in tempo and duration
               fixedRow(
                 tags$h3("Accounting for Changes in Acoustic Features")
               ),

               fixedRow(
                 tags$h4("One might guess that with increasing duration and with an increased number of beats per minute, the number of words rises as well.
                         Feel free to explore these potential driving forces for longer Lyrics further by selecting one of them in the dropdown item below.
                         Both a Scatterplot displaying the Relationship as well as the development of the respective Acoustic Features will be shown.
                         Can you identify one of the Acoustic Features as a reason for longer Lyrics? Also, feel free to hover over the Scatterplot and take
                         a look at which songs are displayed by the dots.", style="text-align: justify;")
               ),

               ## Select Input to chose a Relationship
               fixedRow(
  #               width = 12,
                 column(
                   width = 11,
                   selectInput(inputId = "Relation_select",
                               label = "Investigate potential Driving Force",
                               choices = c("Potential Driving Force: Duration", "Potential Driving Force: Tempo"),
                               multiple = F,
                               selected = "Potential Driving Force: Duration",
                               selectize = TRUE,
                               width = NULL,
                               size = NULL)
                 )),


               ## Next row of plots - based on inpit from above
               fixedRow(
                 #width = 12,
                 column(
                   width = 6,
                   plotly::plotlyOutput("relationship_1")
                   # plotOutput("relationship_1")
                 ),
                 column(
                   width = 6,
                   # plotly::plotlyOutput("relationship_2")
                   plotOutput("relationship_2")
                 ) ),


               #   fluidRow(
               #     width = 12,
               #     column(
               #       width = 4,
               #       # plotly::plotlyOutput("relationship_tempo")
               #       plotOutput("relationship_tempo")
               #     ),
               #     column(
               #       width = 4,
               #       # plotly::plotlyOutput("relationship_duration")
               #       plotOutput("relationship_duration")
               #     ),


               #    column(
               #         width = 4,
               #         tableOutput("correlations_table_compl1")
               #       )
               #     ),

               #   fluidRow(
               #     tags$h4("Given these insights, one might analyze how the BPM and the duration of songs evolved from 1960 to see if this might have caused the increase in words per song.
               #             The following plots, however, indicate that neither the average BPM, nore the average song duration rose overall.
               #             Still, it's noteworthy how the average duration rises until around 1992 (black dashed line) and falls off afterwards.
               #             On the other hand, the mean BPM shot up during that time period and thereby possibly mitigating the effect of shorter songs.")
               #   ),

               #   ## Next row of plots
               #   fluidRow(
               #     width = 12,
               #     column(
               #       width = 6,
               #       plotOutput("plot_bpm")
               #     ),
               #     column(
               #       width = 6,
               #       plotOutput("plot_duration")
               #     )
               #   ),


               ### Song Complexity ###

               fixedRow(
                 tags$h1("Complexity of Lyrics")
               ),

               fixedRow(
                 tags$h3("Repetitiveness in Lyircs")
               ),

               fixedRow(
                 tags$h4("A main point concerning lyrical complexity is the repetitiveness of words utilized. The fewer unique words as well as word stems used, the less complex a text can be deemed.
                         The number of different words and the number of different word stems per length are depicted below.", style="text-align: justify;")
               ),

               ## Input select Genres
               fixedRow(
                 #width = 12,
                 column(
                   width = 12,
                   selectInput(inputId = "Genre_compl_2",
                               label = "Select Genres:",
                               choices = c("blues", "classical", "country", "dance", "folk",
                                           "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"),
                               multiple = TRUE,
                               selected = c("dance", "pop", "rock"),
                               selectize = TRUE,
                               width = NULL,
                               size = NULL)
                 )),

               ## Next Row of Plots
               fixedRow(
                 #width = 12,
                 column(
                   width = 6,
                   # plotly::plotlyOutput("plot_complex_un_1")
                   plotOutput("plot_complex_un_1")
                 ),
                 column(
                   width = 6,
                   # plotly::plotlyOutput("plot_complex_un_1")
                   plotOutput("plot_complex_un_2")
                 )
               ),

               # fluidRow(
               #   tags$h3("Platzhalter. Plot Complex 1+2")
               # ),


               ## Adverbs
               fixedRow(
                 tags$h3("Tell me what you want? Tell me what you REALLY, REALLY want!")
               ),

               fixedRow(
                 tags$h4("Really wanting something is semantically more complex than only wanting something. Because of the use of adverbs - a common measurements for the elaborateness of a language.
                         As is exhibited below, the percentage of adverbs of all words lies around 1% with with no overall increase or decrease over the last 60 years, but a high fluctuation from one year to another.", style="text-align: justify;")
               ),

               ## Next Genre Input
               fixedRow(     #width=12,
                        column(
                          width=11,
                          selectInput("Genre_compl_3",
                                      "Select genres:",
                                      choices = c("blues", "classical", "country", "dance", "folk",
                                                  "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"),
                                      multiple = T,
                                      selected = c("dance", "pop", "rock"),
                                      selectize = TRUE,
                                      width = NULL,
                                      size = NULL),
                        )),

               ## Next Plots
               fixedRow(
                 #width = 12,
                 column(
                   width = 8,
                   # plotly::plotlyOutput("adverb_slider")
                   plotOutput("adverb_slider")
                 ),
                 column(
                   width = 4,
                   tableOutput("correlations_table_compl2")
                 )
               )

               # fluidRow(
               #   tags$h3("Platzhalter. Adverb Slider and Correlations Table")
               # )

             ) # closes fluidPage

    ), # closes tabPanel Length and Complexity


    ##########################################
    ###### PAGE 3  - Sentiment Analysis ######
    ##########################################

    tabPanel("Sentiment Analysis",

             fixedPage(

               fixedRow(
                 tags$h1("Development of General Sentiment across Time")
               ),

               fixedRow(
                 tags$h4("Texts, including song lyrics, carry a sentiment through their semantic structure.
                         The latter may thus be analysed across time and genre for the Hot 100 Charts of the past 60 years.
                         Note that this project focusses primarily on lyrics, no acoustic features are used for emotion detection here.
                         In a first step, the average sentiment score across time and genre dimensions is depicted below.
                         Quick tip: Do not choose too many genres at the same time. As can be seen, the average sentiment of the Hot 100 Song Lyrics has taken a slight fall during the last 60 years.
                         In case of taking an interest in further details and the sentiment algorithm, click on the info button above the graph.", style="text-align: justify;")
               ),

               fixedRow(
  #               width = 12,
                 column(
                   tags$br(),
                   width = 9,
                   selectInput(inputId = "Genre_sent_1",
                               label = "Select Genres:",
                               choices = c("blues", "classical", "country", "dance", "folk",
                                           "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"),
                               multiple = TRUE,
                               selected = c("dance", "pop", "rock"),
                               selectize = TRUE,
                               width = NULL,
                               size = NULL)
                 ),
                 column(width = 3,

                        tags$h4(". ", style = "color:#F7FBFF"),
                        actionButton(inputId = "sent_button",
                                     label = "More Info on the Approach",
                                     style = "color: #fff; background-color: #57a773; border-color: #17251c",
                                     icon("info-circle") ) )
               ),


               fluidRow(
                # width = 12,
                 column(
                   width = 12,
                   plotOutput("plot_sentiment_1")
                 )
               ),


               ## Historical Events
               fluidRow(
                 tags$h3("Considering major historical events")
               ),

               fluidRow(
                 tags$h4("One might expect that historical events, especially those relevant to the American public, lead to changes in demand of popular music in the form of happier or sadder Song Lyrics.
                         The overall development of the lyrical emotion score is depicted below together with date indications of impactful events.", style="text-align: justify;")
               ),

               fluidRow(
              #   width = 12,
                 column(
                   width = 9,
                   plotOutput("plot_sentiment_2")
                 ),
                 column(
                   width = 3,
                   tableOutput("table_hist")
                 )
               ),


               ## Sentiment with Accoustic Features
               fluidRow(
                 tags$h3("Relationship of sentiment with acoustic features")
               ),

               fluidRow(
                 tags$h4("It is reasonable to guess that the faster a song is or the more suitable it is for dancing, the better the  Sentiment conveyed by the Song's Lyrics ought to be.
                         Take a look at the plots and hover over the points below displaying the correlation of the mean sentiment score and both the tempo (left plot) and the danceability (right plot).
                         The table also shows the correlations on a by-genre basis.", style="text-align: justify;")
               ),

               fluidRow(
             #    width = 12,
                 column(
                   width = 4,
                   plotly::plotlyOutput("corr_sent_tempo")
                   # plotOutput("corr_sent_tempo")
                 ),
                 column(
                   width = 4,
                   plotly::plotlyOutput("corr_sent_dance")
                   # plotOutput("corr_sent_dance")
                 ),
                 column(
                   width = 4,
                   tableOutput("corr_sent_table")
                 )
               ),

               fluidRow(
                 tags$h3("Jolly Christmas - Gloomy Autumn?")
               ),

               fluidRow(
                 tags$h4("Lastly, one might ask if the sentiment score differs by month or by season, e.g. because of rather joyful Chart songs for Christmas or during the Summertime.
                         A look at the Season graph shows that the Lyrics average implicit sentiment both by Season (on the left) and by Month (on the right) and reveals only minor differences.", style="text-align: justify;")
               ),

               fluidRow(
             #    width = 12,
                 column(
                   width = 6,
                   plotOutput("sent_season")
                 ),
                 column(
                   width = 6,
                   plotOutput("sent_months"),
                   tags$br(),
                   tags$br(),
                   tags$br()
                 )
               )


             ) # closes fluidPage

    ), # closes tabPanel Sentiment Analysis


    #########################################
    ####### PAGE 4  - Topic Modelling #######
    #########################################

    tabPanel("Topic Modelling",
             fixedPage(

               fixedRow(
                 tags$h1("Analysing the Topics of Chart Songs across time and genre")
               ),

               fixedRow(
                 tags$h4("The topics used in the Billboard Chart songs is a vital point of analysis. However, deriving the latter is not straight-forward as topics for each song aren't provided and need to be inferred.
                         Based on an unsupervised Deep Learning method, five key topics were determined and the two semantically closest topics to the lyrical content were deduced per song. The Info-Button below contains a more detailed explanation of the chosen approach.
                         Below is an overview of both the development of the shares of the semantically closest topics to the lyrics across time and the co-occurences of the topics. The latter shows the shares of topics which were the second closest to the lyrics when the selected genre was the closest."
                         , style="text-align: justify;")
                 ),

               fixedRow(  #width=12,
                        column(width = 7,

                     tags$h4(". ", style = "color:#F7FBFF"),
                     actionButton(inputId = "topic_button",
                                  label = "More Info on the Approach",
                                  style = "color: #fff; background-color: #57a773; border-color: #17251c",
                                  icon("info-circle")) ) ,
                        column(width=5,
                               tags$br(),
                               selectInput("topic_select", "Select a Topic:",
                                           choices = c("Sadness and Critique", "Love and Romance", "Motivation and Ambitions",
                                                       "Affluence and Fame", "Feelgood, friends and Party"),
                                           multiple = F, selected = "Love and Romance", selectize = TRUE, width = NULL, size = NULL),
                        )  ),

               fixedRow(  #width=12,
                        tags$h4("        ")
               ),
               fixedRow(   #width = 12,

                        column(width = 7,
                               plotOutput("plot_topics_time")
                        ),
                        column(width = 5,
                               plotOutput("plot_co_topics")
                        )),

               fixedRow(
                 tags$h4("One can also differentiate by genre and analyse the share of main topics assosiated with each genre and the development of the shares across time. Select a genre below to and take a look at it's assosiated topics."
                         , style="text-align: justify;")),
               fixedRow(  #width=12,
                        column(width=12,
                               selectInput("topic_genre_select", "Select a Genre:",
                                           choices = c("blues", "classical", "country", "dance", "folk",
                                                       "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"),
                                           multiple = F, selected = "rock", selectize = TRUE, width = NULL, size = NULL),
                        )),
               fixedRow(  #width = 12,

                        column(width = 7,
                               plotOutput("plot_genre_topics_time")
                        ),
                        column(width = 5,
                               plotOutput("plot_genre_topics"),
                               tags$br(),
                               tags$br(),
                               tags$br()
                        )),


             ) # closes the fluid page
    ), # closes tabPanel Topic Modelling


    #########################################
    ###### PAGE 5  - Pronoun Analysis #######
    #########################################

    tabPanel("Pronoun Analysis",

             fixedPage(

               fixedRow(
                 tags$h1("Analysis of Actions attributed to Pronouns"),
                 tags$h4(HTML('Within the last decades, societal views on gender roles changed quite a bit.
                              This Analysis tries to examine whether these changes can also be spotted in the songs popular among certain time periods.
                              The actions associated with different genders are based on so-called <em>bi-grams</em>.
                              Verbs are extracted that often follow a specified pronoun, i.e. "he <em>cries</em>", "she <em>dances</em>", "I <em>love</em>".
                              Putting these verbs in a relative context of usage lets us have a look at which actions were associated with which person at a specific time point.'), style="text-align: justify;"),
                 tags$h4("      ")
               ),


               fixedRow(
                 #width = 12,
                 column(
                   width = 7,
                   tags$h3("Categorical Analysis"),
                   tags$h4("Looking at each verb on its own can become quite messy with a large set of datapoints.
                           Underneath, verbs are grouped into categories to distinguish certain actions more generally.", style="text-align: justify;")
                 ),
                 column(
                   width = 5,
                   tags$h4(". ", style = "color:#F7FBFF"),
                   tags$h4(". ", style = "color:#F7FBFF"),
                   actionButton(inputId = "pronoun_cats",
                                label = "Find out more about the categorization of verbs!",
                                style = "color: #fff; background-color: #57a773; border-color: #17251c",
                                icon("info-circle"))
                 )
               ),

               fixedRow(
                 #width = 12,
                 tags$h4(". ", style = "color:#F7FBFF"),
                 column(
                   width = 9,
                   tags$h3("Third Person Singular Analysis")
                 ),
                 column(
                   width = 3,
                   # tags$h4("Verb Classification Scheme"),
                   selectInput("verb_classification_scheme", "Select Verb Classification Scheme:",
                               choices = c("Authors' Categorization" = "authors_cat",
                                           "Levin's Categorization" = "levins_cat"),
                               selected = "authors_cat")
                 )
               ),

               fixedRow(
                # width = 12,
                 column(
                   width = 6,
                   plotOutput("she_categories"),
                   sliderInput("she_cat_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                               width = "100%", timeFormat = "%Y")
                 ),
                 column(
                   width = 6,
                   plotOutput("he_categories"),
                   sliderInput("he_cat_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                               width = "100%", timeFormat = "%Y")
                 )
               ),


               ##### hier noch ueberschrift fÃÂ¼r comparison chart

               fixedRow(
                 #width = 12,
                 tags$h4(". ", style = "color:#F7FBFF"),
                 tags$h4(HTML("Below, the different categories can be compared over time and genre.
                         For example, choose <em>Positive Emotions</em> as Author's Category and observe how the association with the female pronoun increased over time, especially for RnB-music!")),
                 column(width = 9,
                        plotOutput("pronoun_comparison_plot")
                 ),
                 column(width = 3,
                        selectInput("input_author_cat", "Select Authors' Category:",
                                    # choices = c("dance", "love", "hate"),
                                    # choices = verb_choices$x,
                                    # choices = verbs,
                                    choices = c("Thinking",	"Communication", "Positive Emotions",	"Negative Emotions", "Senses",
                                                "Possession",	"Negative Actions", "Positive Actions", "Positive Expectations", "Negative Expectations"),
                                    multiple = TRUE, selected = c("Communication"), selectize = TRUE, width = NULL, size = NULL),
                        selectInput("input_levin_cat", "Select Levin's Category:",
                                    # choices = c("dance", "love", "hate"),
                                    # choices = verb_choices$x,
                                    # choices = verbs,
                                    choices = c("Putting", "Removing", "Sending and Carrying", "Push and Pull", "Change of Possession", "Learning", "Holding and Keeping",
                                                "Concealing", "Throwing", "Contacting", "Poking", "Touching", "Cutting", "Attaching", "Separating", "Coloring",
                                                "Image Creation", "Transformation", "Engendering", "Calving", "Predicative Complements", "Perception", "Psychological Sate",
                                                "Desire", "Judgment", "Assessing", "Searching", "Social Interaction", "Communicating", "Animal Sounds", "Ingesting",
                                                "Physical Activities", "Grooming", "Killing", "Emission", "Destroying", "Change of State", "Lodging", "Existence",
                                                "Occurrence", "Body-Internal Motion", "Nonmovement", "Motion", "Avoiding", "Lingering/Rushing", "Measuring",
                                                "Beginning and Ending", "Holiday", "Weather"),
                                    multiple = TRUE, selected = NULL, selectize = TRUE, width = NULL, size = NULL),
                        selectInput("pr_input_genre", "Select Genre:", choices = c("All", "blues", "classical", "country", "dance", "folk",
                                                                                   "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"), multiple = FALSE, selected = c("All"), selectize = TRUE, width = NULL, size = NULL),
                        checkboxGroupInput("selected_pronoun", "Select Pronoun:",
                                           choices = c("She", "He"),
                                           selected = c("She", "He")))
               ),




               fixedRow(
                 #width = 12,
                 tags$h4(". ", style = "color:#F7FBFF"),
                 tags$h3("Breaking it down to Words"),
                 tags$h4("Categories are helpful to determine the overall picture.
                         However, if one is interested in specifc words attributed to the pronouns the corresponding charts can be found below.
                         Here, also the first person pronouns can be accessed, i.e. which actions the singer or band identifies themselve with.", style="text-align: justify;"),
                 tags$h4(". ", style = "color:#F7FBFF")
                 # column(
                 #   width = 3,
                 #   tags$h3("First Person - Singular"),
                 #   tags$h4("First Person - Singular - Explanation")
                 # ),
                 # column(
                 #   width = 3,
                 #   tags$h3("First Person - Plural"),
                 #   tags$h4("First Person - Plural - Explanation")
                 # ),
                 # column(
                 #   width = 3,
                 #   tags$h3("She"),
                 #   tags$h4("She - Explanation")
                 # ),
                 # column(
                 #   width = 3,
                 #   tags$h3("He"),
                 #   tags$h4("He - Explanation")
                 # )
               ),

               fixedRow(
                 #width = 12,
                 column(
                   width = 3,
                   plotOutput("i_analysis"),
                   sliderInput("i_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                               width = "100%", timeFormat = "%Y")
                 ),
                 column(
                   width = 3,
                   plotOutput("we_analysis"),
                   sliderInput("we_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                               width = "100%", timeFormat = "%Y")
                 ),
                 column(
                   width = 3,
                   plotOutput("she_analysis"),
                   sliderInput("she_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                               width = "100%", timeFormat = "%Y")
                 ),
                 column(
                   width = 3,
                   plotOutput("he_analysis"),
                   sliderInput("he_year", "Year", as.Date("1960-01-01"), as.Date("2021-01-01"), as.Date("1960-01-01"), step = 1,
                               width = "100%", timeFormat = "%Y")
                 )
               ),

               fixedRow(
                 #width = 12,
                 column(
                   width = 12,
                   checkboxInput("aux_verbs_i", "Exclude Auxiliary Verbs", TRUE),
                   HTML("Auxiliary Verbs are words like <em>be</em>, <em>have</em>, <em>will</em> or <em>could</em>, that are often used but carry only limited meaning.")
                 ),
                 tags$h4(". ", style = "color:#F7FBFF"),
               )



             ) # closes fluidPage

    ), # closes tabPanel Prounoun Analysis


  ###########################################
  ###### PAGE 6  - Similarity Analysis ######
  ###########################################

  tabPanel("Similarity Analysis",

           fixedPage(

             ### Similarity Analysis ###


             fixedRow(
               tags$h1("Lyrical Similarity Engine")
             ),

             fixedRow(
               tags$h4("Whether the lyrics of Chart Songs belonging to one genre or with a close release Date are more similar is an interesting point of analysis. Having captured the content of all song lyrics until January 2021 with Sentence Transformers, one can infer the most similar song lyrics per chosen song. Search for a song by typing the Artist Name, then two blanc spaces and then the song title, e.g. `Adele  Hello` to see the three songs with the most similar lyrics to the latter song. Some songs or artists may be called differently, so trying different styles of spelling might make sense.
                       In the case of succes, the songs with the most similar lyrics will be put out. A more techniqucal point of view is presented below and an exlanation is provided when clicking the info-button. For now, check out Songs with the most similar Lyrics to your favourite songs. In case you need a refresher on the Lyrics, simply chose the song and let them be printed out."
                       , style="text-align: justify;")),
             fixedRow(   #width=12,
                      column(width=2,
                            tags$br(),
                             # selectInput("similarity_string", "Select a Song:",
                             #             choices = combination_choices$x,
                             #             multiple = F, selected = "Adele  Hello")
                             selectizeInput("similarity_string", "Select a Song:",
                                            choices = NULL,
                                            multiple = F, selected = "Adele  Hello")  
                            ),
                      column(width = 7,
                             tags$br(),
                             htmlOutput("songs_1")
                             ),
                      column(
                        width = 3,
                        tags$h4(". ", style = "color:#F7FBFF"),
                        actionButton(inputId = "sim_button",
                                     label = "More Info on the Approach",
                                     style = "color: #fff; background-color: #57a773; border-color: #17251c",
                                     icon("info-circle"))
                      )

                      ),
             fixedRow(  #width=12,
                      column(width=2,
                             selectInput("song_lyrics", "Print the Lyrics per Song",
                                         choices = c("Lyrics of Choosen Song", "Most Similar Lyrics",
                                                     "2nd most Similar Lyrics", "3rd most Similar Lyrics"),
                                         multiple = F, selected = NULL )  ) ,
                      column(width = 10,
                             htmlOutput("songs_2")
                      )
             ),


             tags$br(),



             fixedRow(
               tags$h3("Lyrical Similarity across features and time")
             ),
             fixedRow(
               tags$h4("To answer the question if songs belonging to one gerne or with a Chart Entry date close to one another have similar lyrics, one can compare the above mentioned embeddings across these two dimensions. With embeddings of reduced dimensions, the latter problem can be assessed visually.
                       Below, you can choose a dimensionality reduction technique and then inspect the results the Mapping of the  Embedings to two dimensions. Would you argue that the Lyrics od Songs with a similar Chart Entry Date or within one Genre are more similar?"
                       , style="text-align: justify;") ) ,

             tags$br(),

             fixedRow(
               column(width = 2,
                      tags$h4("   ")
                      ),
               column(width = 8, align="center",
                radioButtons("dim_red", "Pick a Dimensionality Reduction Technique", choices = c("Principal Component Analysis", "t-SNE", "Isomap", "UMAP"), selected = "Principal Component Analysis", inline = TRUE)
                      )
             ),

             tags$br(),

             fixedRow(  #width = 12,
                      column(width = 12,
                             plotOutput("similarity_time")
                      )
             ),

             tags$br(),

             fixedRow(
               tags$h4("Next, pick a few genres and compare the 2-dimensional representation of the encoded lyrics across the genre dimension. A quick tip: Do not too many genres at once. Once again, one might try to answer the question if the Lyrics of Songs within one genre are more related to one another than to songs of other genres." , style="text-align: justify;")
             ),
             fixedRow( # width=12,
                      column(width=12,
                             selectInput("genre_similarity_select", "Select genres:",
                                         choices = c("All", "blues", "classical", "country", "dance", "folk",
                                                     "jazz", "latin", "pop", "reggae", "rnb", "rock", "rock-and-roll"),
                                         multiple = T, selected = c("rock", "folk"), selectize = TRUE, width = NULL, size = NULL),
                      )),
             fixedRow(  #width = 12,
                      column(width = 12,
                             plotOutput("similarity_genre"),
                             tags$br(),
                             tags$br(),
                             tags$br()
                      )
             ),


           )

  ), # closes tabPanel Similarity Analysis


  #########################################
  ############ PAGE 7  - Quiz #############
  #########################################

  tabPanel("Quiz",

           fixedPage(

             fixedRow(
               #width = 12,

               tags$h1("Can you guess the Song?"),

               tags$h4(HTML("How good do you know the chart songs of the last years?
                       Beneath, the most frequent words of a song are displayed in a wordcloud.
                       This means that words that occur more often in the song appear bigger and closer in the center.
                       On the left, four options are displayed as possible answers.
                       At first use press <em>Get a song!</em> - go ahead and give it a try!"), style="text-align: justify;")

             ),

             fixedRow(
               #width = 12,

               column(
                 width = 2,
                 actionButton(inputId = "start_quiz",
                              label = "Get a Song!"),
                 tags$h3("    "),
                 radioButtons(inputId = "inRadioButtons2",
                              label = "Select a song!",
                              choices = c("On first use click 'Get a song'!"),
                              # choices = c("1", "2", "3", "4"),
                              selected = character(0)),
                 tags$h3("    "),
                 actionButton(inputId = "sent_guess",
                              label = "Take your Shot!")
               ),


               column(
                 width = 5,
                 # wordcloud2Output("quiz_wordcloud")
                 plotOutput("quiz_wordcloud")  # , width = "auto"
                 # verbatimTextOutput("output_lyrics")
                 # textOutput("output_lyrics_2")
                 # plotOutput("quiz_wordcloud")
               ),

               column(
                 width = 5,
                 tags$h5("Biggest Beatles fan alive? Only Justin Bieber Poster above your bed?
                       Check how well you know the songs of your favorite artist!
                       Here you can filter by some of the top artists from the charts for the past year.", style="text-align: justify;"),
                 selectInput(inputId = "quiz_band_input",
                             label = "Choose your Artist!",
                             choices = c("All", quiz_band_choices$artist),
                             # choices = c("All", "The Beatles"),
                             selected = "All",
                             multiple = FALSE),
                 tags$h5("Born in the wrong decade? Or in the right one?
                       Here you can adjust the date range to wherever your taste of music has fallen into!
                       Note: When an artist is selected the date range does not apply anymore.", style="text-align: justify;"),
                 dateRangeInput(inputId = "date_range_quiz",
                                label = "Select Date Range for Chart Songs!",
                                start = "2020-01-01", end = lubridate::today(),
                                min = "1960-01-01", max = lubridate::today(),
                                format = "dd-mm-yyyy", separator = "    to    "),
                 tags$h5("Filter the songs by chart ranks - choose to display only number one hits, the top10 or all 100 chart places!", style="text-align: justify;"),
                 #sliderInput(inputId = "quiz_rank_filter",
                 #           label = "Choose the Difficulty!",
                 #           value = 100, min = 1, max = 100, step = 1),
                 numericInput(inputId = "quiz_rank_filter",
                              label = "Choose the Difficulty!",
                              value = 100, min = 1, max = 100),
                 # verbatimTextOutput("test"),
                 tags$h5("Songwriters can be lazy when naming their song - the title is often choosing after the most prominent expressions in the text.
                       If you think this is too easy and you want to exclude the words of the title from the wordcloud go ahead and increase the difficulty!", style="text-align: justify;"),
                 shinyWidgets::prettyCheckbox(inputId = "quiz_exclude_title",
                                              label = "Exclude Song Title"),
                 tags$br(),
                 tags$br(),
                 tags$br()
               )

             )


           ) # closes tabPanel Quiz

  ), # closes fluidPage
  
  
  # tags$head(
  #   tags$style(HTML(
  #     "html {
  #            position: relative;
  #            min-height: 100%;
  #          }
  #          body {
  #            margin-bottom: 60px; /* Margin bottom by footer height */
  #          }
  #          .footer {
  #            position: absolute;
  #            bottom: 0;
  #            width: 100%;
  #            height: 50px; /* Set the fixed height of the footer here */
  #            background-color: #f5f5f5;
  #          }"))),
  # 
  #   tags$footer("2021: Natural Lyrics Processing",
  #               tags$a(href = "https://github.com/NicoSchwarzer/Data_Science_Project",  icon("github"), target="_blank"),
  #               class = "footer")
  
) ## end of ui and tagslist

