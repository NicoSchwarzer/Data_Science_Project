
### UI File for the Shiny Web-Application ###


# load packages
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)


### Set up the whole menu ###

shinyUI(navbarPage(
  
  # Title of whole Application
  title = "From the Beatles to the Killers - A Chronology",
  
  # make pages fluid
  fluid = TRUE,
  
  # optimize navbar for smaller displays
  collapsible = TRUE,
  
  # set other theme
  theme = shinytheme("flatly"),
  
  # play with background colors
  #tags$head(tags$style(css)),
  
  
  
  
  ### First Page ###
  tabPanel("Moods over time",
           ### Style for the first page: Sidebar
           sidebarLayout(
             ## What is displayed on the sidebar
             sidebarPanel(
               # Checkbox to select topics
               checkboxGroupInput(inputId = "topics_displayed",
                                  label = "Topics to display",
                                  choices = c("love", "anger", "wanderlust", "money", "christmas", "happy"))
             ),
             
             ## What is displyed in the main panel
             mainPanel(
               plotOutput("topics_plot")
             )
             
           )), ## Parentheses to close first page
  
  ### Second Page ###
  tabPanel("2nd Page",
           ### Style for the first page: Sidebar
           sidebarLayout(
             ## What is displayed on the sidebar
             sidebarPanel(
               # Checkbox to select topics
               checkboxGroupInput(inputId = "topics_displayed2",
                                  label = "Topics to display",
                                  choices = c("love", "anger", "wanderlust", "money", "christmas", "happy"))
             ),
             
             ## What is displyed in the main panel
             mainPanel(
               plotOutput("topics_plot2")
             )
             
           )),  ## Parentheses to close second page
  
  ### Third Page ###
  tabPanel("3rd Page",
           ### Style for third page: Just put everything there
           verticalLayout(
             plotOutput("test_plot"),
             
             actionButton("refresh_data", "Refresh Data!")
           )),
  
  
## complexity panel ## 
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
  
  
  
)) ## Parantheses to close shinyUI and navbar design

















