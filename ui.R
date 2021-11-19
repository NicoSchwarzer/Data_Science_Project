
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
  
  
  ### Fourth Page ###
  tabPanel("4th Page")
  
  
  
)) ## Parantheses to close shinyUI and navbar design

















