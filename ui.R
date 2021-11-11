#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(

    # Application title
    title = "Leonard's shiny WebApp",

    tabPanel("1 Week",
             plotOutput("oneWeek")),
    tabPanel("1 Year",
             plotOutput("oneYear")),
    collapsible = FALSE)

       
)
