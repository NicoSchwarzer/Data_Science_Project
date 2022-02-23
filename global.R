
#######################################################################

###### GLOBAL FILE

#######################################################################


## load packages
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
library(readr)
library(shinyalert)
library(wordcloud)
library(shinyWidgets)



## plot dataframes
df_sents_year <<- data.table::fread("df_sents_year.csv")
df_sents_genre_year <<- data.table::fread("df_sents_genre_year.csv")
df_historical <<- data.table::fread("df_historical.csv")
similarity_topics_df <<- readr::read_csv("similarity_topics_df.csv")
df_topics_genre_year <<- data.table::fread("df_topics_genre_year.csv")
df_topics_year <<- data.table::fread("df_topics_year.csv")
prep_genre <<- data.table::fread("prep_genre.csv")
co_occuring_df <<- data.table::fread("co_occuring_df.csv")
topics_sent <<- data.table::fread("topics_sent.csv")
corr_table2 <<- data.table::fread("corr_table2.csv")
df_uniques_sent <<- data.table::fread("df_uniques_sent.csv")




## choice dataframes
combination_choices <<- readr::read_csv("combinations_choices.csv")
quiz_band_choices <<- readr::read_csv("quiz_band_choices.csv")



