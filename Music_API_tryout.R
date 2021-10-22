##############################################
## DS Project Idea - Analysing Chart Lyrics ##
##############################################


rm(list = ls())

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")

library(jsonlite)
library(httr)
library(rlist)
library(tidyverse)
library(naniar)


################################
## Acquisition of Song Lyrics ##
################################


### Genius API for Song Lyrics
if (!require("geniusr")) install.packages("geniusr")
if (!require("tidytext")) install.packages("tidytext")


library(geniusr)
library(dplyr)
library(tidytext)



# API Data not needed here  

#client_id <- iwCmSttu7FB3DHc8FD2vsAQaFLd-gFNtTqFdtsjn19klywm9zkIdwp5kBcRwg6q5
#secret <- KN0TFaJ6HZ0obvdCGR6xzx_Lu9HF9nfRt9ZVPN5Jc3cnwwfR5Sq26GobU7KX0kmBiz-_k7In0y8wAADrWqRsbg


# Logging in for API use 

token_genius <- "iZdkkCTGhwiZKyjrW6NjTaWhAML-6clc2yg2o77_BCn8CPcEyly423GM77Y3_KZk"



genius_token(TRUE)  # WICHTIG: dann den token in der console eingeben!



## given example 

# Song ID
thingCalledLove <- search_genius(search_term = "I Believe in a Thing Called Love")
id <- thingCalledLove$content[[1]]$id

# Lyrics
thingCalledLoveLyrics <- get_lyrics_id(song_id = id)
a <- thingCalledLoveLyrics$line
paste(unlist(t(a)), collapse = " ")


## example with current charts

# ssong id 
butter <- search_genius(search_term = "BTS Butter")
id <- butter_id <- butter$content[[1]]$id

# lyrics from song id 
butter_lyrics <- get_lyrics_id(song_id = id)
a <- butter_lyrics$line
paste(unlist(t(a)), collapse = " ")


# Optimal  :D



################################
### Aquisition of chart data ###
################################


### Using Kaggle DF


https://www.kaggle.com/danield2255/data-on-songs-from-billboard-19992019



### Web Scraping integrated 

# 2021 

xml <- read_html("https://en.wikipedia.org/wiki/List_of_Billboard_Hot_100_number_ones_of_2021")





