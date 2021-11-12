##################
## Updating DF  ##
##################

## Setting WD 
setwd("C:\\Users\\Nico\\Documents\\Uni\\3. Sem\\DS Projekt\\Code_and_Data")

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")
if (!require("foreach")) install.packages("foreach")
if (!require('spotifyr')) install.packages('spotifyr')
if (!require("rvest")) install.packages("rvest")
if (!require("lubridate")) install.packages("lubridate")


library(spotifyr)
library(jsonlite)
library(httr)
library(rlist)
library(tidyverse)
library(naniar)
library(geniusr)
library(foreach)
library(lubridate)
library(rvest)


## This file is for iteratively/weekly updating the DF contain songs, artists, lyrics, genres and acoustic features 


################################
## Meta-Info for Web Scraping ##
################################


base_url <-  "https://www.billboard.com/charts/hot-100"

x_path_data <- '//*[contains(concat( " ", @class, " " ), concat( " ", "text--truncate", " " ))]'
x_path_date <- '//*[contains(concat( " ", @class, " " ), concat( " ", "button--link", " " ))]'


#############################
## Meta-Data for API Usage ##
#############################

### Spotify API

Sys.setenv(SPOTIFY_CLIENT_ID = "a9fc91d2beb445c5869d0ea04496f606")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "384c5bf72d3d4e85ac5b906258cadd9a")
get_spotify_access_token()

### Genius API

Sys.setenv(GENIUS_API_TOKEN = "iZdkkCTGhwiZKyjrW6NjTaWhAML-6clc2yg2o77_BCn8CPcEyly423GM77Y3_KZk")
genius_token()


########################
## Relevant functions ##
########################


## function to retrieve last n chars from string 

substr_from_right <- function(string, num){
  substr(string, nchar(string)-num+1, nchar(string))
}


## function for getting last available date on Billboard   ## 
## important - this needs to be a saturday (is by default) ##

get_last_date <- function(url, x_path_date) {
  
  result_1 <- rvest::read_html(url)
  
  xml_nodes_1 <- html_elements(result_1, xpath= x_path_date)
  
  html_text <- rvest::html_text(xml_nodes_1)
  
  stopping = 25
  
  date <- substr(html_text, start = 18, stop = stopping)
  century_check <- substr(substr_from_right(date, 4), start = 1, stop = 2)
  
  ## since the date strings have different strings, this ensrues that all chars belonging to the actual date are retrieved
  
  while (century_check != "20") {
    stopping = stopping + 1
    date <- substr(html_text, start = 18, stop = stopping)
    century_check <- substr(substr_from_right(date, 4), start = 1, stop = 2)
  }
  
  
  # in correct day format
  date_format <- lubridate::mdy(date)
  
  return(date_format)
}




## function for scraping and returning list of chart data  ##
## the url must contain a date info, which shall be passed ##

get_charts_list <- function(url, path_1) {
  # function to scrape based URL and x_path using rvest package 
  
  result_1 <- rvest::read_html(url)
  
  xml_nodes_1 <- html_elements(result_1, xpath= path_1)
  
  # retrieving relevant text with charts 1-100 
  charts <- list(rvest::html_text(xml_nodes_1))[[1]][13:212]
  
  return(charts)
}



## function for creating a DF from scraped data ##
## input must be output of get_charts_list      ## 

get_table_from_scraped <- function(scraped_text) {
  
  # building a table from the html text
  
  songs <- rep("a", length(scraped_text)*0.5) # instantiating /w fitting data type, i.e. string
  artists <- rep("a", length(scraped_text)*0.5) # instantiating /w fitting data type, i.e. string
  
  s = 1 # to use as counter for songs
  a = 1 # to use as counter for artists
  
  for (i in 1:length(scraped_text)) {
    
    if (i%%2 != 0) {   # song if index %% 2 != 0
      (songs[s] <- scraped_text[i])
      (s = s+ 1) 
    } else { # song if index %% 1 == 0
      (artists[a] <- scraped_text[i])
      (a = a + 1) 
    }
  }
  
  all_info <- tibble(songs, artists)
  
  return(all_info)
  
}



## function to  get song lyrics from artist song title combination

get_lyrics_from_combination <- function(keyword) {
  # keyword has to be string
  # using genius API
  
  song_info <- geniusr::search_genius(search_term = keyword)
  
  if (length(song_info$content) == 0) {
    lyrics <- "NaN"
  } else {
    songId <- song_info$content[[1]]$id
    
    # lyrics from song id 
    lyrics_a <- geniusr::get_lyrics_id(song_id = songId)
    
    if ( nrow(lyrics_a) == 0) { # assing possible cause of error
      lyrics <- "Nan"
    } else {
      # post-processing
      lyrics_b <- lyrics_a$line
      lyrics <- paste(unlist(t(lyrics_b)), collapse = " ")
    }
  }
  return(lyrics)
}


## sometimes, only "" is returned. Thus, using this function is better!

get_lyrics_from_combination_safely <- function(keyword) {
  repeat {
    lyrics <- get_lyrics_from_combination(keyword)
    if (lyrics != ""){
      break
    }
  }
  return(lyrics)
}


## function to get acoustic features by keyword

get_acoustic_features <- function(keyword) {
  # keyword has to be string
  
  
  result_general <- spotifyr::search_spotify(keyword)
  track_id <- result_general$tracks$items$id[1][1]
  
  
  if (is.null(track_id) == TRUE) { # ensuring consistent data format
    danceability <- NaN
    energy <- NaN
    loudness <- NaN
    tempo <- NaN
    duration <- NaN
    
  } else {
    
    
    # getting further audio features
    audio_feautres <- spotifyr::get_track_audio_features(track_id)
    
    if (nrow(audio_feautres) != 0) {  # error control 
      
      danceability <-   audio_feautres$danceability[1]
      energy <- audio_feautres$energy
      loudness <- audio_feautres$loudness
      tempo <- audio_feautres$tempo
      duration <- audio_feautres$duration_ms[1] / 1000
      
    } else {
      danceability <- NaN
      energy <- NaN
      loudness <- NaN
      tempo <- NaN
      duration <- NaN
    }
  }
  
  all_features <- c(danceability,energy,loudness, tempo,duration)
  
  return(all_features)
  
}    


## function to get genre by keyword, i.e. artist song combination as a string


get_genre__from_combination <- function(keyword) {
  # keyword has to be string
  
  result_general <- spotifyr::search_spotify(keyword)
  track_id <- result_general$tracks$items$id[1][1]
  
  if (is.null(track_id) == TRUE) { # ensuring consistent data format
    genre <- "NAN"
  } else {
    artist_id <- spotifyr::get_track(track_id)$artists$id[1][[1]]
    a <- spotifyr::get_artist(artist_id)$genres[1]
  }
  if (is.null(a[[1]]) == TRUE) {  # ensuring consistent data format
    genre <- "Nan"
  } else {
    genre <- a   
  }
  return(genre)
  
}



###########################
## Reading in overall DF ## 
###########################

df_all_billboard_all_weeks_with_genre_lyrics <- read.csv("df_all_billboard_all_weeks_with_genre_lyrics.csv")
df_all_billboard_all_weeks_with_genre_lyrics <- df_all_billboard_all_weeks_with_genre_lyrics[, c("artists", "songs", "dates", "combination", "genre", "lyrics", "danceability", "energy", "loadness", "tempo", "duration")]

# making sure right datetime format is set 
df_all_billboard_all_weeks_with_genre_lyrics$dates <- as.Date(df_all_billboard_all_weeks_with_genre_lyrics$dates)


# most recent date from overall DF
last_date_all_df <- df_all_billboard_all_weeks_with_genre_lyrics$dates[nrow(df_all_billboard_all_weeks_with_genre_lyrics)]



###################################################################
## Getting last date from the overall DF and from billboard site ## 
###################################################################


# the last date (Saturday) from Billboard 
last_date_bb <- get_last_date(base_url, x_path_date)

# date difference
difftime_general <- difftime(last_date_bb, last_date_all_df, units = "days")
difftime_cleaned <- strtoi(substr( difftime_general, start = 1, stop = 1))



##################################################################
## Creating vector for all dates that are not yet in overall DF ##
##################################################################

vec_missed_dates <- c(last_date_bb)     # vector of dates (Saturdays that need to be added) in right data format
vec_missed_dates <- vec_missed_dates[-1]

num_weeks_missed <- difftime_cleaned / 7


### here start a large if statement: If no missed data is avilable for scraping, then the rest of this script need not be executed!

if (num_weeks_missed != 0)  {

  for (i in 0:(num_weeks_missed-1)) {
    
      
    ## appending the vector 
    missed_date <- last_date_bb - 7*i
    vec_missed_dates <- c(vec_missed_dates, missed_date)
    }
  
    #######################################
    ## Scraping charts for missing weeks ##
    #######################################
    
    
    len <- length(vec_missed_dates)
    i <- 1
    
    # initializing empty vectors 
    songs <- c()
    artists <- c()
    dates <- c(last_date_bb)[-1] # ensuring correct data type
    
    while (i <= len) {
      
        # creating fitting url 
        date_char <- as.character(vec_missed_dates[1])
        url_final <- paste(base_url, "/", date_char, sep="")
        
        # getting data 
        scraped_data <- get_charts_list(url_final, x_path_data)
        
        # cleaning data 
        cleaned_data <- get_table_from_scraped(scraped_data)
        
        # appending vectors 
        songs <- c(songs, cleaned_data$songs)
        artists <- c(artists, cleaned_data$artists)
        
        dates_here <- rep(vec_missed_dates[i], nrow(cleaned_data))
        dates <- c(dates, dates_here)
        
        i = i + 1
      }  
      
  
    
    ## creating a DF from scraped Data 
    df_all_new_data_billboard <- data.frame(artists, songs, dates)
    
    df_all_new_data_billboard$combination <- paste0(df_all_new_data_billboard$artists,"  ",  df_all_new_data_billboard$songs)   # combination ensures  only unique matches
    
    
    ########################################################################
    ## Dividing into new songs (not in all previos charts) and seen songs ##
    ########################################################################
    
    # combinations with A match in already scraped combinations
    df_all_new_data_billboard_seen <- df_all_new_data_billboard[df_all_new_data_billboard$combination %in% (df_all_billboard_all_weeks_with_genre_lyrics$combination) ,]
    
    # combinations with NO match in already scraped combinations
    df_all_new_data_billboard_unseen <- df_all_new_data_billboard[!df_all_new_data_billboard$combination %in% (df_all_billboard_all_weeks_with_genre_lyrics$combination) ,]


    ##################################
    ## Getting data for new songs ####
    ##################################
    
    ## getting the respective genres ## 
      
    df_all_new_data_billboard_unseen$genre <- "a"
    
    max_iter <- nrow(df_all_new_data_billboard_unseen)
       
    for (i in 1:max_iter) {
        
      df_all_new_data_billboard_unseen$genre[i] <- get_genre_from_combination(df_all_new_data_billboard_unseen$combination[i][[1]][1])
    }
    
    # unnesting 
    df_all_new_data_billboard_unseen <- unnest(df_all_new_data_billboard_unseen, cols = genre)
    
    ## getting the respective acoustic features ##  
  
    # defining new column with correct data type 
    df_all_new_data_billboard_unseen$danceability <- 0
    df_all_new_data_billboard_unseen$danceability <- 0
    df_all_new_data_billboard_unseen$energy <- 0
    df_all_new_data_billboard_unseen$loadness <- -1.2
    df_all_new_data_billboard_unseen$tempo <- 0
    df_all_new_data_billboard_unseen$duration <- 0
    
    
    # calling max_iter again since the nesting already removed NANs
    max_iter <- nrow(df_all_new_data_billboard_unseen)
    
    for (i in 1:max_iter) {
      
      # getting correct acoustic features
      acoustic_features <- get_acoustic_features(df_all_new_data_billboard_unseen$combination[i])
      
      # assigning fitting value of output vector 
      df_all_new_data_billboard_unseen$danceability[i] <- acoustic_features[1]
      df_all_new_data_billboard_unseen$energy[i] <- acoustic_features[2]
      df_all_new_data_billboard_unseen$loadness[i] <- acoustic_features[3]
      df_all_new_data_billboard_unseen$tempo[i] <- acoustic_features[4]
      df_all_new_data_billboard_unseen$duration[i] <- acoustic_features[5]
      
    }
    
    
    ## getting respective lyrics ## 
    
    df_all_new_data_billboard_unseen$lyrics <- "a"
    
    for (i in 1:max_iter) {
      
      # getting correct genre  
      df_all_new_data_billboard_unseen$lyrics[i] <- get_lyrics_from_combination_safely(df_all_new_data_billboard_unseen$combination[i])
    }
    
    # all new data with NANs removed
    
    df_all_new_data_billboard_unseen <-  na.omit(df_all_new_data_billboard_unseen)
    
  
    #################################
    ## Getting data for seen songs ##
    #################################
    
    max_iter <- nrow(df_all_new_data_billboard_seen)
    
    for (i in 1:max_iter) {
      
      # genre
      df_all_new_data_billboard_seen$genre[i] <- df_all_billboard_all_weeks_with_genre_lyrics$genre[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      
      # acoustic features 
      df_all_new_data_billboard_seen$danceability[i] <- df_all_billboard_all_weeks_with_genre_lyrics$danceability[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      df_all_new_data_billboard_seen$energy[i] <- df_all_billboard_all_weeks_with_genre_lyrics$energy[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      df_all_new_data_billboard_seen$loadness[i] <- df_all_billboard_all_weeks_with_genre_lyrics$loadness[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      df_all_new_data_billboard_seen$tempo[i] <- df_all_billboard_all_weeks_with_genre_lyrics$tempo[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      df_all_new_data_billboard_seen$duration[i] <- df_all_billboard_all_weeks_with_genre_lyrics$duration[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      
      
      # lyrics 
      df_all_new_data_billboard_seen$lyrics[i] <- df_all_billboard_all_weeks_with_genre_lyrics$lyrics[df_all_billboard_all_weeks_with_genre_lyrics$combination == df_all_new_data_billboard_seen$combination[i] ]
      
    }
    
  
    ###########################################################
    ## Adding both seen and unseen data frames back together ##
    ###########################################################  
  
    df_new_with_features <- rbind(df_all_new_data_billboard_unseen, df_all_new_data_billboard_seen)
    

    #########################################
    ## Adding to large DF and to unique DF ##
    #########################################
    

    df_all_billboard_all_weeks_with_genre_lyrics <- dplyr::bind_rows(df_all_billboard_all_weeks_with_genre_lyrics, df_new_with_features)
    

    ## saving results ##
    
    write.csv(df_all_billboard_all_weeks_with_genre_lyrics,"df_all_billboard_all_weeks_with_genre_lyrics.csv")
    
    
  }   # end of if-statement! 



