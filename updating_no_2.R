###############################
## Weekly iterative updating ##
###############################



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
library(xml2)
library(rvest)
library(stringr)

## This file is for the weekly updating the DF contain songs, artists, lyrics, genres and acoustic features 
## It does so by the use of a repeat statement with a Sys.sleep() command at the end to run continuously but only
## execute the updating weekly. Thus, this file HAS TO be run remotely!
## The library commands, the meta-info and the functions thus only need to be run once (outside of repeat command)


## The Set-Up of this file is as follows: ##

# 1) Defining Meta-Data for Web Scraping and API Usage 
# 2) Calling relevant functions to make subsequent steps more efficient and easier to read
# 3)  Initialising the repeat loop 
#  3.1) loading in dataframe with all genres and dataframe with reduced genres
#   3.2) Getting last date from billboard site 
#   3.3) Creating vector of all "missed dates", i.e. Saturdays of weeks that have not been added to thre overall charts DF.
#   3.4) If the DF is not up to date 
#     3.4.1) Scrape billboard site and retrieve artist and song info
#     3.4.2) Getting genres for unseen songs via Spotify API (copying genres for seen songs)
#     3.4.3) Getting acoustic features for unseen songs via Spotify API (copying features for seen songs)
#     3.4.4) Getting lyrics for unseen songs via Genius API (copying lyrics for seen songs)
#     3.4.5) Applying lyrics modelling 
#     3.4.6) Append dataframe with original lyrics and dataframe with reduced lyrics 
# 3.3) Letting the script sleep for one week




################################
## Meta-Info for Web Scraping ##
################################

base_url <- "https://www.billboard.com/charts/hot-100/"

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


## function to  get song lyrics from artist song title combination
source("functions_get_lyrics.R")


getting_new_artists <- function(df) {
  
  ## This function splits the artist column by commonly used stopwrods for artists and yields two new columns for the artists,  which are added to the DF ##
  ## The DF must contain a artists - column
  
  df$artists_1 <- df$artists
  df$artists_2 <- df$artists
  
  stopwords <- c("and", "And",  "AND", "With", "with", "WITH", "feat", "feat.", "featuring", "Featuring", "&", " (", "(")
  
  for (i in 1:nrow(df)) {
    
    #  i <- 46
    splitted_artist <- str_split(df$artists[i], " ")[[1]]
    
    
    if (sum(stopwords %in% splitted_artist) > 0) {
      
      word_here <- which(stopwords %in% splitted_artist)
      #new artist 
      df$artists_1[i] <- str_split(df$artists[i],  stopwords[word_here] )[[1]][1]
      df$artists_2[i] <- str_split(df$artists[i],  stopwords[word_here] )[[1]][2]
    }
  }
  
  return(df)
}




## function to get acoustic features by keyword

get_acoustic_features <- function(keyword, c1) {
  # keyword has to be string
  
  if (keyword == c1) { # case that artist has not been split 
    
    result_general <- spotifyr::search_spotify(keyword)
    track_id <- result_general$tracks$items$id[1][1]
    
    if (is.null(track_id) == TRUE) { # ensuring consistent data format
      danceability <- NaN
      energy <- NaN
      loudness <- NaN
      tempo <- NaN
      duration <- NaN
      
    } else { # ig it not null 
      
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
    
  } else { # case that artist has been split 
    
    result_general <- spotifyr::search_spotify(keyword)
    track_id <- result_general$tracks$items$id[1][1]
    
    if (is.null(track_id) == TRUE) { # ensuring consistent data format
      danceability <- NaN
      energy <- NaN
      loudness <- NaN
      tempo <- NaN
      duration <- NaN
      
    } else { # id it not null 
      
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
  }
  
  
  all_features <- c(danceability,energy,loudness, tempo,duration)
  
  return(all_features)
}    


## function to get genre 

get_genre_from_combination <- function(c1, c2, c3, a1, a2, a3) {
  
  # c1-c3 have to be string - and are supposed to be the three combination variables 
  # a1-a3 have to be string - and are supposed to be the three artists variables 
  
  
  ## checking if the artist variable had been split ## 
  
  if (c1 == c2) { # case that artist has not been split 
    
    result_general <- spotifyr::search_spotify(c1)
    track_id <- result_general$tracks$items$id[1][1]
    
    if (is.null(track_id) == TRUE) { # now by artist
      artist <- spotifyr::search_spotify(c1)
      a <- artist$artists$items$genres[[1]][1]
    } else {
      artist <- spotifyr::get_track(track_id)$artists$id[1][[1]]
      a <- spotifyr::get_artist(artist)$genres[1]
    }
  } else {  # case that artist has been split 
    
    result_general <- spotifyr::search_spotify(c2)
    track_id <- result_general$tracks$items$id[1][1]
    
    if (is.null(track_id) == TRUE) { # now by artist
      artist <- spotifyr::search_spotify(c2)
      a <- artist$artists$items$genres[[1]][1]
    } else {
      artist <- spotifyr::get_track(track_id)$artists$id[1][[1]]
      a <- spotifyr::get_artist(artist)$genres[1]
    }
    
    # checking if first alternative worked 
    if (is.null(a[[1]]) == TRUE) {
      artist <- spotifyr::search_spotify(c2)
      a <- artist$artists$items$genres[[1]][1]
    } else {
      artist <- spotifyr::get_track(track_id)$artists$id[1][[1]]
      a <- spotifyr::get_artist(artist)$genres[1]
    }
    
  }
  # final check 
  if (is.null(a[[1]]) == TRUE & is.null(artist) == TRUE  ) {
    genre <- "to be classified"
  } else if (is.null(a[[1]]) == TRUE & is.null(artist) == FALSE  ) {
    genre <- "not known"
  } else {
    genre <- a
  }
  
  return(genre)
}


## function to map the many genres to overarching genres

matching_genres <- function(df) {
  
  # function needs a df with column "genre" as input. 
  # Also the matching excel file must be read in 
  
  genres_mapping <- readxl::read_excel("unique_genres.xlsx")
  genres_mapping <- genres_mapping[,c("original_genre", "new_genre")]
  
  for (i in 1:nrow(df)) {
    
    genre_here <- df$genre[i]
    
    # in case the genre is unknown
    if (  (genre_here %in% genres_mapping$original_genre) == T ){
      ge <-  genres_mapping$new_genre[genres_mapping$original_genre == genre_here]
      df$genre[i] <- ge
      
      # in all other cases:     
    } else {
      
      df$genre[i] <- "unknown genre"
      
    }
  }
  
  return(df)
}


# function to generate a new 100 row DF with artists, songs, dates

new_data_df <- function(base_url, date) {
  
  # get current URL
  url <- paste0(base_url, date)
  
  # get website content
  hot100 <- xml2::read_html(url)
  
  # write website content as txt file
  xml2::write_xml(rvest::html_node(hot100, 'body'), "hot100.txt")
  
  # read in again as csv file 
  hot100csv <- read.delim("hot100.txt", sep = "|", header = FALSE)
  
  # extract artists
  artists <- hot100csv[which(hot100csv$V1 == "</h3>")+2,]
  
  # extract songs
  songs <- hot100csv[which(hot100csv$V1 == "</h3>")-1,]
  
  # put them together as dataframe
  help_df <- data.frame(songs, artists)
  
  # remove uneccessary rows and columns
  start_idx <- which(help_df[,1] == "Additional Awards")
  help_df <- help_df[(start_idx+1):(start_idx+100), c(1,4)]
  
  so <- as.character(help_df$V1)
  ar <- as.character(help_df$V1.1)
  
  df_new <- data.frame(ar, so)
  names(df_new) <- c("artists", "songs")
  df_new$dates <- date
  
  df_new$artists
  
  # mapping &amp; to &
  df_new$artists <- stringr::str_replace(df_new$artists, "\\&amp;", "&")
  
  rm(url, hot100,hot100csv, artists,songs, start_idx, help_df, so, ar)
  
  return(df_new)
}


## function to retrieve last date that should be online 
last_date_online <- function(input) {
  
  ## inout needs to be  as.Date(Sys.time())
  today <- input
  dow <- weekdays(today)
  
  # if weekday is tuesday until friday -> expect new genres to expect next saturday
  # else - go back to last/current saturday 
  
  if (dow == "Monday") {
    last_date_on <- today - 2
  } else if (dow == "Tuesday") {
    last_date_on <- today + 4
  } else if (dow == "Wednesday") {
    last_date_on <- today + 3
  } else if (dow == "Thursday") {
    last_date_on <- today + 2
  } else if (dow == "Friday") {
    last_date_on <- today + 1
  } else if (dow == "Saturday") {
    last_date_on <- today
  } else if (dow == "Sunday") {
    last_date_on <- today - 1 
  }
  
  rm(dow)
  rm(today)
  
  return(last_date_on)
  
}

###########################
## Reading in overall DF ## 
###########################

# with original genres

base_data_raw <- read.csv("base_data_raw.csv", stringsAsFactors=F)
base_data_raw <- base_data_raw[, c("dates","chart_rank","songs","artists","lyrics","combination","artists_1" ,"artists_2","combination_1","combination_2","genre", "danceability" ,"energy","loadness","tempo","duration")]

# getting correct data types
base_data_raw$dates <- as.Date(base_data_raw$dates)


# with reduced genres
base_data_cleaned <- read.csv("base_data_cleaned.csv", stringsAsFactors=F)
base_data_cleaned <- base_data_cleaned[, c("dates","chart_rank","songs","artists","lyrics","combination","artists_1" ,"artists_2","combination_1","combination_2","genre", "danceability" ,"energy","loadness","tempo","duration")]

# getting correct data types
base_data_cleaned$dates <- as.Date(base_data_cleaned$dates)

# most recent date from overall DF - here only the DF of the original genre is needed 
last_date_all_df <- base_data_raw$dates[nrow(base_data_raw)]

# getting last available online date
last_date_on <- last_date_online(as.Date(Sys.time()))


difftime_general <- difftime(last_date_on, last_date_all_df, units = "days")

if ( length(substr( difftime_general, start = 1, stop = 1)) == 2 ) {
  difftime_cleaned <- strtoi(substr( difftime_general, start = 1, stop = 2))
} else {
  difftime_cleaned <- strtoi(substr( difftime_general, start = 1, stop = 2))
}


##################################################################
## Creating vector for all dates that are not yet in overall DF ##
##################################################################

vec_missed_dates <- c(last_date_all_df)     # vector of dates (Saturdays that need to be added) in right data format
vec_missed_dates <- vec_missed_dates[-1]

num_weeks_missed <- difftime_cleaned / 7

# appending vec_missed_dates to include all sturdays for which there should be lyrics 

for (i in 1:num_weeks_missed){
  vec_missed_dates <- c(vec_missed_dates, (last_date_all_df + 7*i))
}


## Getting new data ## 

# create empty data frame to store results
df <- data.frame(matrix(nrow = 100*length(vec_missed_dates),
                        ncol = 3))

colnames(df) <- c("artists","songs", "dates")

# specify column types
df$artists <- as.character(df$artists)
df$songs <- as.character(df$songs)
df$dates <- as.Date(df$dates)

a <- 1

for (i in 1:length(vec_missed_dates)) {
  
  d_new <- new_data_df(base_url, vec_missed_dates[1])
  
  df$artists[a:(a+99)] <- as.character(d_new$artists)
  df$songs[a:(a+99)] <- as.character(d_new$songs)
  df$dates[a:(a+99)] <- as.character(d_new$dates)
  
  a <- a + 100
}


## adding charts placement 
df$chart_rank <-  rep(c(1:100), length(vec_missed_dates))


### Splitting artist and getting new combination ###

df$combination <- paste0(df$artists,"  ", df$songs)   

df <- getting_new_artists(df)

# Coding variable of combination of artist and song title - also for the alternative representation of the artists
df$combination_1 <- paste0(df$artists_1,"  ", df$songs)   
df$combination_2 <- paste0(df$artists_2,"  ", df$songs)   



########################################################################
## Dividing into new songs (not in all previos charts) and seen songs ##
########################################################################

# combinations with A match in already scraped combinations
df_all_new_data_billboard_seen <- df[df$combination %in% (df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED$combination) ,]


# combinations with NO match in already scraped combinations
df_all_new_data_billboard_unseen <- df[!df$combination %in% (df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED$combination) ,]

df_all_new_data_billboard_unseen$artists <- as.character(df_all_new_data_billboard_unseen$artists)
df_all_new_data_billboard_unseen$songs <- as.character(df_all_new_data_billboard_unseen$songs)


##################################
## Getting data for new songs ####
##################################

## getting the respective genres ## 

df_all_new_data_billboard_unseen$genre <- "c"

max_iter <- nrow(df_all_new_data_billboard_unseen)

for (i in 1:max_iter) {
  
  # getting correct genre  
  df_all_new_data_billboard_unseen$genre[i] <- get_genre_from_combination(df_all_new_data_billboard_unseen$combination[i], df_all_new_data_billboard_unseen$combination_1[i], df_all_new_data_billboard_unseen$combination_2[i], df_all_new_data_billboard_unseen$artists[i], df_all_new_data_billboard_unseen$artists_1[i], df_all_new_data_billboard_unseen$artists_2[i])
  
  if (( i %% 100) == 0) {
    Sys.sleep(40)
  }  
  
}


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
  acoustic_features <- get_acoustic_features(df_all_new_data_billboard_unseen$combination[i], df_all_new_data_billboard_unseen$combination_1[i])
  
  # assigning fitting value of output vector 
  df_all_new_data_billboard_unseen$danceability[i] <- acoustic_features[1]
  df_all_new_data_billboard_unseen$energy[i] <- acoustic_features[2]
  df_all_new_data_billboard_unseen$loadness[i] <- acoustic_features[3]
  df_all_new_data_billboard_unseen$tempo[i] <- acoustic_features[4]
  df_all_new_data_billboard_unseen$duration[i] <- acoustic_features[5]
  
  if (( i %% 200) == 0) {
    Sys.sleep(25)
  }  
  
}



## Getting Lyrics ####
# NEEDS function_get_lyrics file!


## add lyrics column
df_all_new_data_billboard_unseen$lyrics <- as.character(NA) 

for (i in 1:nrow(df_all_new_data_billboard_unseen)){
  
  # make artist vector
  artist_vector <- get_strcombinations(df_all_new_data_billboard_unseen$artists[i])
  
  # make song vector
  song_vector <- get_strcombinations(df_all_new_data_billboard_unseen$songs[i])
  
  # get lyrics
  l <- get_lyrics(artist_vector = artist_vector,
                  song_vector = song_vector,
                  repeats = 5, print_comb = FALSE)
  
  
  # fill in lyrics
  df_all_new_data_billboard_unseen$lyrics[i] <- l
  
  success <- sum(!is.na(df_all_new_data_billboard_unseen$lyrics))
  if(i%%10 == 0){
    print(paste0(i, "     Success Count: ", success))
  }
  
}



########## TO BE ADDED ###########################

# HIER KOMMT TRANSLATION HIN





##############################
## Taking care of seen data ## 
##############################

df_all_new_data_billboard_seen  <- dplyr::left_join(df_all_new_data_billboard_seen, base_data_raw[, !names(base_data_raw) %in% c('dates', 'chart_rank', 'artists', 'songs', 'artists_1', 'artists_2', 'combination_1', 'combination_2')], by = "combination")

rm(gg)


###########################################################
## Adding both seen and unseen data frames back together ##
###########################################################  

df_new_with_features <- rbind(df_all_new_data_billboard_seen, df_all_new_data_billboard_unseen)

rm(df_all_new_data_billboard_seen)
rm(df_all_new_data_billboard_unseen)

## Re-mapping the genres for the new DF
df_new_with_features_reduced <- matching_genres(df_new_with_features)


#########################
## Adding to large DF  ##
#########################

### for DF with original genres ###

# appending large DF


base_data_raw <- dplyr::bind_rows(base_data_raw, df_new_with_features)


# appending DF with reduced genres 
base_data_cleaned <- dplyr::bind_rows(base_data_cleaned, df_new_with_features)

rm(df_new_with_features)


## saving results ##

write.csv(base_data_raw, "base_data_raw.csv")

write.csv(base_data_cleaned, "base_data_cleaned.csv")



## removing 



## sourcing 




