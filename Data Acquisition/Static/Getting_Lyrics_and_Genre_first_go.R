#########################################
## Getting Lyrics and Genre  first go  ##
#########################################

## Note that this file is old. 
# The use of the LastFM and AudioDB APIs for the Genres was applied later and resides in th files
# LastFM_Genres.R and AudioDB_Genres.R
# Also, the lyrics function is old. The approach that leads to our DF resides
# in get_lyrics.R!


## please make sure that the file all_data_billboard_weeks.csv resides here!


### This file serves to obtain lyrics and genre for the Billboard Top 100 for every week
### The latter were scraped in the file "Web_Scraping_Billboard_first_go". 
### The Spotify API is used for genre, the genius API is used for lyrics.
### Thus, this file also only needs to be executed once! Another file is used for updating


if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")
if (!require("foreach")) install.packages("foreach")
#install.packages('spotifyr')

library(spotifyr)
library(jsonlite)
library(httr)
library(rlist)
library(tidyverse)
library(naniar)
library(geniusr)
library(foreach)


### Set-Up of this script ### 

## 1) Defining meta-data for API Usage 
## 1) Relevant function 
## 2) Loading in data from scraping and some manipulations
## 3) Selecting only unique songs
## 4) Getting the genre with the genius API
## 5) Getting the lyrics with the genius API
## 6) Re-merging the lyrics and the genres for the unique songs to all chart songs



#############################
## Meta-Data for API Usage ##
#############################

### Spotify API

Sys.setenv(SPOTIFY_CLIENT_ID = "XX")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "XX")
get_spotify_access_token()

### Genius API

Sys.setenv(GENIUS_API_TOKEN = "XX")
genius_token()




########################
## Relevant functions ##
########################


getting_new_artists <- function(df) {
  
  ## This function splits the artist column by commonly used stopwrods for artists and yields two new columns for the artists,  which are added to the DF ##
  ## The DF must contain a artists - column
  
  df$artists_1 <- df$artists
  df$artists_2 <- df$artists
  
  stopwords <- c("and", "And",  "AND", "With", "with", "WITH", "feat", "feat.", "featuring", "Featuring", "&")
  
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
    if ( length(genre_here) == 0 ) {
      df$genre[i] <- "unknown genre"
      
      # in all other cases:     
    } else {
      
      matched_genre <- genres_mapping$new_genre[i]
      df$genre[i] <- matched_genre
      
    }
  }
  
  return(df)
}




############################################################################
### Loading in DF containing artists and songs from all billboard charts ###
############################################################################


# reading in data  
df_all_billboard_weeks = read.csv("all_data_billboard_weeks.csv", stringsAsFactors = F)

df_all_billboard_weeks <- df_all_billboard_weeks[,c("artists","songs","dates")]

# displaying head
#head(df_all_billboard_weeks, 10)


# Coding variable of combination of artist and song title - also for the alternative representation of the artists
df_all_billboard_weeks$combination <- paste0(df_all_billboard_weeks$artists,"  ", df_all_billboard_weeks$songs)   


#################################
## Selecting only unique songs ##
#################################


## to do this, remove duplicates for time saving and merge back together later! Also dropping 
## date column since this is different for same songs!

df_all_billboard_weeks_no_date <- df_all_billboard_weeks[,c("artists", "songs", "combination")]

df_all_billboard_weeks_unique <- df_all_billboard_weeks_no_date  %>%
  distinct(combination, .keep_all = TRUE)

#nrow(df_all_billboard_weeks_no_date) # 322800
#nrow(df_all_billboard_weeks_unique) # 28369 -> less than a 1/10 


# Getting alternative representation of the artists 
df_all_billboard_weeks_unique <- getting_new_artists(df_all_billboard_weeks_unique)


# Coding variable of combination of artist and song title - also for the alternative representation of the artists
df_all_billboard_weeks_unique$combination_1 <- paste0(df_all_billboard_weeks_unique$artists_1,"  ", df_all_billboard_weeks_unique$songs)   
df_all_billboard_weeks_unique$combination_2 <- paste0(df_all_billboard_weeks_unique$artists_2,"  ", df_all_billboard_weeks_unique$songs)   



###################################################################
##  Getting the genre and acoustic features with the Spotify API ##
###################################################################

# renaming DF
df_all_billboard_weeks_unique_with_genre <- df_all_billboard_weeks_unique

## getting the genres for the unique DF


#df_all_billboard_weeks_unique_with_genre$genre <- lapply(df_all_billboard_weeks_unique_with_genre$combination, FUN = get_genre_from_combination)


# defining new column with correct data type 
df_all_billboard_weeks_unique_with_genre$genre <- "a"

max_iter <- nrow(df_all_billboard_weeks_unique_with_genre)




for (i in 16455:max_iter) {
  
  # getting correct genre  
  df_all_billboard_weeks_unique_with_genre$genre[i] <- get_genre_from_combination(df_all_billboard_weeks_unique_with_genre$combination[i], df_all_billboard_weeks_unique_with_genre$combination_1[i], df_all_billboard_weeks_unique_with_genre$combination_2[i], df_all_billboard_weeks_unique_with_genre$artists[i], df_all_billboard_weeks_unique_with_genre$artists_1[i], df_all_billboard_weeks_unique_with_genre$artists_2[i])
  
  # overview for error control
  print(i)
  print(df_all_billboard_weeks_unique_with_genre$genre[i])
  
  if (( i %% 100) == 0) {
    Sys.sleep(30)
  }  
  
}



# saving intermediate result 
write.csv(df_all_billboard_weeks_unique_with_genre,"df_all_billboard_weeks_unique_with_genre_no_lyrics.csv")


## getting acoustic features for unique DF

# defining new column with correct data type 
df_all_billboard_weeks_unique_with_genre$danceability <- 0
df_all_billboard_weeks_unique_with_genre$danceability <- 0
df_all_billboard_weeks_unique_with_genre$energy <- 0
df_all_billboard_weeks_unique_with_genre$loadness <- 0
df_all_billboard_weeks_unique_with_genre$tempo <- 0
df_all_billboard_weeks_unique_with_genre$duration <- 0


for (i in 1:max_iter) {
  
  # getting correct acoustic features
  acoustic_features <- get_acoustic_features(df_all_billboard_weeks_unique_with_genre$combination[i])
  
  # assigning fitting value of output vector 
  df_all_billboard_weeks_unique_with_genre$danceability[i] <- acoustic_features[1]
  df_all_billboard_weeks_unique_with_genre$energy[i] <- acoustic_features[2]
  df_all_billboard_weeks_unique_with_genre$loadness[i] <- acoustic_features[3]
  df_all_billboard_weeks_unique_with_genre$tempo[i] <- acoustic_features[4]
  df_all_billboard_weeks_unique_with_genre$duration[i] <- acoustic_features[5]
  
  # overview for error control
  print(i)
  print(df_all_billboard_weeks_unique_with_genre$danceability[i])
  
}


# saving intermediate result 
write.csv(df_all_billboard_weeks_unique_with_genre_no_lyrics,"df_all_billboard_weeks_unique_with_genre_no_lyrics.csv")



#############################################
##  Getting the lyrics with the genius API ##
#############################################



# perhaps reloading the dataframe containing the genres 
#df_all_billboard_weeks_unique_with_genre_no_lyrics <- read.csv("df_all_billboard_weeks_unique_with_genre_no_lyrics.csv",  stringsAsFactors = FALSE)
#df_all_billboard_weeks_unique_with_genre_no_lyrics <- df_all_billboard_weeks_unique_with_genre_no_lyrics[,c("artists","songs","combination", "genre")]


# renaming data frame
df_all_billboard_weeks_unique_with_genre <- df_all_billboard_weeks_unique_with_genre


## getting lyrics data 

#df_all_billboard_weeks_unique_with_genre_lyrics$lyrics <- lapply(df_all_billboard_weeks_unique_with_genre_lyrics$combination, FUN = get_lyrics_from_combination_safely)


# defining new column with correct data type 
df_all_billboard_weeks_unique_with_genre_lyrics$lyrics <- "a"


for (i in 1:max_iter) {
  
  # getting correct genre  
  df_all_billboard_weeks_unique_with_genre_lyrics$lyrics[i] <- get_lyrics_from_combination_safely(df_all_billboard_weeks_unique_with_genre_lyrics$combination[i])
  
  # overview for error control
  print(i)
  print(df_all_billboard_weeks_unique_with_genre_lyrics$lyrics[i])
  
}



## restriction of lyrics - nicht "last updated
## zu kurz (nur 10 character)
## sehr viele | oder - oder \ 




# saving DF with genre and lyrics
write.csv(df_all_billboard_weeks_unique_with_genre_lyrics,"df_all_billboard_weeks_unique_with_genre_lyrics.csv")



### re-merging to the DF containing all charts (not just unique ones) ### 

#df_all_billboard_weeks_unique_with_genre_lyrics_no_na <- na.omit(df_all_billboard_weeks_unique_with_genre_lyrics)
##

df_all_billboard_weeks_unique_with_genre_lyrics_no_na <- df_all_billboard_weeks_unique_with_genre_lyrics_no_na[df_all_billboard_weeks_unique_with_genre_lyrics_no_na$genre != "NAN",]
df_all_billboard_weeks_unique_with_genre_lyrics_no_na <- df_all_billboard_weeks_unique_with_genre_lyrics_no_na[df_all_billboard_weeks_unique_with_genre_lyrics_no_na$genre != "Nan",]
##
df_all_billboard_weeks_unique_with_genre_lyrics_no_na <- df_all_billboard_weeks_unique_with_genre_lyrics_no_na[df_all_billboard_weeks_unique_with_genre_lyrics_no_na$lyrics != "NAN",]
df_all_billboard_weeks_unique_with_genre_lyrics_no_na <- df_all_billboard_weeks_unique_with_genre_lyrics_no_na[df_all_billboard_weeks_unique_with_genre_lyrics_no_na$lyrics != "Nan",]


all_data_billboard_weeks <- read.csv("all_data_billboard_weeks.csv")


df_all_billboard_all_weeks_with_genre_lyrics <- merge(x=all_data_billboard_weeks,y=df_all_billboard_weeks_unique_with_genre_lyrics_no_na,by=c("artists", "songs"),all.x=FALSE, all.y=FALSE)

# ordering by date 
df_all_billboard_all_weeks_with_genre_lyrics <- df_all_billboard_all_weeks_with_genre_lyrics %>%
  dplyr::arrange(dates)


## saving results ##

## unique songs (no nans)

#write.csv(df_all_billboard_weeks_unique_with_genre_lyrics_no_na,"df_all_billboard_weeks_unique_with_genre_lyrics_no_na.csv")
# 10987 uniques 


## all songs (no nans)

# the DF shall be saved as "df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED" to indicate that NO cleaning has been done until now 


write.csv(df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED, "base_data_raw.csv")


## applying the genre mapping and saving 

df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED <- matching_genres(df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED)

write.csv(df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED,"base_data_cleaned.csv")









