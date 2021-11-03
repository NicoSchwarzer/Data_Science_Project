####################################
## Getting Genre from Spotify API ##
####################################

#rm(list = ls())

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")
if (!require("foreach")) install.packages("foreach")
#if (!require("doMC")) install.packages("doMC")


#install.packages('spotifyr')

library(spotifyr)
library(jsonlite)
library(httr)
library(rlist)
library(tidyverse)
library(naniar)
library(geniusr)
library(foreach)
#library(doMC)


Sys.setenv(SPOTIFY_CLIENT_ID = "a9fc91d2beb445c5869d0ea04496f606")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "384c5bf72d3d4e85ac5b906258cadd9a")

get_spotify_access_token()


###############
## functions ##
###############

### NAN-wrapper function to avoid code interruptions 

nan_wrapper <- function(element) {
  if (is.null(element) == TRUE) {
    (output <- "NaN") 
  } else {
    output <- element
  }
  return(output)
}



### searching by keyword - sollte funktionieren! ###

# function to get genre by keyword, i.e. artist song combination as a string
get_genre_from_combination <- function(keyword) {
   # keyword has to be string
   
   result_general <- spotifyr::search_spotify(keyword)
   track_id <- result_general$tracks$items$id[1][1]
   
   if (is.null(track_id) == TRUE) {
     genre <- "NAN"
   } else {
   artist_id <- spotifyr::get_track(track_id)$artists$id[1][[1]]
   genre <- spotifyr::get_artist(artist_id)$genres[1]
   }
   return(genre)

}


# quick check

#get_genre_from_combination("Aha take on me")
#get_genre_from_combination("Farruko Pepas")
#get_genre_from_combination("Aha Take on me")
#get_genre_from_combination("Kenning West, Alder")



### function for searching by Artists ###
get_genre_from_artist <- function(artist) {
  # artist has to be string
  
  artist_id <-spotifyr::get_artist_audio_features(artist)$artist_id[1]
  genre <- spotifyr::get_artist(artist_id)$genres[1]
  
  return(genre)

}

# quick check
#get_genre_from_artist("Farruko")
#get_genre_from_artist("David Guetta")
#get_genre_from_artist("Whitney Houston")



###################################################
## combining with genius API for lyrics querying ##
###################################################


Sys.setenv(GENIUS_API_TOKEN = "iZdkkCTGhwiZKyjrW6NjTaWhAML-6clc2yg2o77_BCn8CPcEyly423GM77Y3_KZk")
genius_token()



# function to  get song lyrics from artist song title combination

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
  
  # post-processing
  lyrics_b <- lyrics_a$line
  lyrics <- paste(unlist(t(lyrics_b)), collapse = " ")
  }
  return(lyrics)
    
}



#get_lyrics_from_combination("LadyGaga Pokerface")
#get_lyrics_from_combination("Farruko  Pepas")
#get_lyrics_from_combination("Nelly Furtado  Maneater")
#get_lyrics_from_combination()

## sometimes, only "" is returned. Thus, using the latter is better!


get_lyrics_from_combination_safely <- function(keyword) {
  repeat {
    lyrics <- get_lyrics_from_combination(keyword)
    if (lyrics != ""){
      break
    }
  }
  return(lyrics)
}


get_lyrics_from_combination_safely("Wizkid Essence")  # nice :D


### getting song title & artist ### 

get_title_artist <- function(keyword) {
  # keyword has to be string !
  # using genius API
  
    song_info <- geniusr::search_genius(search_term = keyword)
    
    if (length(song_info$content) == 0) {
      
      all_info <- c("NaN", "NaN")
      
    } else {
    
    artist <- song_info$content[[1]]$primary_artist$name # for artist
    title <- song_info$content[[1]]$title # for title
    
    all_info <- list(artist, title)
    }
  return(all_info)
}




#aa <- get_title_artist("Wizkid Essence")
#aa[[1]]
#aa[[2]]


### NAN-wrapper function to avoid code interruptions 

nan_wrapper <- function(element) {
  if (is.null(element) == TRUE) {
    (output <- "NaN") 
  } else {
    output <- element
  }
  return(output)
}


### Filling an exemplary tibble ###

get_info_tibble <- function(songs) {

  # create tibble
  tibble_1 <- tibble(
    artist = character(),
    song = character(),
    lyrics = character(),
    genre = character(),
  )

  # filling it by using functions 
  
  for (i in 1:length(songs) ) {
    
    
    artist_title <- get_title_artist(songs[i])

    
    tibble_1[i,1] <- artist_title[[1]]
    tibble_1[i,2] <- artist_title[[2]]
    
    tibble_1[i,3] <- get_lyrics_from_combination_safely(songs[i])
    
    
    tibble_1[i,4] <- nan_wrapper(get_genre_from_combination(songs[i])[[1]])
      }  
  
  return(tibble_1)
  
}




#  one without lyrics, one with no genre
songs_1 <- c("Ludwig van Beethoven Für Elise",  "Alesso Years", "The Killers Mr. Brightside", "Kygo Born to be yours", "Eros Ramazotti Piu Bella Cosa", "Kenning West Alder", "Cher Believe",  "Avicii Levels", "Billie Eilish No Time to Die",  "Luis Fonsi Despacito", "Alan Walker Faded", "Farruko  Pepas", "Nelly Furtado  Maneater")

so <- "Alesso Years"

get_info_tibble(so)


