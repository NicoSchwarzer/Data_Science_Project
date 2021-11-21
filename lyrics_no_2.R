
library(spotifyr)
library(jsonlite)
library(httr)
library(rlist)
library(tidyverse)
library(naniar)
library(geniusr)
library(foreach)



#########################


### Spotify API

Sys.setenv(SPOTIFY_CLIENT_ID = "a9fc91d2beb445c5869d0ea04496f606")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "384c5bf72d3d4e85ac5b906258cadd9a")
get_spotify_access_token()

### Genius API

Sys.setenv(GENIUS_API_TOKEN = "iZdkkCTGhwiZKyjrW6NjTaWhAML-6clc2yg2o77_BCn8CPcEyly423GM77Y3_KZk")
genius_token()


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


###############################



### getting lyrics No 2 ### 

df_all_billboard_weeks <- read.csv("C:/Users/Nico/Documents/Uni/3. Sem/DS Projekt/Code_and_Data/all_data_billboard_weeks.csv", stringsAsFactors = FALSE)



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



################################################################################################################


get_lyrics_from_combination_al <- function(a, a_1, s) {
  
  if (a == a_1) { # case that artist has not been split 
    
    out_1 <- get_lyrics_search(a, s)
    

    if (length(out_1$line) == 0) {
      
      lyrics <- ""
      
    } else {
      

      lyrics <- paste(unlist(out_1$line), collapse = " ")
 

      }
    
  } else { # in other cases try  artist
    
    out_1 <- get_lyrics_search(a_1, s)
    
    
    if (length(out_1$line) == 0) {
      
      lyrics <- ""
      
    } else {
      
      lyrics <- paste(unlist(out_1$line), collapse = " ")
      
    }
  }
    return(lyrics)
}



## creating purrr function for error control !


library(purrr)


poss_lyrics <- possibly(get_lyrics_from_combination_al, otherwise = NaN)




##


get_lyrics_from_combination_safely_al <- function(a, a_1, s) {
  
  aa <- Sys.time()
  
  repeat {
    
    lyrics <- as.character(get_lyrics_from_combination_al(a, a_1, s))
    b <- Sys.time() 
    
    if (lyrics !=  ""){
      
      break
      
    } else if ((b-aa)>15) {
      break
    }
    
  }
  
  return(lyrics)
}



poss_lyrics <- possibly(get_lyrics_from_combination_safely_al, otherwise = NaN)



## 



for (i in 1:20) {
  
  df_all_billboard_weeks_unique_with_genre$lyrics[i] <- poss_lyrics(df_all_billboard_weeks_unique_with_genre$artists[i], df_all_billboard_weeks_unique_with_genre$artists_1[i], df_all_billboard_weeks_unique_with_genre$songs[i])
  
  
  print(i)
  
  if ((i %% 100) == 0) {
    Sys.sleep(10)
  }
  
}


df_all_billboard_weeks_unique_with_genre$lyrics[1:20]

g <- list(df_all_billboard_weeks_unique_with_genre$artists, df_all_billboard_weeks_unique_with_genre$artists_1, df_all_billboard_weeks_unique_with_genre$songs)


nested_list_lyrics <-  purrr::pmap(g, poss_lyrics)


list_lyrics <- unlist(nested_list_lyrics)



