##################
## Audio DB API ##
##################

library(tidyverse)
library(readr)
library(httr)
library(jsonlite)
library(curl)

## Since the Spotify Genres are not available at the song level, we also try to get genres via the Audio DB API.

df <- read_csv("base_data_raw.csv")

df <- df[, c("artists", "artists_1", "artists_2", "songs", "combination")]
df <- distinct(df)


## Meta Info 
string_1a <- "http://www.theaudiodb.com/api/v1/json/523532/searchtrack.php?s="
string_2a <- "&t="

genres_mapping <- readxl::read_excel("unique_genres.xlsx")
genres_mapping <- genres_mapping[,c("original_genre", "new_genre")]



audio_db_genre <- function(artist, artist_1, artist_2, song ) {
  
  info_indication <- 0 
  
  string_song <-  tolower(song)
    

  url_1 <- paste0(string_1a, tolower(artist), string_2a, string_song)
  
  
  res_1 <- httr::GET(url_1)
  
  aa1 <- jsonlite::parse_json(res_1)
  
  
  
  jsonlite::parse_json(res_1)
  
  tag_1 <- aa1$track[[1]]$strGenre

  
  if (is.null(tag_1) == F) {
    
    info_indication <- 1
    genre <- as.character(aa1$track[[1]]$strGenre)
    
  } else {    # case of artist_1
    
    url_2 <- paste0(string_1a, tolower(artist_1), string_2a, string_song)
    res_2 <- httr::GET(url_2)
    aa2 <- jsonlite::parse_json(res_2)
    tag_2 <- aa2$track[[1]]$strGenre

    
    if (is.null(tag_2) == F) {
      info_indication <- 2
      genre <- as.character(aa2$track[[1]]$strGenre)
      
    } else {     # case of artist_2
      
      url_3 <- paste0(string_1a, tolower(artist_2), string_2a, string_song)
      res_3 <- httr::GET(url_3)
      aa3 <- jsonlite::parse_json(res_3)
      tag_3 <- aa3$track[[1]]$strGenre
      
      if (is.null(tag_3) == F) {
        
        info_indication <- 3
        genre <- as.character(aa3$track[[1]]$strGenre)
        
      } else {
        info_indication <- 4 
        genre <- NA
      } } } 
  
  
  # replacing genre not in known genres
  genre <- tolower(genre)
  
 # if ( (genre %in% genres_mapping$original_genre) == F)  {
#    genre <- NA
#  }
  
  return(genre)
  
}

## possibly function for error control 
poss_audio_db_genre <- possibly(audio_db_genre, otherwise = NA)



## trying out to get all genres to append our excel list

df$ge <- NA

for (i in 1:nrow(df)) { # 1:
  
#  i <- 2
  
  #df$genre_2[i] <- 
  print(i)
  df$ge[i] <-   poss_fm_genre(df$artists[i], df$artists_1[i], df$artists_2[i], df$songs[i]) 
  print(df$ge[i])
}


# potential new genres
unique(df$ge[1:nrow(df)])[!(unique(df$ge[1:nrow(df)]) %in% genres_mapping$original_genre)]


## have been added (with mappings) to the excel file
  


## Now re-rwriting the function above to exclude non-sensical genres and letting it run affter the DF

df$artists[i]

i <- 2
artist <- df$artists[i]
song <- df$songs[i]



audio_db_genre <- function(artist, artist_1, artist_2, song ) {
  
  info_indication <- 0 
  
  string_song <-  tolower(song)
  
  
  url_1 <- paste0(string_1a, tolower(artist), string_2a, string_song)
  
  
  res_1 <- httr::GET(url_1)
  
  aa1 <- jsonlite::parse_json(res_1)
  
  jsonlite::parse_json(res_1)
  
  tag_1 <- aa1$track[[1]]$strGenre
  
  
  if (is.null(tag_1) == F) {
    
    info_indication <- 1
    genre <- as.character(aa1$track[[1]]$strGenre)
    
  } else {    # case of artist_1
    
    url_2 <- paste0(string_1a, tolower(artist_1), string_2a, string_song)
    res_2 <- httr::GET(url_2)
    aa2 <- jsonlite::parse_json(res_2)
    tag_2 <- aa2$track[[1]]$strGenre
    
    
    if (is.null(tag_2) == F) {
      info_indication <- 2
      genre <- as.character(aa2$track[[1]]$strGenre)
      
    } else {     # case of artist_2
      
      url_3 <- paste0(string_1a, tolower(artist_2), string_2a, string_song)
      res_3 <- httr::GET(url_3)
      aa3 <- jsonlite::parse_json(res_3)
      tag_3 <- aa3$track[[1]]$strGenre
      
      if (is.null(tag_3) == F) {
        
        info_indication <- 3
        genre <- as.character(aa3$track[[1]]$strGenre)
        
      } else {
        info_indication <- 4 
        genre <- NA
      } } } 
  
  
  # replacing genre not in known genres
  genre <- tolower(genre)
  
   if ( (genre %in% genres_mapping$original_genre) == F)  {
      genre <- NA
    }
  
  return(genre)
  
}

## possibly function for error control 
poss_audio_db_genre <- possibly(audio_db_genre, otherwise = NA)




df$ge <- NA

for (i in 1:nrow(df)) { 
  print(i)
  df$ge[i] <-   poss_fm_genre(df$artists[i], df$artists_1[i], df$artists_2[i], df$songs[i]) 
  print(df$ge[i])

}



base_data_cleaned <- readr::read_csv("base_data_cleaned.csv")
base_data_raw <- readr::read_csv("base_data_raw.csv")


dd <- dplyr::left_join(base_data_raw,df[, c("combination", "ge")], by = "combination")

for (i in 1:nrow(dd)) {
  
  if (  ( (is.na(dd$ge[i]) == F) ) & (dd$ge[i] %in% genres_mapping$original_genre) ) {
    
    dd$genre[i] <- dd$ge[i]
    print(i)
  }  
}



# einmal genres_mapping function laufen lassen!

dd2 <- matching_genres(dd)

dd2$genre_2 <- NULL  ##dd$ge
dd$genre_2 <- NULL

dd$'...1' <- NULL
dd2$'...1' <- NULL

base_data_cleaned$'...1' <- NULL
base_data_raw$'...1' <- NULL


dd$ge <- NULL
dd2$ge <- NULL

# just to check
names(dd2) == names(base_data_cleaned)
names(dd) == names(base_data_raw)


## zu base data raw

base_data_raw <- dd
write.csv(base_data_raw, "base_data_raw.csv")


## zu base data raw

base_data_cleaned <- dd2
write.csv(base_data_cleaned, "base_data_cleaned.csv")





rm(dd)
rm(dd2)
rm(base_data_cleaned)
rm(base_data_raw)
rm(df)
rm(genres_mapping)

