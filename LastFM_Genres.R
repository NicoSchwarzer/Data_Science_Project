###################
## LastFM Genres ##
###################

## Since the Spotify Genres are not available at the song level, we also try to get genres via the LastFM API.
install.packages(c("httr", "jsonlite"))


library(tidyverse)
library(readr)
library(httr)
library(jsonlite)
library(curl)

df <- read_csv("base_data_raw.csv")

df <- df[, c("artists", "artists_1", "artists_2", "songs", "combination")]
df <- distinct(df)


## Meta Info 

fm_url <- "http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key="
api_key_last_fm <- "a2e1207d25ce4237d86ded2d073fabea"
string_1 <- "&artist="
string_2 <- "&track="
string_3 <- "&format=json"
#"http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key=a2e1207d25ce4237d86ded2d073fabea&artist=cher&track=dfg&format=json")

genres_mapping <- readxl::read_excel("unique_genres.xlsx")
genres_mapping <- genres_mapping[,c("original_genre", "new_genre")]


df$genre_2 <- NA


get_lastfm_genre <- function(artist, artist_1, artist_2, song ) {
  
  info_indication <- 0 
  
  string_song <- tolower(gsub(" ", "",song))
  string_song <-  str_replace_all(string_song, "[^[:alnum:]]", "")

  url_1 <- paste0(fm_url, api_key_last_fm, string_1, tolower(gsub(" ", "",artist)), string_2, string_song, string_3)

  
  res_1 <- httr::GET(url_1)
  aa1 <- jsonlite::parse_json(res_1)
  tag_1 <- aa$track$toptags$tag[1][[1]]


  if (is.null(tag_1) == F) {
    
    info_indication <- 1
    genre <- as.character(aa$track$toptags$tag[[1]]$name)
    
  } else {    # case of artist_1
    
    url_2 <- paste0(fm_url, api_key_last_fm, string_1, tolower(gsub(" ", "",artist_1)), string_2, string_song, string_3)
    res_2 <- httr::GET(url_1)
    aa2 <- jsonlite::parse_json(res_1)
    tag_2 <- aa2$track$toptags$tag[1][[1]]
      
    if (is.null(tag_1) == F) {
        info_indication <- 2
        genre <- as.character(aa2$track$toptags$tag_2[[1]]$name)
        
    } else {     # case of artist_2
        
      url_3 <- paste0(fm_url, api_key_last_fm, string_1, tolower(gsub(" ", "",artist_2)), string_2, string_song, string_3)
      res_3 <- httr::GET(url_1)
      aa3 <- jsonlite::parse_json(res_1)
      tag_3 <- aa3$track$toptags$tag[1][[1]]
      
      if (is.null(tag_1) == F) {

        info_indication <- 3
        genre <- as.character(aa3$track$toptags$tag_3[[1]]$name)
        
      } else {
        info_indication <- 4 
        genre <- NA
      } } } 
  
  
  # replacing genre not in known genres
  genre <- tolower(genre)
  
  if (is.na(genre) == F) { 
  if ((genre %in%  genres_mapping$original_genre) == F)  {
    
    if (info_indication == 1){
      tag_1 <- aa$track$toptags$tag[1][[1]]
      if (is.null(tag_1) == F) {
        genre <- tag_1
      }}
    
    if (info_indication == 2){
      tag_2 <- aa2$track$toptags$tag[1][[1]]
      if (is.null(tag_2) == F) {
        genre <- tag_2
      }}

    if (info_indication == 3){
      tag_3 <- aa3$track$toptags$tag[1][[1]]
      if (is.null(tag_3) == F) {
        genre <- tag_3
      }}
    }    
  } 
  # checking if genre can be used at all 
  
  genre <- tolower(genre)
  
  if ( (genre %in% genres_mapping$original_genre) == F)  {
    genre <- NA
  }
    
  return(genre)
  
}
    
  
## possibly function for error control 
poss_fm_genre <- possibly(get_lastfm_genre, otherwise = NA)


for (i in 1:nrow(df)) { # 1:
  
  df$genre_2[i] <- poss_fm_genre(df$artists[i], df$artists_1[i], df$artists_2[i], df$songs[i])


  # stopping every 200th time
  if ((i %% 200) == 0) {
    Sys.sleep(30)
    
  }
}


base_data_cleaned <- readr::read_csv("base_data_cleaned.csv")
base_data_raw <- readr::read_csv("base_data_raw.csv")


dd <- dplyr::left_join(base_data_raw,df[, c("combination", "genre_2")], by = "combination")


for (i in 1:nrow(dd)) {
  
  if (  ( (is.na(dd$genre_2[i]) == F) ) & (dd$genre_2[i] %in% genres_mapping$original_genre) ) {

    dd$genre[i] <- dd$genre_2[i]
    print(i)
  }  
}



source("Genres_mapping.R")

dd2 <- matching_genres(dd)



dd2$genre_2 <- NULL
dd$genre_2 <- NULL

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

