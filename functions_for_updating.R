###############################
#### Functions for updating ###
###############################



fm_url <- "http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key="
api_key_last_fm <- "a2e1207d25ce4237d86ded2d073fabea"
string_1 <- "&artist="
string_2 <- "&track="
string_3 <- "&format=json"

## Audio DB API

string_1a <- "http://www.theaudiodb.com/api/v1/json/523532/searchtrack.php?s="
string_2a <- "&t="



## Getting genres via Audio DB 

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



fm_url <- "http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key="
api_key_last_fm <- "a2e1207d25ce4237d86ded2d073fabea"
string_1 <- "&artist="
string_2 <- "&track="
string_3 <- "&format=json"


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


## function to  get song lyrics from artist song title combination
source("functions_get_lyrics.R")


getting_new_artists <- function(df) {
  
  ## This function splits the artist column by commonly used stopwords for artists and yields two new columns for the artists,  which are added to the DF ##
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
  xml2::write_xml(rvest::html_node(hot100, "body"), "hot100.txt")
  
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
  help_df <- help_df[(start_idx+1):nrow(help_df), c(1,4)]
  
  so <- as.character(help_df$V1)
  ar <- as.character(help_df$V1.1)
  
  ar2 <- c()
  i <- 1
  
  while (i <= 397) {
    ar2 <- c(ar2, ar[i])
    i = i + 4
  }
  
  so2 <- c()
  i <- 1
  
  while (i <= 397) {
    so2 <- c(so2, so[i])
    i = i + 4
  }
  
  
  df_new <- data.frame(ar2, so2)
  names(df_new) <- c("artists", "songs")
  df_new$dates <- date
  
  
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
    last_date_on <- today - 3
  } else if (dow == "Wednesday") {
    last_date_on <- today -4 
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

## sourcing in function for translation
source("lyrics_translation_functions.R")


