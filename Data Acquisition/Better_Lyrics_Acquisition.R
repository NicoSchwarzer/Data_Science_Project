
rm(list = ls())

# read in lyrics data by Leo
leo_lyrics <- readr::read_csv("leo_lyrics.csv")
leo_lyrics$lyrics <- ifelse(is.na(leo_lyrics$lyrics_v1), leo_lyrics$lyrics_v2, leo_lyrics$lyrics_v1)
leo_lyrics <- leo_lyrics %>%
  select(song, artist, lyrics)
leo_lyrics <- leo_lyrics[!is.na(leo_lyrics$lyrics),]
leo_lyrics <- leo_lyrics[, c("artist", "song", "lyrics")]
colnames(leo_lyrics) <- c("Artist", "Song", "Lyrics")
glimpse(leo_lyrics)

# read in lyrics data by Nico
nico_data <- readr::read_csv("df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED.csv")
nico_data_u <- nico_data[!duplicated(nico_data[, c("songs", "artists")]),]
nico_data_u <- nico_data_u %>%
  select(artists, songs, lyrics)
colnames(nico_data_u) <- c("Artist", "Song", "Lyrics")
nico_data_u[nico_data_u$Lyrics == "NaN", ] <- NA
nico_data_u <- na.omit(nico_data_u)
glimpse(nico_data_u)
rm(nico_data)

# read in all songs
data <- readr::read_csv("billboard.csv")
data_u <- data[!duplicated(data[, c("song", "artist")]),]
data_u <- data_u %>%
  select(song, artist)
data_u <- data_u[, c("artist", "song")]
colnames(data_u) <- c("Artist", "Song")
glimpse(data_u)
rm(data)





## bind all lyrics dataframes
lyrics_data <- rbind(leo_lyrics, nico_data_u)
rm(list = c("leo_lyrics", "nico_data_u"))
# remove duplicates
lyrics_data_u <- lyrics_data[!duplicated(lyrics_data[, c("Artist", "Song")]),]
rm(lyrics_data)

data_u <- dplyr::left_join(data_u, lyrics_data_u,
                           by = c("Song" = "Song", "Artist" = "Artist"))

data_u_lyrics <- na.omit(data_u)
write.csv(data_u_lyrics, "C:/Users/leona/Documents/Data_Science_Project/Data/lyrics_take1.csv")


## now go to the trouble maker
trouble_maker <- data_u[is.na(data_u$Lyrics),]
rm(list = c("data_u_lyrics", "data_u"))



## remove the featuring stuff
trouble_maker$pure_artist <- gsub(" Featuring.*", "", trouble_maker$Artist)
trouble_maker$pure_artist <- gsub(" Feat.*", "", trouble_maker$pure_artist)
trouble_maker$pure_artist <- gsub(" &.*", "", trouble_maker$pure_artist)
trouble_maker <- trouble_maker %>%
  select(-c("Lyrics"))
glimpse(trouble_maker)


## try merge again
trouble_maker <- left_join(trouble_maker, lyrics_data_u,
                           by = c("pure_artist" = "Artist", "Song" = "Song"))

glimpse(trouble_maker)

peacefull_trublemaker <- trouble_maker[!is.na(trouble_maker$Lyrics),]
peacefull_trublemaker <- peacefull_trublemaker %>%
  select(-pure_artist)
glimpse(peacefull_trublemaker)
write.csv(peacefull_trublemaker, "C:/Users/leona/Documents/Data_Science_Project/Data/lyrics_take2.csv")
rm(peacefull_trublemaker)

trouble_maker <- trouble_maker[is.na(trouble_maker$Lyrics),]

# clean song name a little
trouble_maker$Song <- gsub("&amp;", "&", trouble_maker$Song)


trouble_maker <- left_join(trouble_maker, lyrics_data_u,
                           by = c("Artist" = "Artist", "Song" = "Song"))

trouble_maker$Lyrics <- trouble_maker$Lyrics.x
trouble_maker$Lyrics <- ifelse(is.na(trouble_maker$Lyrics), trouble_maker$Lyrics.y, trouble_maker$Lyrics.x)

peacefull_trublemaker <- trouble_maker[!is.na(trouble_maker$Lyrics),]
dplyr::glimpse(peacefull_trublemaker)
peacefull_trublemaker <- peacefull_trublemaker %>%
  select(Artist, Song, Lyrics)
write.csv(peacefull_trublemaker, "C:/Users/leona/Documents/Data_Science_Project/Data/lyrics_take3.csv")
rm(peacefull_trublemaker)

trouble_maker <- trouble_maker %>%
  select(Artist, Song, pure_artist)



# -> nochmal scrapen mit removed ' & sonderzeichen in songs und artists und pure artist undso

# create pure songs/artists
trouble_maker$url_artist <- gsub("'", "", trouble_maker$pure_artist)
trouble_maker$url_song <- gsub("'", "", trouble_maker$Song)

# create empty column to fill in lyrics 
trouble_maker$Lyrics <- NA
trouble_maker$Lyrics <- as.character(trouble_maker$Lyrics)

write.csv(trouble_maker, "C:/Users/leona/Documents/Data_Science_Project/Data/troublemaker.csv")

trouble_maker <- readr::read_csv("C:/Users/leona/Documents/Data_Science_Project/Data/troublemaker.csv")

glimpse(trouble_maker)
success <- 0

## try catching songs via URL
for (i in 1:nrow(trouble_maker)){
  
  # count the tries
  r <- 0
  
  while(r<4){
    # try get lyrics via URL
    l <- get_lyrics_v2(a = trouble_maker[i,]$url_artist,
                       s = trouble_maker[i,]$url_song)
    #print(r)
    # escape the loop if found or after third try
    if(is.null(nrow(l))){
      r <- r + 1
      #print("escaped increased")
    } else {
      r <- r + 1 + nrow(l)
      #print("worked increases")
    }
    
  }
  
  if(!is.null(nrow(l))){
    # if something is found fill in
    if(nrow(l) > 0){
      # fill in
      lyrics <- paste(l$line, collapse = " ")
      trouble_maker$Lyrics[i] <- lyrics
      success <- success + 1
    }
  }
  
  if(i%%50 == 0){
    print(paste0(i, "     Success Count: ", success))
  }
  
}



trouble_maker <- trouble_maker[,c("Artist", "Song", "Lyrics")]
write.csv(trouble_maker, "C:/Users/leona/Documents/Data_Science_Project/Data/troublemaker.csv",
          row.names = FALSE)

trouble_maker <- readr::read_csv("troublemaker.csv")
trouble_maker$Lyrics <- as.character(trouble_maker$Lyrics)

glimpse(trouble_maker)


rm(list = ls())

write.csv(peacefull_trublemaker, "C:/Users/leona/Documents/Data_Science_Project/Data/lyrics_take5.csv")
peacefull_trublemaker <- trouble_maker[!is.na(trouble_maker$Lyrics),]
rm(peacefull_trublemaker)

trouble_maker <- trouble_maker[is.na(trouble_maker$Lyrics),]

glimpse(trouble_maker)

# -> remove "." from artists and songs
# and statt &
# alles in klammer weg oder einfach klammer weg
# satzzeichen !?, weg 

trouble_maker$url_artist2 <- gsub("&", "and", trouble_maker$Artist)
trouble_maker$url_artist2 <- gsub("'", "", trouble_maker$url_artist2)
trouble_maker$url_artist2 <- gsub("\\.", "", trouble_maker$url_artist2)
trouble_maker$url_artist2 <- gsub("!", "", trouble_maker$url_artist2)
trouble_maker$url_artist2 <- gsub("?", "", trouble_maker$url_artist2, fixed = TRUE)
trouble_maker$url_artist2 <- gsub(",", "", trouble_maker$url_artist2)


trouble_maker$url_song2 <- gsub("&", "and", trouble_maker$url_song)
trouble_maker$url_song2 <- gsub("\\.", "", trouble_maker$url_song2)
trouble_maker$url_song2 <- gsub("!", "", trouble_maker$url_song2)
trouble_maker$url_song2 <- gsub("?", "", trouble_maker$url_song2, fixed = TRUE)
trouble_maker$url_song2 <- gsub(",", "", trouble_maker$url_song2)


trouble_maker$url_artist3 <- gsub(" \\(.*", "", trouble_maker$url_artist2)
trouble_maker$url_song3 <- gsub(" \\(.*", "", trouble_maker$url_song2)

trouble_maker$url_artist4 <- gsub(" \\(", "", trouble_maker$url_artist2)
trouble_maker$url_artist4 <- gsub("\\)", "", trouble_maker$url_artist4)

trouble_maker$url_song4 <- gsub("\\(", "", trouble_maker$url_song2)
trouble_maker$url_song4 <- gsub("\\)", "", trouble_maker$url_song4)
trouble_maker$url_song4 <- gsub("\\*\\*", "uc", trouble_maker$url_song4)


#### 2 ####
trouble_maker2 <- trouble_maker[is.na(trouble_maker$Lyrics),]

# try catching songs via URL
for (i in 1:nrow(trouble_maker2)){
  
  # count the tries
  r <- 0
  
  while(r<6){
    # try get lyrics via URL
    l <- get_lyrics_v2(a = trouble_maker2[i,]$url_artist2,
                       s = trouble_maker2[i,]$url_song2)
    #print(r)
    # escape the loop if found or after third try
    if(is.null(nrow(l))){
      r <- r + 1
      #print("escaped increased")
    } else {
      r <- r + 1 + nrow(l)
      #print("worked increases")
    }
    
  }
  
  if(!is.null(nrow(l))){
    # if something is found fill in
    if(nrow(l) > 0){
      # fill in
      lyrics <- paste(l$line, collapse = " ")
      trouble_maker2$Lyrics[i] <- lyrics
      success <- success + 1
    }
  }
  
  if(i%%50 == 0){
    print(paste0(i, "     Success Count: ", success))
  }
  
}



#### 3 ####
trouble_maker3 <- trouble_maker2[is.na(trouble_maker2$Lyrics),]

# try catching songs via URL
for (i in 1:nrow(trouble_maker3)){
  
  # count the tries
  r <- 0
  
  while(r<6){
    # try get lyrics via URL
    l <- get_lyrics_v2(a = trouble_maker3[i,]$url_artist3,
                       s = trouble_maker3[i,]$url_song3)
    #print(r)
    # escape the loop if found or after third try
    if(is.null(nrow(l))){
      r <- r + 1
      #print("escaped increased")
    } else {
      r <- r + 1 + nrow(l)
      #print("worked increases")
    }
    
  }
  
  if(!is.null(nrow(l))){
    # if something is found fill in
    if(nrow(l) > 0){
      # fill in
      lyrics <- paste(l$line, collapse = " ")
      trouble_maker3$Lyrics[i] <- lyrics
      success <- success + 1
    }
  }
  
  if(i%%50 == 0){
    print(paste0(i, "     Success Count: ", success))
  }
  
}




#### 4 ####
trouble_maker4 <- trouble_maker[is.na(trouble_maker$Lyrics),]

# try catching songs via URL
for (i in 1:nrow(trouble_maker4)){
  
  # count the tries
  r <- 0
  
  while(r<4){
    # try get lyrics via URL
    l <- get_lyrics_v2(a = trouble_maker4[i,]$url_artist2,
                       s = trouble_maker4[i,]$url_song4)
    #print(r)
    # escape the loop if found or after third try
    if(is.null(nrow(l))){
      r <- r + 1
      #print("escaped increased")
    } else {
      r <- r + 1 + nrow(l)
      #print("worked increases")
    }
    
  }
  
  if(!is.null(nrow(l))){
    # if something is found fill in
    if(nrow(l) > 0){
      # fill in
      lyrics <- paste(l$line, collapse = " ")
      trouble_maker4$Lyrics[i] <- lyrics
      success <- success + 1
    }
  }
  
  if(i%%50 == 0){
    print(paste0(i, "     Success Count: ", success))
  }
  
}


success <- 0



trouble_maker2_clean <- trouble_maker2[!is.na(trouble_maker2$Lyrics),]
trouble_maker3_clean <- trouble_maker3[!is.na(trouble_maker3$Lyrics),]


trouble_maker_clean <- rbind(trouble_maker2_clean, trouble_maker3_clean)

write.csv(trouble_maker_clean, "C:/Users/leona/Documents/Data_Science_Project/Data/lyrics_take6.csv")

trouble_maker_clean <- trouble_maker_clean %>%
  select(Artist, Song, Lyrics)


trouble_maker <- trouble_maker4
rm(list = c("trouble_maker_clean", "trouble_maker2_clean", "trouble_maker3_clean"))
rm(list = c("trouble_maker_orig", "trouble_maker2", "trouble_maker3", "trouble_maker4"))



#######################


peacefull_troublemaker <- trouble_maker4[!is.na(trouble_maker4$Lyrics),]
peacefull_troublemaker <- peacefull_troublemaker %>%
  select(Artist, Song, Lyrics)

trouble_maker <- trouble_maker4[is.na(trouble_maker4$Lyrics),]

write.csv(peacefull_troublemaker, "C:/Users/leona/Documents/Data_Science_Project/Data/lyrics_take9.csv")


#### now process all lyrics data ####
setwd("C:/Users/leona/Documents/Data_Science_Project/Data")
lyrics1 <- readr::read_csv("lyrics_take1.csv")
lyrics2 <- readr::read_csv("lyrics_take2.csv")
lyrics3 <- readr::read_csv("lyrics_take3.csv")
lyrics4 <- readr::read_csv("lyrics_take4.csv")
lyrics5 <- readr::read_csv("lyrics_take5.csv")
lyrics6 <- readr::read_csv("lyrics_take6.csv")
lyrics7 <- readr::read_csv("lyrics_take7.csv")

lyrics1 <- lyrics1 %>%
  select(Artist, Song, Lyrics)
lyrics2 <- lyrics2 %>%
  select(Artist, Song, Lyrics)
lyrics3 <- lyrics3 %>%
  select(Artist, Song, Lyrics)
lyrics4 <- lyrics4 %>%
  select(Artist, Song, Lyrics)
lyrics5 <- lyrics5 %>%
  select(Artist, Song, Lyrics)
lyrics6 <- lyrics6 %>%
  select(Artist, Song, Lyrics)
lyrics7 <- lyrics7 %>%
  select(Artist, Song, Lyrics)

lyrics_data <- rbind(lyrics1, lyrics2, lyrics3, lyrics4, 
                     lyrics5, lyrics6, lyrics7)



#### read in billboard data and merge with lyrics data
billboard <- readr::read_csv("billboard.csv")

billboard <- dplyr::left_join(billboard, lyrics_data,
                              by = c("artist" = "Artist", "song" = "Song"))


count <- billboard %>%
  filter(!is.na(Lyrics)) %>%
  group_by(week) %>%
  count()


mean(count$n)

plot(count$week, count$n/100, type = "l",
     ylim = c(0,1))





write.csv(billboard, "C:/Users/leona/Documents/Data_Science_Project/Data/billboard_lyrics.csv")






# in artist & durch and ersetzen

# [] komplett weg am Anfang

d <- str_detect(trouble_maker$url_song4, "\\*\\*")


tail(trouble_maker[d, c("Artist", "url_song2")])



gsub("\\*\\*", "uc", trouble_maker$url_song2)

str_detect("F**k it", "\\*\\*")



############## Next Go ##############

## Load packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("geniusr")) install.packages("geniusr")
if (!require("stringr")) install.packages("stringr")

source("functions_get_lyrics.R")

base_data <- readr::read_csv("base_data_cleaned.csv")

trouble_maker <- base_data %>%
  select(c("songs", "artists", "lyrics")) %>%
  filter(is.na(lyrics))

trouble_maker <- trouble_maker[!duplicated(trouble_maker[, c("songs", "artists")]),]

glimpse(trouble_maker)



for (i in 1:nrow(trouble_maker)){
  
  # make artist vector
  artist_vector <- get_strcombinations(trouble_maker$artists[i])
  
  # make song vector
  song_vector <- get_strcombinations(trouble_maker$songs[i])
  
  # get lyrics
  l <- get_lyrics(artist_vector = artist_vector,
                  song_vector = song_vector,
                  repeats = 5, print_comb = TRUE)
  
  
  # fill in lyrics
  trouble_maker$lyrics[i] <- l
  
  success <- sum(!is.na(trouble_maker$lyrics))
  if(i%%10 == 0){
    print(paste0(i, "     Success Count: ", success))
  }
  
}



#### Merge trouble maker with base data

peacefull_troublemaker <- trouble_maker[!is.na(trouble_maker$lyrics),]

colnames(peacefull_troublemaker)[colnames(peacefull_troublemaker) == "lyrics"] <- "Lyrics"

base_data <- readr::read_csv("base_data_raw.csv")


base_data <- left_join(base_data, peacefull_troublemaker,
                       by = c("songs", "artists"))
glimpse(base_data)
glimpse(peacefull_troublemaker)


base_data$lyrics <- ifelse(is.na(base_data$lyrics), base_data$Lyrics, base_data$lyrics)

base_data <- base_data %>%
  select(-Lyrics)

count <- base_data %>%
  filter(!is.na(lyrics)) %>%
  group_by(dates) %>%
  count()

mean(count$n)

write.csv(base_data, "base_data_raw.csv",
          col.names = FALSE)

trouble_maker <- trouble_maker[is.na(trouble_maker$lyrics),]

plot(count$dates, count$n/100,
     ylim = c(0,1), type = "l")


sum(count$n == 100)









