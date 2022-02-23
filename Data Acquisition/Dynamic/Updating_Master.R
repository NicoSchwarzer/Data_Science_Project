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
if (!require("deeplr")) install.packages("deeplr")
if (!require("xgboost")) install.packages("xgboost")
if (!require("stringi")) install.packages("stringi")
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("curl")) install.packages("curl")
if(!require("writexl")) install.packages("writexl")



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
library(deeplr)
library(xgboost)
library(stringi)
library(httr)
library(jsonlite)
library(curl)
library(writexl)




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
# Nicos token
Sys.setenv(GENIUS_API_TOKEN = "iZdkkCTGhwiZKyjrW6NjTaWhAML-6clc2yg2o77_BCn8CPcEyly423GM77Y3_KZk")
genius_token()

# Leos token
#Sys.setenv(GENIUS_API_TOKEN = "V0YxmAF_FAVWyTard84m4oAB5vhcyaIy7N-Uw24IGv8bcwH7chcgfpH-7FB7Mc5WL2uAeehKM8WgtNr9t_Sx0g")
#genius_token()



### Last FM API

fm_url <- "http://ws.audioscrobbler.com/2.0/?method=track.getInfo&api_key="
api_key_last_fm <- "a2e1207d25ce4237d86ded2d073fabea"
string_1 <- "&artist="
string_2 <- "&track="
string_3 <- "&format=json"

## Audio DB API

string_1a <- "http://www.theaudiodb.com/api/v1/json/523532/searchtrack.php?s="
string_2a <- "&t="


#############################
## Load Relevant functions ##
#############################

source("functions_for_updating.R")


#############################
### Read Data for Genres ####
#############################

genres_mapping <- readxl::read_excel("unique_genres.xlsx")
genres_mapping <- genres_mapping[,c("original_genre", "new_genre")]




###### start loop

while (3 < 4){


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

# get time difference of available date and online date
difftime_general <- difftime(last_date_on, last_date_all_df, units = "days")

if ( length(substr( difftime_general, start = 1, stop = 1)) == 2 ) {
  difftime_cleaned <- strtoi(substr( difftime_general, start = 1, stop = 2))
} else {
  difftime_cleaned <- strtoi(substr( difftime_general, start = 1, stop = 2))
}


### only performing updating when difftime_cleaned > 50 


if (difftime_cleaned > 0) {   ## start of large loop 

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
  

##################################################################
########### Getting new songs by scraping Billboard ##############
##################################################################

# create empty data frame to store results
df <- data.frame(matrix(nrow = 100*length(vec_missed_dates),
                        ncol = 3))

colnames(df) <- c("artists","songs", "dates")

# specify column types
df$artists <- as.character(df$artists)
df$songs <- as.character(df$songs)
df$dates <- as.Date(df$dates)

# create indices for the loop
a <- 1
d <- 1 

## loop through missed dates and scrape new songs
for (i in 1:length(vec_missed_dates)) {
  
  d_new <- new_data_df(base_url, vec_missed_dates[i])

  df$artists[a:(a+99)] <- as.character(d_new$artists)
  df$songs[a:(a+99)] <- as.character(d_new$songs)
  df$dates[a:(a+99)] <- rep(vec_missed_dates[d], 100)
  
  a <- a + 100
  d <- d + 1
}

## remove variables
rm(a)
rm(d)


## adding charts placement 
df$chart_rank <-  rep(c(1:100), length(vec_missed_dates))


### Splitting artist and getting new combination ###
df$combination <- paste0(df$artists,"  ", df$songs)   
df <- getting_new_artists(df)

# Coding variable of combination of artist and song title - also for the alternative representation of the artists
df$combination_1 <- paste0(df$artists_1,"  ", df$songs)   
df$combination_2 <- paste0(df$artists_2,"  ", df$songs)   


#########################################################################
## Dividing into new songs (not in all previous charts) and seen songs ##
#########################################################################

# combinations with A match in already scraped combinations
df_all_new_data_billboard_seen <- df[(df$combination %in% base_data_cleaned$combination) ,]


# combinations with NO match in already scraped combinations
df_all_new_data_billboard_unseen <- df[!(df$combination %in% base_data_cleaned$combination) ,]

df_all_new_data_billboard_unseen$artists <- as.character(df_all_new_data_billboard_unseen$artists)
df_all_new_data_billboard_unseen$songs <- as.character(df_all_new_data_billboard_unseen$songs)


##################################
## Getting data for new songs ####
##################################

## getting the respective genres ## 


## first trying per song genre with Audio DB API

df_all_new_data_billboard_unseen$genre <- "c"
max_iter <- nrow(df_all_new_data_billboard_unseen)


for (i in 1:max_iter) {
  
  # getting correct genre  
  df_all_new_data_billboard_unseen$genre[i] <- poss_audio_db_genre(df_all_new_data_billboard_unseen$artists[i], df_all_new_data_billboard_unseen$artists_1[i], df_all_new_data_billboard_unseen$artist_2[i], df_all_new_data_billboard_unseen$songs[i])

  
}  

##secondly trying with Last FM API


for (i in 1:max_iter) {
  
  if ( (df_all_new_data_billboard_unseen$genre[i] %in% genres_mapping$original_genre) == F) { 
    
    # getting correct genre  
    df_all_new_data_billboard_unseen$genre[i] <- poss_fm_genre(df_all_new_data_billboard_unseen$artists[i], df_all_new_data_billboard_unseen$artists_1[i], df_all_new_data_billboard_unseen$artist_2[i], df_all_new_data_billboard_unseen$songs[i])
                                                               
    
     
  }
  if (( i %% 100) == 0) {
    Sys.sleep(40)
  }  
}



##thirdly/lastly trying with Spotify API

for (i in 1:max_iter) {
  
  if ( (df_all_new_data_billboard_unseen$genre[i] %in% genres_mapping$original_genre) == F) { 

    # getting correct genre  
  df_all_new_data_billboard_unseen$genre[i] <- get_genre_from_combination(df_all_new_data_billboard_unseen$combination[i], df_all_new_data_billboard_unseen$combination_1[i], df_all_new_data_billboard_unseen$combination_2[i], df_all_new_data_billboard_unseen$artists[i], df_all_new_data_billboard_unseen$artists_1[i], df_all_new_data_billboard_unseen$artists_2[i])
  
  }
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



######################################
########### Getting Lyrics ###########
######################################

## add lyrics column

df_all_new_data_billboard_unseen$lyrics <- as.character(NA) 

for (i in 1:nrow(df_all_new_data_billboard_unseen)){
  
  # make artist vector
  artist_vector <- get_strcombinations(df_all_new_data_billboard_unseen$artists[i])
  
  # make song vector
  song_vector <- get_strcombinations(df_all_new_data_billboard_unseen$songs[i])
  
  # get lyrics
  l <-  get_lyrics_alt(artist_vector = artist_vector,
                  song_vector = song_vector,
                  repeats = 5, print_comb = FALSE)
  
  
  # fill in lyrics
  df_all_new_data_billboard_unseen$lyrics[i] <- l
  
#  success <- sum(!is.na(df_all_new_data_billboard_unseen$lyrics))
#  if(i%%10 == 0){
#    print(paste0(i, "     Success Count: ", success))
#  }
  
}


########################################################
########## Language Detection and Translation ########## 
########################################################

#### Language Detection of new songs

## read in vocabulary and detection model
vocab <- readr::read_csv("vocabulary_language_detection.csv")
vocab <- as.vector(vocab$x)
xgb_language_model <- xgboost::xgb.load("xgb_language_model.model")

# initialize empty vector to store detected languages
language <- character(length = nrow(df_all_new_data_billboard_unseen))

# loop through new lyrics and detect language (sorry for loop, didnt try to vectorize the function :D)
for (i in 1:nrow(df_all_new_data_billboard_unseen)){
  # get language for every new song
  language[i] <- detect_language(lyrics = df_all_new_data_billboard_unseen$lyrics[i],
                                 vocab = vocab,
                                 num_lang_identifier = num_lang_identifier,
                                 model = xgb_language_model)
}

df_all_new_data_billboard_unseen$language <- language


#### Translation for non-english songs

## loop through the new songs and translate the non-english songs
for (i in 1:nrow(df_all_new_data_billboard_unseen)){
  
  if (df_all_new_data_billboard_unseen$language[i] != "english"){
    # need to check if deepl-key volumne still enough for the month :D
    volume_left <- deeplr::usage2(deepl_key)$character_limit - deeplr::usage2(deepl_key)$character_count
    char_count <- stringr::str_count(df_all_new_data_billboard_unseen$lyrics[i])
    if (volume_left < char_count){
      # break out of the loop if key not active any more
      break
    } else {
      # otherwise translate
      translation <- deeplr::translate2(
        text = df_all_new_data_billboard_unseen$lyrics[i],
        target_lang = "EN",
        auth_key = deepl_key
      )
      
      # replace lyrics by translation
      df_all_new_data_billboard_unseen$lyrics[i] <- translation
    }
  }
  
}

# drop language variable
df_all_new_data_billboard_unseen <- df_all_new_data_billboard_unseen %>%
  select(-language)

# clean memory inbetween
gc()




##############################
## Taking care of seen data ## 
##############################


df_all_new_data_billboard_seen_2 <- dplyr::left_join(df_all_new_data_billboard_seen, dplyr::distinct(base_data_raw[, (!names(base_data_raw) %in% c('dates', 'chart_rank', 'artists', 'songs', 'artists_1', 'artists_2', 'combination_1', 'combination_2'))]), by = "combination")
df_all_new_data_billboard_seen <- distinct(df_all_new_data_billboard_seen_2, combination, .keep_all = T)

rm(df_all_new_data_billboard_seen_2)


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
base_data_cleaned <- dplyr::bind_rows(base_data_cleaned, df_new_with_features_reduced)

tail(base_data_cleaned)


rm(df_new_with_features)
rm(df_new_with_features_reduced)


## saving results ##

write.csv(base_data_raw, "base_data_raw.csv",
          row.names = FALSE)

write.csv(base_data_cleaned, "base_data_cleaned.csv",
          row.names = FALSE)





## sourcing 

source("IntroPage_Complexity.R")

source("pronoun_analysis_for_update.R")

source("Quiz_Data_Updating.R")

print(paste0("New data retrieved and successuflly updated on ", Sys.time()))

} else {    ## end of large loop that ensures that updating is only done when 
  
  print(paste0("No new data on ", Sys.time()))
  
  gc()
  
  # sleep for two hours if no new data is available, then check again
  Sys.sleep(2*60*60)
}   



}   ## end of while loop


