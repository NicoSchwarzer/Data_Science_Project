#########################
## Web Scraping Tryout ##
#########################


if (!require("rvest")) install.packages("rvest")
if (!require("lubridate")) install.packages("lubridate")
if (!require("writexl")) install.packages("writexl")


library(lubridate)
library(rvest)
library(writexl)


## 1 Getting last available day (indicate for week) from Genius 

## 2 Getting a vector of all days from beginning of 2010 till last day -> retrieving each 7th 

## 3 For these dates:

  ## 3.1 Parse billboard site to retrieve top 100 charts 

  ## 3.2 Order Information in a table 

  ## 3.3 Retrieve genre info from Spotify API and lyrics info from genius API

  ## 3.4 Append one large df



################################
## Meta-Info for Web Scraping ##
################################


base_url <-  "https://www.billboard.com/charts/hot-100#/charts/hot-100"

x_path_data <- '//*[contains(concat( " ", @class, " " ), concat( " ", "text--truncate", " " ))]'
x_path_date <- '//*[contains(concat( " ", @class, " " ), concat( " ", "button--link", " " ))]'


########################
## Relevant functions ##
########################

## function for getting last available date on Billboard ## 
get_last_date <- function(url, x_path_date) {
  
  result_1 <- rvest::read_html(url)
  
  xml_nodes_1 <- xml_nodes(result_1, xpath= x_path_date)
  
  html_text <- rvest::html_text(xml_nodes_1)
  
  date_here <- substr(html_text, start = 18, stop = 33)
  
  # in correct day format
  date_format <- lubridate::mdy(date_here)
  
  return(date_format)
  
}

## function for scraping ##

get_correct_text <- function(url, path_1) {
  # function to scrape based URL and x_path using rvest package 
  
  result_1 <- rvest::read_html(url)
  
  xml_nodes_1 <- xml_nodes(result_1, xpath= path_1)
  
  # retrieving relevant text with charts 1-100 
  charts <- list(rvest::html_text(xml_nodes_1))[[1]][13:100]
  
  return(charts)
}


## function for creating a DF from scraped data
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
  
  
  # combining to one DF
  df1 <- data.frame(artists,songs)
  
  
  # getting combination string for better identification
  df1$combination <- stringr::str_c(df1$artists, "  ", df1$songs)
  
  # renaming columns
  names(df1) <- c("artists", "songs", "combination")
  
  return(df1)
  
}


### function for getting lyrics and genre for the DF and apply cleaning 
# !! IMPORTANT for cleaning: if spotify_artist = "Spotify", then the querying has failed, so replace lyrics and genre by NAN

get_lyrics_genre_clean <- function(df_table_from_scraped, time_col) {
  # Function to apply after "get_table_from_scraped"
  # time_col has to be the assosiated week (in datetime format )
  # Combines the data with the information available through the Spotify & Genius API 
  # also performs cleaning (see below)
  
  
  # Combination needs to be called "combination"
  more_data <- data.frame(get_info_tibble(df_table_from_scraped$combination))
  names(more_data) <- c("spotify_artist", "spotify_song", "genius_lyrics", "spotify_genre")
  
  # appending original DF
  df_table_from_scraped$spotify_artists <- more_data$spotify_artist
  df_table_from_scraped$spotify_song <- more_data$spotify_song
  df_table_from_scraped$genius_lyrics <- more_data$genius_lyrics
  df_table_from_scraped$spotify_genre <- more_data$spotify_genre
  
  # Cleaning 
  # IMPORTANT: If spotify_artist = "Spotify", then the querying has failed, so replace lyrics and genre by NAN
  
  df_table_from_scraped$spotify_genre[df_table_from_scraped$spotify_artists == "Spotify"] <- NaN
  df_table_from_scraped$spotify_song[df_table_from_scraped$spotify_artists == "Spotify"] <- NaN
  df_table_from_scraped$genius_lyrics[df_table_from_scraped$spotify_artists == "Spotify"] <- NaN
  df_table_from_scraped$spotify_artists[df_table_from_scraped$spotify_artists == "Spotify"] <- NaN
  
  # adding column for time stamp 
  df_table_from_scraped$time <- time_col
    
    
  return(df_table_from_scraped)
  
}



#######
## 1 ##
#######


last_date <- get_last_date(base_url, x_path_date)


#######
## 2 ## 
#######

vec_all_dates <- seq(as.Date("2010-01-02"), last_date, by="days")

# every day at the end of the week - indicative day for Billboard
vec_7_days <- vec_all_dates[seq(1, length(vec_all_dates), 7)]


## initializing empty dataframe 


data_frame_all <- data.frame(tibble_all <- tibble(
  artist = character(),
  combination = character(),
  spotify_artists = character(),
  spotify_song = character(),
  genius_yrics = character(),
  spotify_genre = character(),
  time = date()
)
)


#### creating URLs and all further steps in one large loop ####


for (i in 1:300 ) { #  length(vec_7_days)) {
  
  # creating url for respective date
  
  print(i)
  
  date_char <- as.character(vec_7_days[i])
  url_final <- paste(base_url, "/", date_char, sep="")
  
  
  #######
  ## 3 ##
  #######
  
  # scraping based on new url 
  scraped_data <- get_correct_text(url_final, x_path_data)
  
  # getting final DF
  df_all <- get_table_from_scraped(scraped_data)
  
  cleaned_df <- get_lyrics_genre_clean(df_all, vec_7_days[i])
  
  data_frame_all <- rbind(data_frame_all, cleaned_df)
  
}



head(data_frame_all,10)

nrow(data_frame_all)

# geil 

### saving results 

#data_frame_example_1 <- data_frame_all

#writexl::write_xlsx(data_frame_example_1, "C:/Users/Nico/Documents/Uni/3. Sem/DS Projekt/Code_and_Data/example_df1.xlsx")





