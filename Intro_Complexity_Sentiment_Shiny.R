#########################################################
## Introdcutory page & Length and complexity of lyrics ##
#########################################################





############################
### Complexity of genres ###
############################


#setwd("C:/Users/Nico/Documents/Uni/3. Sem/DS Projekt/Code_and_Data")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("pryr")) install.packages("pryr")
if (!require("plotly")) install.packages("plotly")
if (!require("tm")) install.packages("tm")
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("SnowballC")) install.packages("SnowballC")
if (!require("textdata")) install.packages("textdata")
#if (!require("dbscan")) install.packages("dbscan")
if (!require("stringi")) install.packages("stringi")
if (!require("lubridate")) install.packages("lubridate")




library(tidyverse)
library(tidytext)
library(pryr)
library(lubridate)
library(plotly)
library(tm)
library(shiny)
library(shinythemes)
library(SnowballC)
library(textdata)
library(stringi)
library(lubridate)




## Einfach nehmnen des neusten "base_data_cleaned"

df_lyrics <- readr::read_csv("base_data_cleaned.csv")


########################################
## length of songs per genre and year w/ & wo/ stopwrods and oomatopoetics ##
## Mean Tempo & Mean Duration // Unique words (before & after stemming ) ####
## Adverbs & unique adverbs 
########################################


df_lengths_genres_dates_1 <- df_lyrics %>%
  filter(is.na(lyrics) == F) %>%
   mutate(len = sapply(strsplit(lyrics, " "), length)) %>%   
  filter(len <= 5000) %>%
  filter(len > 20) %>%
  mutate(lyrics = stri_encode(lyrics , "", "UTF-8")) %>%
  # removing non-alpha numeric characters 
  mutate(lyrics = str_replace_all(lyrics, "[^[:alnum:]]", " ") ) %>%
  select("dates", "genre", "len", "tempo", "duration", "combination", "lyrics", "danceability", "sent")



## onomopoetics dervied from 
#common_words <- df_lyrics[nrow(df_lyrics)-1000:nrow(df_lyrics),] %>%
#     unnest_tokens(output = "word", input = lyrics, token = "words") %>%
#    count(word, sort = T) #%>%
#common_words$word[1:1000]  


onomopoetics <- c("ah","ooh","shimmy","uh","wah","oop","na","nah","bop","whoa","ya","mm","mmh","mmm","mmmh","o","oh","oo","ole","ola","hip","hipp","yo","jo","ee","eeh","da","dah","da-da","dada","ho","hoh","wo","woo", "uh", "du","da", "duh", "dah", "wow", "yeah", "la")

## stopwords from base R
sw <- stop_words$word


## getting adverbs 
adverbs <- tidytext::nma_words$word[tidytext::nma_words$modifier == "adverb"]


## important now - > len refers to overall lyrics length while len1 refers to to reduced length!
xx <- df_lengths_genres_dates_1[, c("lyrics", "combination", "tempo", "duration", "genre", "len", "danceability", "sent")]
df_uniques <- distinct(xx)
rm(xx)

# removing stopwords and onomatopoetics
df_uniques <- df_uniques %>%
  mutate(ly2 =  removeWords(lyrics,sw)) %>%
  mutate(ly =  removeWords(ly2,onomopoetics)) 


for (i in 1:nrow(df_uniques)) {
  
  a <- strsplit(df_uniques$ly[i], " ")
  aa <- strsplit(df_uniques$ly2[i], " ")
  
  # reduced length
  df_uniques$len1[i] <-  length(a[[1]][a[[1]]!= ""]) # reduced length general 
  df_uniques$len2[i] <- length(aa[[1]][aa[[1]]!= ""])  # length when only removing stopwords 
  ## unique words 
  df_uniques$compl[i] <- length(unique(strsplit(df_uniques$lyrics[i], " ")[[1]]))
  df_uniques$compl_stem[i] <- length(unique(wordStem(strsplit(df_uniques$lyrics[i], " ")[[1]], "english")))
  ## adverbs 
  df_uniques$adverbs[i] <- sum(strsplit(df_uniques$lyrics[i], " ")[[1]] %in% adverbs)
  
}


# getting number of onomatopoetics (relative to length)
df_uniques <- df_uniques %>%
  mutate(rel_num_onomatos =  ((len2 - len1) / len) )
    

df_uniques <- df_uniques[, c("lyrics", "ly", "combination", "len", "len1", "duration", "tempo", "danceability", "genre", "compl", "compl_stem", "adverbs", "sent")]
df_lengths_genres_dates_2_1 <- left_join(df_lengths_genres_dates_1[,  c("combination", "dates", "genre")], df_uniques[, c("combination", "len", "len1", "compl", "compl_stem", "duration", "tempo", "danceability", "adverbs", "sent")], by = "combination")


## Sentiment Scores to Binary 
tre_best <- -0.3
df_lengths_genres_dates_2_1$sent_2 <- 0 
df_lengths_genres_dates_2_1$sent_2[df_lengths_genres_dates_2_1$sent > tre_best] <- 1 


df_lengths_genres_dates  <- df_lengths_genres_dates_2_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  mutate(compl = compl / len) %>% # depending on song length 
  mutate(compl_stem = compl_stem / len) %>% # depending on song length 
  mutate(adverbs_rel = adverbs / len) %>%
  summarise(Mean_Song_Length =  ceiling(mean(len)), Mean_Red_Song_Length =  ceiling(mean(len1)), Mean_Tempo = mean(tempo, na.rm = T), Mean_Duration = mean(duration, na.rm = T),   Mean_Compl = mean(compl, na.rm = T),  Mean_Compl_Stem = mean(compl_stem, na.rm = T), Num_Adverbs = mean(adverbs, na.rm = T), Rel_Adverbs = mean(adverbs_rel, na.rm = T), Scores = mean(sent, na.rm = T), Scores_2  = mean(sent_2, na.rm = T) ) %>%
  filter(genre != "unknown genre")
df_lengths_genres_dates$Scores[is.nan(df_lengths_genres_dates$Scores) == T] <- 0



length_duration <-   df_lengths_genres_dates_2_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(Date) %>%
  summarise(Mean_Tempo = mean(tempo, na.rm = T), Mean_Duration = mean(duration, na.rm = T) ) 



########################################
## Development of length and duration ##
########################################

## correlations ## 


corr_table1 <- df_uniques %>%
  filter(genre!= "unknown genre") %>%
  mutate(Rel_Adverbs = (adverbs / len)*100) %>%
  group_by(genre) %>%
  summarise(Corr1 = cor(tempo, len, use = "complete.obs"),  Corr2 = cor(duration, len, use = "complete.obs"), Rel_Adverbs = mean(Rel_Adverbs, na.rm = T), Corr_Sent_Tempo = cor(sent, tempo, use = "complete.obs"), Corr_Sent_Dance = cor(sent, danceability, use = "complete.obs") )

names(corr_table1) <- c("Genre", "Correlation Lyrics Length & BPM", "Correlation Lyrics Length & Duration", "Percentage of adverbs", "Correlation Sentiment & BPM", "Correlation Sentiment & Danceability")




### reducing DFs
df_uniques <- df_uniques[, c("tempo", "duration", "len", "combination", "sent", "danceability")]

#################
## Overview DF ##
#################

df_over <- data.frame(  as.character(min(df_lyrics$dates)),  as.character(max(df_lyrics$dates)),  as.character(nrow(df_lyrics)) , as.character(nrow(  df_lyrics[is.na(df_lyrics$lyrics) == F,] )), as.character(nrow(df_uniques)) )
names(df_over) <- c("First date", "Most recent date", "Number of songs scraped", "Songs with available lyrics", "Unique available songs")


####################
## Sentiment Data ##
####################

df_sents_genre_year  <-  df_lengths_genres_dates_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  summarise(Scores = mean(sent, na.rm = T) ) %>%
  filter(genre != "unknown genre")


df_sents_year  <- df_lengths_genres_dates_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "month")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(Date) %>%
  summarise(Scores = mean(sent, na.rm = T) )




## seasonal sentiments ##

df_seasonal_sent <- df_lengths_genres_dates_2_1 %>%
  mutate(dates = as.character(floor_date(dates, "month"))) %>%
  select(c(combination, dates, sent)) %>%
  mutate(Month = substr(dates,6,7))
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("12","01","02")] <- "Winter"
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("03","04","05")] <- "Spring"
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("06","07","08")] <- "Summer"
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("09","10","11")] <- "Autumn"

df_seasonal_sent_month <- df_seasonal_sent %>%
  group_by(Month) %>%
  summarise(x = mean(sent, na.rm = T))

df_seasonal_sent_season <- df_seasonal_sent %>%
  group_by(Season) %>%
  summarise(x = mean(sent, na.rm = T))


###############################
## DF with historical events ##
###############################

Number <- c(1,2,3,4,5,6,7) # 8
Date <- c('May 1975', 'Jan. 1981 - Jan. 1983', 'July 1988 - Dec. 1991', 'Sep. 2001', 'March 2003', 'Aug. 2007', 'March 2020')  # , 'Sep. 2021'
Event <- c('End of Vietnam War', 'US Economic Crisis', 'Dissolution of the Soviet Union', '9/11', 'Start of second Iraq War', 'Financial Crisis', 'First Wave of Covid Pandemic') # , 'Second Wave of Covid Pandemic'
Events <- c("1)  Vietnam War End (5/'75)", "2)  US Economic Crisis (01/'88-01/'83)", "3) End of Soviet Union (07/'88-12/'91)", "4)  9/11 (09/'01)", "5)  Start Iraq War (03/'03)", "6)  Financial Crisis (08/'07-04/'09)", "7)  First Covid Wave (03/'20)")

df_historical <- data.frame(Number, Date, Event, Events)






##### relevant DFs to be deleted or sent to shiny app 

## delete:
rm(df_lyrics)
rm(df_lengths_genres_dates_1)
rm(df_lengths_genres_dates_2_1)
rm(df_seasonal_sent)

## sent to APP

write.csv(df_over, "/srv/shiny-server/DS_Project/df_over.csv")
write.csv(df_lengths_genres_dates, "/srv/shiny-server/DS_Project/df_lengths_genres_dates.csv")
write.csv(length_duration , "/srv/shiny-server/DS_Project/length_duration.csv")
write.csv(df_uniques , "/srv/shiny-server/DS_Project/df_uniques.csv")
write.csv(df_pos_words , "/srv/shiny-server/DS_Project/df_pos_words.csv")
write.csv(corr_table1 , "/srv/shiny-server/DS_Project/corr_table1.csv")
write.csv(df_seasonal_sent_month , "/srv/shiny-server/DS_Project/df_seasonal_sent_month.csv")
write.csv(df_seasonal_sent_season , "/srv/shiny-server/DS_Project/df_seasonal_sent_season.csv")


print("all well executed")

# for local storage - not for hosting app externally 

#write.csv(df_over, "df_over.csv", row.names = FALSE)
#write.csv(df_lengths_genres_dates, "df_lengths_genres_dates.csv", row.names = FALSE)
#write.csv(length_duration, "length_duration.csv", row.names = FALSE)
#write.csv(df_uniques, "df_uniques.csv", row.names = FALSE)
#write.csv(df_pos_words, "df_pos_words.csv", row.names = FALSE)
#write.csv(corr_table1, "corr_table1.csv", row.names = FALSE)
#write.csv(df_seasonal_sent_month, "df_seasonal_sent_month.csv", row.names = FALSE)
#write.csv(df_seasonal_sent_season, "df_seasonal_sent_season.csv", row.names = FALSE)
#write.csv(df_sents_genre_year, "df_sents_genre_year.csv", row.names = FALSE)
#write.csv(df_sents_year, "df_sents_year.csv", row.names = FALSE)
#write.csv(df_historical, "df_historical.csv", row.names = FALSE)


### From Leo: send to shiny App on server ###
#write.csv(df_over, "/srv/shiny-server/data_science_project/df_over.csv", row.names = FALSE)
#write.csv(df_lengths_genres_dates, "/srv/shiny-server/data_science_project/df_lengths_genres_dates.csv", row.names = FALSE)
#write.csv(length_duration, "/srv/shiny-server/data_science_project/length_duration.csv", row.names = FALSE)
#write.csv(df_uniques, "/srv/shiny-server/data_science_project/df_uniques.csv", row.names = FALSE)
#write.csv(df_pos_words, "/srv/shiny-server/data_science_project/df_pos_words.csv", row.names = FALSE)
#write.csv(corr_table1, "/srv/shiny-server/data_science_project/corr_table1.csv", row.names = FALSE)
#write.csv(df_seasonal_sent_month, "/srv/shiny-server/data_science_project/df_seasonal_sent_month.csv", row.names = FALSE)
#write.csv(df_seasonal_sent_season, "/srv/shiny-server/data_science_project/df_seasonal_sent_season.csv", row.names = FALSE)
#write.csv(df_sents_genre_year, "/srv/shiny-server/data_science_project/df_sents_genre_year.csv", row.names = FALSE)
#write.csv(df_sents_year, "/srv/shiny-server/data_science_project/df_sents_year.csv", row.names = FALSE)
#write.csv(df_historical, "/srv/shiny-server/data_science_project/df_historical.csv", row.names = FALSE)




