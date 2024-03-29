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
  filter(len <= 2000) %>%  ## removing outliers
  filter(len > 20) %>%
  mutate(lyrics = stri_encode(lyrics , "", "UTF-8")) %>%
  # removing non-alpha numeric characters 
  mutate(lyrics = str_replace_all(lyrics, "[^[:alnum:]]", " ") ) %>%
  select("dates", "genre", "len", "tempo", "duration", "combination", "lyrics", "danceability")



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
xx <- df_lengths_genres_dates_1[, c("lyrics", "combination", "tempo", "duration", "genre", "len", "danceability")]
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
    

df_uniques <- df_uniques[, c("lyrics", "ly", "combination", "len", "len1", "duration", "tempo", "danceability", "genre", "compl", "compl_stem", "adverbs")]
df_lengths_genres_dates_2_1 <- left_join(df_lengths_genres_dates_1[,  c("combination", "dates", "genre")], df_uniques[, c("combination", "len", "len1", "compl", "compl_stem", "duration", "tempo", "danceability", "adverbs")], by = "combination")




df_lengths_genres_dates  <- df_lengths_genres_dates_2_1 %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  mutate(compl = compl / len) %>% # depending on song length 
  mutate(compl_stem = compl_stem / len) %>% # depending on song length 
  mutate(adverbs_rel = adverbs / len) %>%
  summarise(Mean_Song_Length =  ceiling(mean(len)), Mean_Red_Song_Length =  ceiling(mean(len1)), Mean_Tempo = mean(tempo, na.rm = T), Mean_Duration = mean(duration, na.rm = T),   Mean_Compl = mean(compl, na.rm = T),  Mean_Compl_Stem = mean(compl_stem, na.rm = T), Num_Adverbs = mean(adverbs, na.rm = T), Rel_Adverbs = mean(adverbs_rel, na.rm = T) ) %>%
  filter(genre != "unknown genre")



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
  summarise(Corr1 = cor(tempo, len, use = "complete.obs"),  Corr2 = cor(duration, len, use = "complete.obs"), Rel_Adverbs = mean(Rel_Adverbs, na.rm = T) )

names(corr_table1) <- c("Genre", "Correlation Lyrics Length & BPM", "Correlation Lyrics Length & Duration", "Percentage of adverbs")




### reducing DFs
df_uniques <- df_uniques[, c("tempo", "duration", "len", "combination", "danceability")]

#################
## Overview DF ##
#################

df_over <- data.frame(  as.character(min(df_lyrics$dates)),  as.character(max(df_lyrics$dates)),  as.character(nrow(df_lyrics)) , as.character(nrow(  df_lyrics[is.na(df_lyrics$lyrics) == F,] )), as.character(nrow(df_uniques)) )
names(df_over) <- c("First date", "Most recent date", "Number of songs scraped", "Songs with available lyrics", "Unique available songs")





##### relevant DFs to be deleted or sent to shiny app 

## delete:
rm(df_lyrics)
rm(df_lengths_genres_dates_1)
rm(df_lengths_genres_dates_2_1)

## sent to APP

write.csv(df_over, "/srv/shiny-server/DS_Project/df_over.csv", row.names = FALSE)
write.csv(df_lengths_genres_dates, "/srv/shiny-server/DS_Project/df_lengths_genres_dates.csv", row.names = FALSE)
write.csv(length_duration , "/srv/shiny-server/DS_Project/length_duration.csv", row.names = FALSE)
write.csv(df_uniques , "/srv/shiny-server/DS_Project/df_uniques.csv", row.names = FALSE)
write.csv(corr_table1 , "/srv/shiny-server/DS_Project/corr_table1.csv", row.names = FALSE)



# for local storage - not for hosting app externally 

write.csv(df_over, "df_over.csv", row.names = FALSE)
write.csv(df_lengths_genres_dates, "df_lengths_genres_dates.csv", row.names = FALSE)
write.csv(length_duration, "length_duration.csv", row.names = FALSE)
write.csv(df_uniques, "df_uniques.csv", row.names = FALSE)
#write.csv(df_pos_words, "df_pos_words.csv", row.names = FALSE)
write.csv(corr_table1, "corr_table1.csv", row.names = FALSE)




print("all well executed")

