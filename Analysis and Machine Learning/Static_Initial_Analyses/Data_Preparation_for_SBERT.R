############################################
## Prepping data from Sentence BERT Model ##
############################################

## This file prepares the data to be used in the SBERT Models 
## It shall also compute the Chart Entry Date 
## as this shall be important in the Similarity Analysis

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("pryr")) install.packages("pryr")
if (!require("tm")) install.packages("tm")
if (!require("textdata")) install.packages("textdata")
if (!require("stringi")) install.packages("stringi")


library(tidyverse)
library(tidytext)
library(pryr)
library(tm)
library(textdata)
library(stringi)


df_lyrics <- readr::read_csv("base_data_cleaned.csv")

# getting first date of Chart Appearance 

df_lyrics$first_appearance <- as.Date(NA)

all_combinations <- c()
first_appearance <- c()



for (i in 1:nrow(df_lyrics)) {
  if (df_lyrics$combination[i] %in% all_combinations) {
    #print(paste0(str(i), "already in"))
  }  else {
    all_combinations <- c(all_combinations, df_lyrics$combination[i])
    first_appearance <- c(first_appearance, df_lyrics$dates[i])
    print(paste0(str(i), "added"))
  }
  print(i)
}


df_appearances <- data.frame(all_combinations, first_appearance)


data_for_BERT_all <- dplyr::left_join(df_lyrics[,c("genre", "combination", "lyrics")], df_appearances, by = c("combination" = "all_combinations"))


unique(data_for_BERT$genre)

# only uniques
data_for_BERT <- distinct(data_for_BERT_all)
rm(data_for_BERT_all)



## Using songs with appropriate lyrical length
## changin encoding to utf 8 
# removing non-alpha numeric characters 

data_for_BERT <- data_for_BERT %>%
  filter(is.na(lyrics) == F) %>%
  mutate(len = sapply(strsplit(lyrics, " "), length)) %>%   
  filter(len <= 5000) %>%
  filter(len > 20) %>%
  mutate(lyrics = stri_encode(lyrics , "", "UTF-8")) %>%
  # removing non-alpha numeric characters 
  mutate(lyrics = str_replace_all(lyrics, "[^[:alnum:]]", " ") ) %>%
  select("genre", "combination", "lyrics", "first_appearance")

nrow(data_for_BERT)


rm(df_lyrics)

## saving data locally to be used on colab later 

write.csv(data_for_BERT, "data_for_BERT.csv", row.names = FALSE)


