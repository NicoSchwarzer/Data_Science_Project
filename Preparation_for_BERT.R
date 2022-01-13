############################################
## Prepping data from Sentence BERT Model ##
############################################

setwd("C:/Users/Nico/Documents/Uni/3. Sem/DS Projekt/Code_and_Data")

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


## Einfach nehmnen des neusten "base_data_cleaned"

df_lyrics <- readr::read_csv("base_data_cleaned.csv")


# getting first date of Chart Appearance 

df_lyrics$first_appearance <- as.Date(NA)

all_combinations <- c("First")
first_appearance <- c(as.Date("2000-01-01"))



for (i in 1:2000) {   ## nrow(df_lyrics)
  if (df_lyrics$combination[i] %in% all_combinations) {
    #print(paste0(str(i), "already in"))
  }  else {
    all_combinations <- c(all_combinations, df_lyrics$combination[i])
    first_appearance <- c(first_appearance, df_lyrics$dates[i])
    print(paste0(str(i), "added"))
  }
}
  

# only uniques
data_for_BERT_all <- df_lyrics[,c("genre", "combination", "lyrics")]
data_for_BERT <- distinct(data_for_BERT_all)


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
  select("genre", "combination", "lyrics")


rm(df_lyrics)

## saving data locally to be used on colab later 


write.csv(data_for_BERT, "data_for_BERT.csv", row.names = FALSE)

