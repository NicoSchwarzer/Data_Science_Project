#### NER Songs First Steps ####

## install and load packages
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidytext")) install.packages("tidytext")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")

library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)


# read in data
music_data <- readr::read_csv("C:/Users/leona/Documents/Data_Science_Project/Data/billboardHot100_1999-2019.csv")
glimpse(music_data)

# extract year/month/day of appearance in the charts
date_split <- stringr::str_split_fixed(music_data$Week, "-", 3)

music_data$Year <- as.numeric(date_split[,1])
music_data$Month <- as.numeric(date_split[,2])
music_data$Day <- as.numeric(date_split[,3])
rm(date_split)

# work on subset for now (only year 2000)
data <- filter(music_data, Year == 2000)

# only keep lyrics and week of chart appearance for now
data_whole <- select(music_data, c("Lyrics", "Week"))
data <- select(data, c("Lyrics", "Week"))

# collapse all song texts for every week (subset)
week_lyrics <- data %>%
  group_by(Week) %>%
  summarize(text = toString(Lyrics))

# collapse all song texts for every week (whole)
week_lyrics_whole <- data_whole %>%
  group_by(Week) %>%
  summarize(text = toString(Lyrics))





### SHORT WRAP-UP ###
# week_lyrics now contains all lyrics of all songs as one "big" song-text given the week of
# chart appearance

# clean the texts (Zeilenumbrueche)
week_lyrics$text <- stringr::str_replace_all(week_lyrics$text, "\n", " ")

# tokenize the lyrics for each week
text_token <- tidytext::unnest_tokens(week_lyrics, word, text)

# remove stop words, list of stop words given by tidytext package
text_token <- text_token %>%
  anti_join(tidytext::stop_words, by = "word")


##############################################
#### Check for appearance of single words ####
##############################################

# count words
text_token_c <- text_token %>%
  group_by(Week) %>%
  count(word, sort = TRUE)


# exemplary plot bar chart for a week
text_token_c %>%
  filter(Week == "2000-01-03") %>%
  filter(n > 50) %>%
  ggplot(aes(x = n, y = reorder(word, n))) +
  geom_col()


##############################################
##### Check for bigrams (words together) #####
##############################################

# get bigrams for all lyrics and filter out the ones with stop words
bi_grams <- week_lyrics %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  tidyr::separate(bigram, c("Word1", "Word2"), sep = " ") %>%
  filter(!Word1 %in% stop_words$word) %>%
  filter(!Word2 %in% stop_words$word) %>%
  tidyr::unite(word, Word1, Word2, sep = " ")


# count the bigrams for a week
bi_grams_count <- bi_grams %>%
  group_by(Week) %>%
  count(word, sort = TRUE)


# plot bigrams for a week
bi_grams_count %>%
  filter(Week == "2000-01-03") %>%
  filter(n > 25) %>%
  ggplot(aes(x = n, y = reorder(word, n))) +
  geom_col()



##############################################
##### Topic identification #####
##############################################
# now applied to the whole (kaggle) dataset from 1999 - 2016

# get length of all the lyrics that week
song_len <- str_count(week_lyrics_whole$text, "\\w+")

# get number of words related to topics
love_count <- str_count(week_lyrics_whole$text, c("love|girlfriend|boyfriend"))
anger_count <- str_count(week_lyrics_whole$text, c("hate|anger|trouble|sad"))
wanderlust_count <- str_count(week_lyrics_whole$text, c("travel|adventure|nature|excited"))
money_count <- str_count(week_lyrics_whole$text, c("money|fame|rich"))
christmas_count <- str_count(week_lyrics_whole$text, c("christmas|presents|santa"))
happy_count <-  str_count(week_lyrics_whole$text, c("joy|happy|good"))

# make data frame
topic_df <- data.frame("love" = love_count,
                       "anger" = anger_count,
                       "wanderlust" = wanderlust_count,
                       "money" = money_count,
                       "christmas" = christmas_count,
                       "happy" = happy_count)

# get relative scores
topic_df <- as.data.frame(apply(topic_df, 2, "/", song_len))
# topic_df <- as.data.frame(apply(topic_df, 2, log))   ## log scale??

# add date variable
topic_df$date <- week_lyrics_whole$Week

# make long format
topic_df_long <- gather(topic_df, love, anger, wanderlust, money, christmas, happy, key = "topic", value = "appearance")


#write.csv(topic_df_long,
#          "C:/Users/leona/Documents/Data_Science_Project/Data/topic_df.csv")



# plot topics over time
topics_displayed <- c("love", "anger", "wanderlust", "money", "christmas", "happy")
topic_df_long %>%
  filter(topic %in% topics_displayed) %>%
  ggplot(aes(x = date, y = appearance)) + 
  geom_line(aes(color = topic)) +
  geom_smooth(method = "lm", aes(color = topic), alpha = 0.2, size = 0.1)





## next steps:
# modify list of stop words
# n-grams
# relative appearance over time