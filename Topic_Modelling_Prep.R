###################################
## Preparing the topic Modelling ##
###################################


## This file prepares the DFs which shall bes used for the Topic Modelling Page

library(tidyverse)
library(readr)



## reading in DFs and merging


df_lyrics <- readr::read_csv("base_data_cleaned.csv")
similarity_topics_df <- readr::read_csv("similarity_topics_df.csv")

df_lyrics <- dplyr::left_join(df_lyrics, similarity_topics_df[,c("combination", "topics", "topics_2")], by = "combination")


## Analysis ##



df_lengths_genres_dates_x <- df_lyrics %>%
  select("dates", "genre", "combination", "sent", "topics", "topics_2") %>%
  filter(is.na(topics) == F )

df_lengths_genres_dates_x$topics <- as.factor(df_lengths_genres_dates_x$topics)
df_lengths_genres_dates_x$topics_2 <- as.factor(df_lengths_genres_dates_x$topics_2)

unique(df_lengths_genres_dates_x$topics)


## by genre and year

df_topics_genre_year  <- df_lengths_genres_dates_1 %>%
  mutate(dates = floor_date(dates, "year")) %>%
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  summarize(pct.Sadness = mean(topics == "Sadness and Critique", na.rm = T), pct.Love = mean(topics == "Love and Romance", na.rm = T),  pct.Motivation = mean(topics == "Motivation and Ambitions", na.rm = T),  pct.Affluence = mean(topics == "Affluence and Fame", na.rm = T),  pct.Party = mean(topics == "Feelgood, friends and Party", na.rm = T))  #%>%
# just for checking
  #summarize(all = pct.Sadness + pct.Love + pct.Motivation + pct.Affluence + pct.Party)




names(df_topics_genre_year) <- c("genre", "Date", "Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")


df_topics_genre_year <- df_topics_genre_year %>%
  filter(genre != "unknown genre")


## by date

df_topics_year  <- df_lengths_genres_dates_1 %>%
     mutate(dates = floor_date(dates, "year")) %>%
     mutate(Date = dates) %>%
     group_by(Date) %>%
     summarize(pct.Sadness = mean(topics == "Sadness and Critique", na.rm = T), pct.Love = mean(topics == "Love and Romance", na.rm = T),  pct.Motivation = mean(topics == "Motivation and Ambitions", na.rm = T),  pct.Affluence = mean(topics == "Affluence and Fame", na.rm = T),  pct.Party = mean(topics == "Feelgood, friends and Party", na.rm = T))


names(df_topics_year) <- c("Date", "Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")


 ## Per Genre ( 2 topics )


prep_genre  <- df_lengths_genres_dates_1 %>%
  group_by(genre) %>%
  summarize(pct.Sadness = mean(topics == "Sadness and Critique", na.rm = T), pct.Love = mean(topics == "Love and Romance", na.rm = T),  pct.Motivation = mean(topics == "Motivation and Ambitions", na.rm = T),  pct.Affluence = mean(topics == "Affluence and Fame", na.rm = T),  pct.Party = mean(topics == "Feelgood, friends and Party", na.rm = T)) %>%
  filter(genre != "unknown genre")

names(prep_genre) <- c("genre", "Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")

# hierauf basierend reactive pie chart!


### co-occuring topics !!

co_occuring_df <- df_lengths_genres_dates_1 %>%
  group_by(topics) %>%
  summarize(pct.Sadness = mean(topics_2 == "Sadness and Critique", na.rm = T), pct.Love = mean(topics_2 == "Love and Romance", na.rm = T),  pct.Motivation = mean(topics_2 == "Motivation and Ambitions", na.rm = T),  pct.Affluence = mean(topics_2 == "Affluence and Fame", na.rm = T),  pct.Party = mean(topics_2 == "Feelgood, friends and Party", na.rm = T)) %>%
  filter(is.na(topics) == F)

names(co_occuring_df) <- c("genre", "Sadness and Critique", "Love and Romance" , "Motivation and Ambitions" , "Affluence and Fame" ,  "Feelgood, friends and Party")



## relationship with Sentiment!

topics_sent <- df_lengths_genres_dates_1 %>%
  mutate(Topics = topics) %>%
  group_by(Topics) %>%
  summarise('Mean Sentiment Score' = mean(sent, na.rm = T)) %>%
  filter(is.na(Topics) == F)



## saving and sending to Shiny Server



write.csv(df_topics_genre_year , "df_topics_genre_year.csv", row.names = FALSE)
write.csv(df_topics_year , "df_topics_year.csv", row.names = FALSE)
write.csv(prep_genre , "prep_genre.csv", row.names = FALSE)
write.csv(co_occuring_df, "co_occuring_df.csv", row.names =  FALSE)
write.csv(topics_sent, "topics_sent.csv", row.names =  FALSE)

## to Server


write.csv(df_topics_genre_year , "/srv/shiny-server/DS_Project/df_topics_genre_year.csv", row.names = FALSE)
write.csv(df_topics_year , "/srv/shiny-server/DS_Project/df_topics_year.csv", row.names = FALSE)
write.csv(prep_genre , "/srv/shiny-server/DS_Project/prep_genre.csv", row.names = FALSE)
write.csv(co_occuring_df, "/srv/shiny-server/DS_Project/co_occuring_df.csv", row.names =  FALSE)
write.csv(topics_sent, "/srv/shiny-server/DS_Project/topics_sent.csv", row.names =  FALSE)






