#############################
## Sentiment Analysis Prep ##
#############################

## This file prepares the results of the Sentiment Analysis 
## for the display in the App!

library(tidyverse)




# merging with Base Data Cleaned

base_data_cleaned <- read_csv("base_data_cleaned.csv")
assigned_sentiments <- read_csv("data_BERT_Sent.csv")

base_sents <- dplyr::inner_join(base_data_cleaned, assigned_sentiments[, c("combination", "sent_per_pos") ], by = "combination")
base_sents$sentiment <- base_sents$sent_per_pos

df_sentiments <- base_sents[, c("dates", "combination", "genre", "danceability", "tempo", "sentiment")]

# saving base data 
write.csv(df_sentiments, "df_sentiments.csv")


##################################
## Creating relevant DataFrames ##
##################################


## development ##

df_sents_genre_year  <-  df_sentiments %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "year")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(genre, Date) %>%
  summarise(Scores = mean(sentiment, na.rm = T) ) %>%
  filter(genre != "unknown genre")


df_sents_year  <- df_sentiments %>%
  ### function for length of words 
  mutate(dates = floor_date(dates, "month")) %>% ## function for aggregating at year level 
  mutate(Date = dates) %>%
  group_by(Date) %>%
  summarise(Scores = mean(sentiment, na.rm = T) )



## correlations 

df_uniques_1 <- df_sentiments[,c("danceability", "tempo", "sentiment", "genre")]  # only uniques
df_uniques_sent <- distinct(df_uniques_1)


corr_table2 <- df_uniques_sent %>%
  filter(genre!= "unknown genre") %>%
  group_by(genre) %>%
  summarise(Corr_Sent_Tempo = cor(sentiment, tempo, use = "complete.obs"), Corr_Sent_Dance = cor(sentiment, danceability, use = "complete.obs") )

names(corr_table2) <- c("Genre", "Correlation Sentiment & BPM", "Correlation Sentiment & Danceability")



## seasonal sentiments ##

df_seasonal_sent <- df_sentiments %>%
  mutate(dates = as.character(floor_date(dates, "month"))) %>%
  select(c(combination, dates, sentiment)) %>%
  mutate(Month = substr(dates,6,7))

df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("12","01","02")] <- "Winter"
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("03","04","05")] <- "Spring"
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("06","07","08")] <- "Summer"
df_seasonal_sent$Season[df_seasonal_sent$Month %in% c("09","10","11")] <- "Autumn"

df_seasonal_sent_month <- df_seasonal_sent %>%
  group_by(Month) %>%
  summarise(x = mean(sentiment, na.rm = T) )

df_seasonal_sent_season <- df_seasonal_sent %>%
  group_by(Season) %>%
  summarise(x = mean(sentiment, na.rm = T))



###############################
## DF with historical events ##
###############################

Number <- c(1,2,3,4,5,6,7) # 8
Date <- c('May 1975', 'Jan. 1981 - Jan. 1983', 'July 1988 - Dec. 1991', 'Sep. 2001', 'March 2003', 'Aug. 2007', 'March 2020')  # , 'Sep. 2021'
Event <- c('End of Vietnam War', 'US Economic Crisis', 'Dissolution of the Soviet Union', '9/11', 'Start of second Iraq War', 'Financial Crisis', 'First Wave of Covid Pandemic') # , 'Second Wave of Covid Pandemic'
Events <- c("1)  Vietnam War End (5/'75)", "2)  US Economic Crisis (01/'88-01/'83)", "3) End of Soviet Union (07/'88-12/'91)", "4)  9/11 (09/'01)", "5)  Start Iraq War (03/'03)", "6)  Financial Crisis (08/'07-04/'09)", "7)  First Covid Wave (03/'20)")

df_historical <- data.frame(Number, Date, Event, Events)




## Saving all 

write.csv(df_sents_genre_year, "/srv/shiny-server/DS_Project/df_sents_genre_year.csv", row.names = FALSE)
write.csv(df_sents_year, "/srv/shiny-server/DS_Project/df_sents_year.csv", row.names = FALSE)
write.csv(df_historical, "/srv/shiny-server/DS_Project/df_historical.csv", row.names = FALSE)
write.csv(df_seasonal_sent_month , "/srv/shiny-server/DS_Project/df_seasonal_sent_month.csv", row.names = FALSE)
write.csv(df_seasonal_sent_season , "/srv/shiny-server/DS_Project/df_seasonal_sent_season.csv", row.names = FALSE)
write.csv(corr_table2 , "/srv/shiny-server/DS_Project/corr_table2.csv", row.names = FALSE)
write.csv(df_uniques_sent , "/srv/shiny-server/DS_Project/df_uniques_sent.csv", row.names = FALSE)



write.csv(df_seasonal_sent_month, "df_seasonal_sent_month.csv", row.names = FALSE)
write.csv(df_seasonal_sent_season, "df_seasonal_sent_season.csv", row.names = FALSE)
write.csv(df_sents_genre_year, "df_sents_genre_year.csv", row.names = FALSE)
write.csv(df_sents_year, "df_sents_year.csv", row.names = FALSE)
write.csv(df_historical, "df_historical.csv", row.names = FALSE)
write.csv(corr_table2 , "corr_table2.csv", row.names = FALSE)
write.csv(df_uniques_sent , "df_uniques_sent.csv", row.names = FALSE)






  
