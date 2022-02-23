
#### Quiz Lyrics Data - Updating ####

# load packages
library(dplyr)

# extract lyrics
lyrics <- base_data_cleaned %>%
  select(songs, artists, lyrics) %>%
  distinct()

dplyr::glimpse(base_data_cleaned)

# get best chart rank and first appearance
quiz_lyrics_data <- base_data_cleaned %>%
  select(dates, chart_rank, artists, songs) %>%
  group_by(artists, songs) %>%
  summarise(first_appearance = min(dates),
            best_rank = min(chart_rank))

# join together 
quiz_lyrics_data <- left_join(quiz_lyrics_data, lyrics,
                              by = c("songs" = "songs", "artists" = "artists"))


# clean
quiz_lyrics_data <- quiz_lyrics_data %>%
  na.omit()

# clean lyrics
quiz_lyrics_data$lyrics <- stringr::str_replace_all(quiz_lyrics_data$lyrics, "\n", " ")
quiz_lyrics_data$lyrics <- stringr::str_replace_all(quiz_lyrics_data$lyrics, "\"", " ")

# get only plausible song texts
quiz_lyrics_data <- quiz_lyrics_data %>%
  filter(nchar(lyrics) > 150 & nchar(lyrics) < 20000)



# store locally
write.csv(quiz_lyrics_data, "quiz_lyrics_data.csv", row.names = FALSE)

# sent to Shiny App
write.csv(quiz_lyrics_data, "/srv/shiny-server/DS_Project/quiz_lyrics_data.csv", row.names = FALSE)


try(rm(list = c("lyrics", "quiz_lyrics_data")))




