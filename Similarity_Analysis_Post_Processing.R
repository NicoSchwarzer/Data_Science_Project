#####################################
## Preparing the Lyrics for display #
#####################################


## This file prepares the lyrics that shall be displayed in the lyrical recommendation engine

library(tidyverse)
library(readr)


## lastly saving the df_lyrics as a .dat 

lyrics_df = read_csv("df_lyrics.csv")[, c("lyrics")]

glimpse(lyrics_df)

lyrics_df$lyrics <- gsub(' s ', "'s ", lyrics_df$lyrics)
lyrics_df$lyrics <- gsub(' ll ', "'ll ", lyrics_df$lyrics)
lyrics_df$lyrics <- gsub(' ve ', "'ve ", lyrics_df$lyrics)
lyrics_df$lyrics <- gsub(' m ', "'m ", lyrics_df$lyrics)



write.table(lyrics_df, "lyrics_df.dat", row.names = FALSE)
write.table(lyrics_df, "/srv/shiny-server/DS_Project/lyrics_df.dat", row.names = FALSE)

combinations_df <- read_csv("df_lyrics.csv")[, c("combination")]

write.table(combinations_df, "combinations_df.dat", row.names = FALSE)
write.table(combinations_df, "/srv/shiny-server/DS_Project/combinations_df.dat", row.names = FALSE)


rm(df_lyrics)
rm(similarity_topics_df)
rm(prep_genre)
rm(df_lengths_genres_dates_x)
