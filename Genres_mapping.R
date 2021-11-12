##############################
## Topic Mapping for Lyrics ##
##############################

## Setting WD 
setwd("C:\\Users\\Nico\\Documents\\Uni\\3. Sem\\DS Projekt\\Code_and_Data")

## please make sure that the excel file df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED.csv resides here!

## This file can apply the re-mapping of genres of the df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED Dataframe 
## in case that the excel file with the assosiation is updated.

## Then, the csv-file df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED is updated,
## the weekly updating need not be changed 


if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)


## function to map the many genres to overarching genres

matching_genres <- function(df) {
  
  # function needs a df with column "genre" as input. 
  # Also the matching excel file must be read in 
  
  genres_mapping <- readxl::read_excel("unique_genres.xlsx")
  genres_mapping <- genres_mapping[,c("original_genre", "new_genre")]
  
  for (i in 1:nrow(df)) {
    
    genre_here <- df$genre[i]
    
    # in case the genre is unknown
    if ( length(genre_here) == 0 ) {
      df$genre[i] <- "unknown genre"
      
    # in all other cases:     
    } else {
      matched_genre <- genres_mapping$new_genre[i]
      df$genre[i] <- matched_genre
      
    }
  }
  
  return(df)
}


## applying to DF ## 

# reading in 
df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED <- read.csv("df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED.csv")

# applying function 
df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED <- matching_genres(df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED)

# saving (overwriting old df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED)
write.csv(df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED, "df_all_billboard_all_weeks_with_reduced_genre_lyrics_NOT_CLEANED.csv")

