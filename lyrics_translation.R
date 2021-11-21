##################################################################
##################################################################

#### Part that might need to be modified 
#### when implemented in R instance 

## install packages
# package for language recognition
install.packages("cld3")
library(cld3)

# R wrapper for Deepl API
devtools::install_github("zumbov2/deeplr")

## read in base dataset
data <- readr::read_csv("df_all_billboard_all_weeks_with_genre_lyrics_NOT_CLEANED.csv")

##################################################################
##################################################################

## detect languages of songs 
lang <- detect_language(data$lyrics)

## append to dataframe
data$language <- lang

## extract all songs and lyrics that are not english and have lyrics
lyrics_to_translate <- data %>%
  filter(language != "en") %>%
  filter(is.nan(lyrics))

## remove all duplicate songs
lyrics_to_translate <- lyrics_to_translate[!duplicated(lyrics_to_translate$songs),]

## clean the lyrics a little bit so deepl can work with them
lyrics_to_translate$lyrics <- stringr::str_replace_all(lyrics_to_translate$lyrics, "'\'", "")

## actually translate the lyrics
translated_lyrics <- deeplr::translate2(
  text = lyrics_to_translate$lyrics,
  target_lang = "EN",
  auth_key = key
)

## place the translated lyrics in the dataframe
lyrics_to_translate$lyrics_eng <- translated_lyrics

## to merge the translated lyrics into the old dataframe modify the translated one a little
lyrics_to_translate <- lyrics_to_translate %>%
  dplyr::select(combination, lyrics_eng)

## merge the translated dataset into the old one
data <- left_join(data, lyrics_to_translate,
                  by = "combination")

## fill the new column with only english lyrics with english lyrics
data$lyrics_eng <- ifelse(is.na(data$lyrics_eng), data$lyrics, data$lyrics_eng)















