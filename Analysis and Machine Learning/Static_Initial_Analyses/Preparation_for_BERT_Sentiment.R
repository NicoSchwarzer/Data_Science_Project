##############################################################
## Scraping Lyrics for alternative Sentiment Classification ##
##############################################################

source("functions_for_updating.R")

moody_df <- read_csv("MoodyLyrics.csv")
glimpse(moody_df)


glimpse(base_data_cleaned)
glimpse(moody_df)



## getting lyrics 

moody_df$lyrics <- "na"

## if combination already present . just taking these data 


for (i in 1:nrow(moody_df)){
  
  # make artist vector
  artist_vector <- get_strcombinations(moody_df$artist[i])
  
  
  # make song vector
  song_vector <- get_strcombinations(moody_df$title[i])
  
  # get lyrics
  l <-  get_lyrics_alt(artist_vector = artist_vector,
                       song_vector = song_vector,
                       repeats = 5, print_comb = FALSE)
  
  
  
  # fill in lyrics
  moody_df$lyrics[i] <- l
  print(i)
  print(l)
  
}

## only keeping entries where lyrics are present 


moody_df_lyrics <- moody_df[moody_df$lyrics != "na",]

nrow(moody_df_lyrics) #  4998 (!!!)

# getting combination
moody_df_lyrics$combination <- paste0(moody_df_lyrics$artist,"  ", moody_df_lyrics$title)

# saving 

write.csv(moody_df_lyrics, "moody_df_lyrics.csv")


